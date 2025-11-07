/**
 * Testes E2E que verificam o fluxo completo do chatbot:
 * carregamento, intenções principais e respostas esperadas para o dataset.
 */
describe('Fluxo Completo do Chatbot Sakila-Prolog', () => {

  beforeEach(() => {
    cy.visit('/');
    cy.intercept('GET', '/filmes-por-ator/*').as('filmesPorAtor');
    cy.intercept('GET', '/genero-do-filme/*').as('generoDoFilme');
    cy.intercept('GET', '/filmes-por-genero/*').as('filmesPorGenero');
    cy.intercept('GET', '/contar-filmes*').as('contarFilmes');
    cy.intercept('GET', '/recomendar/ator-e-genero*').as('atorEGenero');
    cy.intercept('GET', '/recomendar/dois-generos*').as('doisGeneros');
    cy.intercept('GET', '/filmes-por-diretor/*').as('filmesPorDiretor');
  });

  it('Deve carregar e exibir a mensagem inicial do Bot', () => {
    // Verifica a mensagem de ajuda inicial renderizada pelo HTML
    cy.get('#chat-log .bot-message.help-text').first().should('contain.text', 'Olá! Sou o Chatbot Sakila');
  });

  it('Deve testar a intenção "filmes por ator" (E2E)', () => {
    // 1. Digitar com erro (fuzzy) e submeter via Enter (dataset Netflix)
    cy.get('#user-input').type('filmes por ator adam sandlr {enter}');
    // 2. Verifica se a mensagem do utilizador foi adicionada
    cy.get('#chat-log .user-message').last().should('contain.text', 'filmes por ator adam sandlr');
    // 3. Aguarda resposta do endpoint para garantir rendering
    cy.wait('@filmesPorAtor', { timeout: 10000 });
    // 4. Conteúdo esperado (dataset Netflix): deve listar pelo menos um filme do Adam Sandler
    cy.get('#chat-log .bot-message').last().should(($el) => {
      const txt = $el.text();
      expect(/Grown Ups|Hubie Halloween|Murder Mystery|Uncut Gems|100% FRESH/i.test(txt)).to.be.true;
    });
  });

  it('Deve testar a intenção "contar filmes" (E2E)', () => {
    cy.get('#user-input').type('contar filmes de action em 2006{enter}');
    cy.wait('@contarFilmes', { timeout: 10000 });
    // Verifica última resposta do bot com o formato de contagem
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Contagem (TV ACTION & ADVENTURE, 2006): 1');
  });

  it('Deve testar a intenção "gênero do filme" (E2E)', () => {
    cy.get('#user-input').type('genero do uncut gems{enter}');
    cy.wait('@generoDoFilme', { timeout: 10000 });
    // Deve retornar um dos géneros conhecidos do filme (DRAMA ou THRILLER)
    cy.get('#chat-log .bot-message').last().should(($el) => {
      const txt = $el.text();
      expect(/DRAMA|THRILLER/.test(txt)).to.be.true;
    });
  });

  it('Deve testar a intenção "filmes por genero" (E2E)', () => {
    cy.get('#user-input').type('filmes de acao{enter}');
    cy.wait('@filmesPorGenero', { timeout: 10000 });
    // 1. Mensagem do utilizador deve aparecer
    cy.get('#chat-log .user-message').last().should('contain.text', 'filmes de acao');
    // 2. Resposta do bot deve listar pelo menos um filme (conteúdo não vazio)
    cy.get('#chat-log .bot-message').last().invoke('text').should('match', /\w+/);
  });

  it('Deve testar a intenção "recomendar por ator" (E2E)', () => {
    cy.get('#user-input').type('recomendar do ator adam sandlr{enter}');
    cy.wait('@filmesPorAtor', { timeout: 10000 });
    // 1. Mensagem do utilizador deve aparecer
    cy.get('#chat-log .user-message').last().should('contain.text', 'recomendar do ator adam sandlr');
    // 2. Resposta do bot deve listar recomendações (filmes do Adam Sandler)
    cy.get('#chat-log .bot-message').last().should(($el) => {
      const txt = $el.text();
      expect(/Grown Ups|Hubie Halloween|Murder Mystery|Uncut Gems|100% FRESH/i.test(txt)).to.be.true;
    });
  });

  it('Deve mostrar uma mensagem de "Não entendi" para inputs inválidos', () => {
    cy.get('#user-input').type('qual a previsão do tempo?{enter}');
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Não entendi');
  });

  // (8º) Intenção composta: ator e gênero
  it('Deve testar a intenção composta "ator e genero" (E2E)', () => {
    const query = 'filme de drama com ator adam sandlr';

    cy.get('#user-input').type(`${query}{enter}`);
    cy.wait('@atorEGenero', { timeout: 10000 });

    // Verifica se a query do utilizador apareceu
    cy.get('#chat-log .user-message').last().should('contain.text', query);

    // Verifica se o bot respondeu com o filme esperado (ajustado ao dataset)
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Uncut Gems');
  });

  // (9º) Intenção composta: dois gêneros
  it('Deve testar a intenção composta "dois generos" (E2E)', () => {
    const query = 'filme de drama e triler';

    cy.get('#user-input').type(`${query}{enter}`);
    cy.wait('@doisGeneros', { timeout: 10000 });

    // Verifica se a query do utilizador apareceu
    cy.get('#chat-log .user-message').last().should('contain.text', query);

    // Verifica se o bot respondeu com o filme esperado (ajustado ao dataset)
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Uncut Gems');
  });

  it('Deve testar a intenção "filmes por diretor" (E2E)', () => {
    const query = 'filmes do diretor quentin tarantino';

    cy.get('#user-input').type(`${query}{enter}`);
    cy.wait('@filmesPorDiretor', { timeout: 10000 });

    // Verifica se a query do utilizador apareceu
    cy.get('#chat-log .user-message').last().should('contain.text', query);

    // Verifica se o bot respondeu listando pelo menos um filme do Tarantino
    cy.get('#chat-log .bot-message').last().should(($el) => {
      const txt = $el.text();
      expect(/Pulp Fiction|Django Unchained|Inglourious Basterds|Jackie Brown|Kill Bill: Vol\. 1|Kill Bill: Vol\. 2|The Hateful Eight/i.test(txt)).to.be.true;
    });
  });
});