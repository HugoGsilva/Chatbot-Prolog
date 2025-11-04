// cypress/e2e/chatbot_flow.cy.js
describe('Fluxo Completo do Chatbot Sakila-Prolog', () => {

  beforeEach(() => {
    // Visita a página antes de cada teste
    cy.visit('/');
    // Aguarda pelos endpoints relevantes
    cy.intercept('GET', '/filmes-por-ator/*').as('filmesPorAtor');
    cy.intercept('GET', '/genero-do-filme/*').as('generoDoFilme');
    cy.intercept('GET', '/filmes-por-genero/*').as('filmesPorGenero');
    cy.intercept('GET', '/contar-filmes*').as('contarFilmes');
    cy.intercept('GET', '/recomendar/ator-e-genero*').as('atorEGenero');
    cy.intercept('GET', '/recomendar/dois-generos*').as('doisGeneros');
  });

  it('Deve carregar e exibir a mensagem inicial do Bot', () => {
    // Verifica a mensagem de ajuda inicial renderizada pelo HTML
    cy.get('#chat-log .bot-message.help-text').first().should('contain.text', 'Olá! Sou o Chatbot Sakila');
  });

  it('Deve testar a intenção "filmes por ator" (E2E)', () => {
    // 1. Digitar com erro (fuzzy) e submeter via Enter (dataset V2)
    cy.get('#user-input').type('filmes por actor a{enter}');
    // 2. Verifica se a mensagem do utilizador foi adicionada
    cy.get('#chat-log .user-message').last().should('contain.text', 'filmes por actor a');
    // 3. Aguarda resposta do endpoint para garantir rendering
    cy.wait('@filmesPorAtor', { timeout: 10000 });
    // 4. Conteúdo esperado (dataset V2): deve listar "Sample Film"
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Sample Film');
  });

  it('Deve testar a intenção "contar filmes" (E2E)', () => {
    cy.get('#user-input').type('contar filmes de action em 2006{enter}');
    cy.wait('@contarFilmes', { timeout: 10000 });
    // Verifica última resposta do bot com o formato de contagem
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Contagem (ACTION, 2006):');
  });

  it('Deve testar a intenção "gênero do filme" (E2E)', () => {
    cy.get('#user-input').type('genero do sample film{enter}');
    cy.wait('@generoDoFilme', { timeout: 10000 });
    // Deve retornar um dos géneros conhecidos do filme (COMEDY ou DRAMA)
    cy.get('#chat-log .bot-message').last().should(($el) => {
      const txt = $el.text();
      expect(/COMEDY|DRAMA/.test(txt)).to.be.true;
    });
  });

  it('Deve testar a intenção "filmes por genero" (E2E)', () => {
    cy.get('#user-input').type('filmes de acao{enter}');
    cy.wait('@filmesPorGenero', { timeout: 10000 });
    // 1. Mensagem do utilizador deve aparecer
    cy.get('#chat-log .user-message').last().should('contain.text', 'filmes de acao');
    // 2. Resposta do bot deve listar pelo menos um filme
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Another Title');
  });

  it('Deve testar a intenção "recomendar por ator" (E2E)', () => {
    cy.get('#user-input').type('recomendar do actor a{enter}');
    cy.wait('@filmesPorAtor', { timeout: 10000 });
    // 1. Mensagem do utilizador deve aparecer
    cy.get('#chat-log .user-message').last().should('contain.text', 'recomendar do actor a');
    // 2. Resposta do bot deve listar recomendações
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Sample Film');
  });

  it('Deve mostrar uma mensagem de "Não entendi" para inputs inválidos', () => {
    cy.get('#user-input').type('qual a previsão do tempo?{enter}');
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Não entendi');
  });

  // (8º) Intenção composta: ator e gênero
  it('Deve testar a intenção composta "ator e genero" (E2E)', () => {
    const query = 'filme de drama com actor a';

    cy.get('#user-input').type(`${query}{enter}`);
    cy.wait('@atorEGenero', { timeout: 10000 });

    // Verifica se a query do utilizador apareceu
    cy.get('#chat-log .user-message').last().should('contain.text', query);

    // Verifica se o bot respondeu com o filme esperado (ajustado ao dataset)
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Sample Film');
  });

  // (9º) Intenção composta: dois gêneros
  it('Deve testar a intenção composta "dois generos" (E2E)', () => {
    const query = 'filme de drama e comdy';

    cy.get('#user-input').type(`${query}{enter}`);
    cy.wait('@doisGeneros', { timeout: 10000 });

    // Verifica se a query do utilizador apareceu
    cy.get('#chat-log .user-message').last().should('contain.text', query);

    // Verifica se o bot respondeu com o filme esperado (ajustado ao dataset)
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Sample Film');
  });
});