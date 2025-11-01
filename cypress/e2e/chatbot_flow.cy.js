// cypress/e2e/chatbot_flow.cy.js
describe('Fluxo Completo do Chatbot Sakila-Prolog', () => {

  beforeEach(() => {
    // Visita a página antes de cada teste
    cy.visit('/');
    // Aguarda pelos endpoints relevantes
    cy.intercept('GET', '/filmes-por-ator/*').as('filmesPorAtor');
  });

  it('Deve carregar e exibir a mensagem inicial do Bot', () => {
    // Verifica a mensagem de ajuda inicial renderizada pelo HTML
    cy.get('#chat-log .bot-message.help-text').first().should('contain.text', 'Olá! Sou o Chatbot Sakila');
  });

  it('Deve testar a intenção "filmes por ator" (E2E)', () => {
    // 1. Digitar com erro (fuzzy) e submeter via Enter
    cy.get('#user-input').type('flmes por penlope{enter}');
    // 2. Verifica se a mensagem do utilizador foi adicionada
    cy.get('#chat-log .user-message').last().should('contain.text', 'flmes por penlope');
    // 3. Aguarda resposta do endpoint para garantir rendering
    cy.wait('@filmesPorAtor', { timeout: 10000 });
    // 4. Conteúdo esperado (fuzzy): deve listar filmes da Penelope
    // Alinhado aos dados reais retornados pelo backend (ex.: "AMADEUS HOLY")
    cy.get('#chat-log .bot-message').last().should('contain.text', 'AMADEUS HOLY');
  });

  it('Deve testar a intenção "contar filmes" (E2E)', () => {
    cy.get('#user-input').type('contar filmes de action em 2006{enter}');
    // Verifica última resposta do bot com o formato de contagem
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Contagem (Action, 2006):');
  });

  it('Deve testar a intenção "gênero do filme" (E2E)', () => {
    cy.get('#user-input').type('genero do acdemy dinosaur{enter}');
    // Deve retornar o género correto do filme
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Documentary');
  });

  it('Deve testar a intenção "filmes por genero" (E2E)', () => {
    cy.get('#user-input').type('filmes de acao{enter}');
    // 1. Mensagem do utilizador deve aparecer
    cy.get('#chat-log .user-message').last().should('contain.text', 'filmes de acao');
    // 2. Resposta do bot deve listar vários filmes (texto com vírgulas)
    cy.get('#chat-log .bot-message').last().should('exist').and('contain.text', ',');
  });

  it('Deve testar a intenção "recomendar por ator" (E2E)', () => {
    cy.get('#user-input').type('recomendar do penelope{enter}');
    // 1. Mensagem do utilizador deve aparecer
    cy.get('#chat-log .user-message').last().should('contain.text', 'recomendar do penelope');
    // 2. Resposta do bot deve listar recomendações (texto com vírgulas)
    cy.get('#chat-log .bot-message').last().should('exist').and('contain.text', ',');
  });

  it('Deve mostrar uma mensagem de "Não entendi" para inputs inválidos', () => {
    cy.get('#user-input').type('qual a previsão do tempo?{enter}');
    cy.get('#chat-log .bot-message').last().should('contain.text', 'Não entendi');
  });
});