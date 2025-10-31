// cypress/e2e/chatbot_flow.cy.js
describe('Fluxo Completo do Chatbot Sakila-Prolog', () => {

  beforeEach(() => {
    // Visita a página antes de cada teste
    cy.visit('/');
  });

  it('Deve carregar e exibir a mensagem inicial do Bot', () => {
    // Stub: Verificar se a primeira mensagem do Bot está visível
    cy.get('#chat-log').contains('Bot: Olá! Pergunte-me sobre filmes.');
  });

  it('Deve testar a intenção "filmes por ator" (E2E)', () => {
    // Stub:
    // 1. Digitar no input
    cy.get('#user-input').type('filmes por PENELOPE GUINESS');
    // 2. Clicar em Enviar
    cy.get('#send-button').click();
    // 3. Verificar a resposta do Bot
    cy.get('#chat-log').contains('Bot:');
    cy.get('#chat-log').contains('ACADEMY DINOSAUR'); // Assumindo que este filme existe
  });

  it('Deve testar a intenção "contar filmes" (E2E)', () => {
    // Stub:
    cy.get('#user-input').type('contar filmes de Action em 2006');
    cy.get('#send-button').click();
    cy.get('#chat-log').contains('Bot: Contagem (Action, 2006):');
  });

  it('Deve testar a intenção "gênero do filme" (E2E)', () => {
    // Stub:
    cy.get('#user-input').type('gênero do ACADEMY DINOSAUR');
    cy.get('#send-button').click();
    cy.get('#chat-log').contains('Bot: Documentary');
  });

  it('Deve testar a intenção "filmes por genero" (E2E)', () => {
    // Stub: (Implemente este)
  });

  it('Deve testar a intenção "recomendar por ator" (E2E)', () => {
    // Stub: (Implemente este)
  });

  it('Deve mostrar uma mensagem de "Não entendi" para inputs inválidos', () => {
    // Stub:
    cy.get('#user-input').type('qual a previsão do tempo?');
    cy.get('#send-button').click();
    cy.get('#chat-log').contains('Bot: Não entendi.');
  });
});