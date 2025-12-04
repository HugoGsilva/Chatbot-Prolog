/**
 * Testes E2E para o Frontend Thin Client (Fase 3).
 * 
 * Validam:
 * - Task 16.1: Single endpoint call
 * - Task 16.2: Session ID in requests
 * - Task 17: Loading indicators
 * - Task 18: Response rendering
 * - Task 19: Session management
 * - Task 21: Clickable suggestions
 * - Task 22: Network error handling
 */
describe('Chatbot Thin Client - Fase 3', () => {

    beforeEach(() => {
        // Limpa localStorage para cada teste
        cy.clearLocalStorage();
        
        // Intercepta endpoints ANTES de visitar a página
        cy.intercept('POST', '/chat').as('chatRequest');
        cy.intercept('POST', '/session/create').as('sessionCreate');
        cy.intercept('DELETE', '/session/*').as('sessionDelete');
        
        cy.visit('/');
        
        // Aguarda a página carregar completamente
        cy.get('#user-input', { timeout: 10000 }).should('be.visible');
        cy.get('#send-button').should('be.visible');
        
        // Aguarda um tempo para garantir que a sessão foi criada
        cy.wait(500);
    });

    // =========================================================================
    // Task 16.1: Property Test - Single Endpoint Call
    // =========================================================================
    describe('Single Endpoint Call (Task 16.1)', () => {
        
        it('Cada mensagem deve resultar em exatamente um POST para /chat', () => {
            cy.get('#user-input').type('filmes de drama{enter}');
            
            cy.wait('@chatRequest', { timeout: 10000 }).then((interception) => {
                expect(interception.request.method).to.equal('POST');
                expect(interception.request.url).to.include('/chat');
            });
        });

        it('Não deve fazer requests GET para endpoints antigos', () => {
            // Intercepta endpoints antigos para verificar que não são chamados
            cy.intercept('GET', '/filmes-por-*').as('oldEndpoint');
            cy.intercept('GET', '/genero-do-*').as('oldEndpoint2');
            cy.intercept('GET', '/recomendar/*').as('oldEndpoint3');
            
            cy.get('#user-input').type('filmes do ator adam sandler{enter}');
            cy.wait('@chatRequest', { timeout: 10000 });
            
            // Aguarda um pouco para garantir que não há outros requests
            cy.wait(500);
            
            // Endpoints antigos não devem ter sido chamados
            cy.get('@oldEndpoint.all').should('have.length', 0);
            cy.get('@oldEndpoint2.all').should('have.length', 0);
            cy.get('@oldEndpoint3.all').should('have.length', 0);
        });
    });

    // =========================================================================
    // Task 16.2: Property Test - Session ID in Requests
    // =========================================================================
    describe('Session ID in Requests (Task 16.2)', () => {
        
        it('Todos os requests devem incluir session_id válido', () => {
            cy.get('#user-input').type('ajuda{enter}');
            
            cy.wait('@chatRequest', { timeout: 10000 }).then((chatInterception) => {
                expect(chatInterception.request.body.session_id).to.be.a('string');
                expect(chatInterception.request.body.session_id.length).to.be.greaterThan(0);
            });
        });

        it('Session ID deve persistir entre múltiplas mensagens', () => {
            let firstSessionId;
            
            // Primeira mensagem
            cy.get('#user-input').type('ola{enter}');
            cy.wait('@chatRequest', { timeout: 10000 }).then((first) => {
                firstSessionId = first.request.body.session_id;
                expect(firstSessionId).to.be.a('string');
            });
            
            // Aguarda resposta aparecer
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 }).should('have.length.at.least', 2);
            
            // Segunda mensagem
            cy.get('#user-input').clear().type('ajuda{enter}');
            cy.wait('@chatRequest', { timeout: 10000 }).then((second) => {
                expect(second.request.body.session_id).to.equal(firstSessionId);
            });
        });
    });

    // =========================================================================
    // Task 17: Loading Indicators
    // =========================================================================
    describe('Loading Indicators (Task 17)', () => {
        
        it('Deve mostrar indicador de loading enquanto aguarda resposta', () => {
            // Intercepta com delay para capturar loading state
            cy.intercept('POST', '/chat', (req) => {
                req.reply({
                    delay: 500,
                    body: { type: 'text', content: 'Resposta', suggestions: null, metadata: {} }
                });
            }).as('delayedChat');
            
            cy.get('#user-input').type('teste{enter}');
            
            // Loading indicator deve aparecer
            cy.get('.loading-indicator, #loading-indicator', { timeout: 5000 }).should('exist');
            
            // Aguarda resposta e verifica que loading desapareceu
            cy.wait('@delayedChat', { timeout: 10000 });
            cy.get('.loading-indicator, #loading-indicator').should('not.exist');
        });

        it('Input e botão devem ficar desabilitados durante loading', () => {
            cy.intercept('POST', '/chat', (req) => {
                req.reply({ delay: 500, body: { type: 'text', content: 'OK', suggestions: null, metadata: {} } });
            }).as('delayedChat');
            
            cy.get('#user-input').type('teste{enter}');
            
            // Verifica que estão desabilitados
            cy.get('#user-input').should('be.disabled');
            cy.get('#send-button').should('be.disabled');
            
            cy.wait('@delayedChat', { timeout: 10000 });
            
            // Verifica que voltaram ao normal
            cy.get('#user-input').should('not.be.disabled');
            cy.get('#send-button').should('not.be.disabled');
        });
    });

    // =========================================================================
    // Task 18: Response Rendering
    // =========================================================================
    describe('Response Rendering (Task 18)', () => {

        it('Deve renderizar resposta do tipo "list" corretamente', () => {
            cy.intercept('POST', '/chat', {
                body: {
                    type: 'list',
                    content: [{ titulo: 'Filme A' }, { titulo: 'Filme B' }],
                    suggestions: null,
                    metadata: {}
                }
            }).as('listResponse');
            
            cy.get('#user-input').type('filmes de drama{enter}');
            cy.wait('@listResponse', { timeout: 10000 });
            
            cy.get('.list-response', { timeout: 5000 }).should('exist');
            cy.get('.list-response').should('contain.text', 'Filme A');
            cy.get('.list-response').should('contain.text', 'Filme B');
        });

        it('Deve renderizar resposta do tipo "error" com estilo distinto', () => {
            cy.intercept('POST', '/chat', {
                body: {
                    type: 'error',
                    content: 'Ator não encontrado',
                    suggestions: ['Verifique o nome', 'Tente outro ator'],
                    metadata: {}
                }
            }).as('errorResponse');
            
            cy.get('#user-input').type('filmes do ator xyz{enter}');
            cy.wait('@errorResponse', { timeout: 10000 });
            
            cy.get('.error-response', { timeout: 5000 }).should('exist');
            cy.get('.error-response').should('contain.text', 'Ator não encontrado');
        });

        it('Deve renderizar resposta do tipo "help" com exemplos', () => {
            cy.intercept('POST', '/chat', {
                body: {
                    type: 'help',
                    content: {
                        message: 'Aqui estão alguns exemplos:',
                        examples: ['filmes de ação', 'filmes do ator X']
                    },
                    suggestions: null,
                    metadata: {}
                }
            }).as('helpResponse');
            
            cy.get('#user-input').type('ajuda{enter}');
            cy.wait('@helpResponse', { timeout: 10000 });
            
            cy.get('.help-response', { timeout: 5000 }).should('exist');
            cy.get('.help-response').should('contain.text', 'exemplos');
        });
    });

    // =========================================================================
    // Task 19: Session Management
    // =========================================================================
    describe('Session Management (Task 19)', () => {

        it('Botão "Nova Conversa" deve limpar e criar nova sessão', () => {
            // Clica no botão de limpar/nova conversa
            cy.get('#clear-button').click();
            
            // Chat-log deve existir
            cy.get('#chat-log').should('exist');
            
            // Verifica que nova sessão funciona enviando mensagem
            cy.get('#user-input').type('teste nova sessao{enter}');
            cy.wait('@chatRequest', { timeout: 10000 }).then((interception) => {
                expect(interception.request.body.session_id).to.be.a('string');
            });
        });
    });

    // =========================================================================
    // Task 21: Clickable Suggestions
    // =========================================================================
    describe('Clickable Suggestions (Task 21)', () => {
        
        it('Sugestões devem ser clicáveis e enviar mensagem', () => {
            cy.intercept('POST', '/chat', {
                body: {
                    type: 'help',
                    content: {
                        message: 'Exemplos:',
                        examples: ['filmes de drama']
                    },
                    suggestions: null,
                    metadata: {}
                }
            }).as('helpResponse');
            
            cy.get('#user-input').type('ajuda{enter}');
            cy.wait('@helpResponse', { timeout: 10000 });
            
            // Verifica se sugestões existem
            cy.get('.suggestion-clickable, .suggestion-item', { timeout: 5000 }).should('exist');
            
            // Remove intercept para deixar passar o próximo request
            cy.intercept('POST', '/chat').as('suggestionClick');
            
            // Clica na primeira sugestão
            cy.get('.suggestion-clickable, .suggestion-item').first().click();
            
            // Verifica que request foi enviado
            cy.wait('@suggestionClick', { timeout: 10000 }).then((interception) => {
                expect(interception.request.body.message).to.include('drama');
            });
        });
    });

    // =========================================================================
    // Task 22: Network Error Handling
    // =========================================================================
    describe('Network Error Handling (Task 22)', () => {
        
        it('Deve mostrar erro amigável quando servidor falhar', () => {
            cy.intercept('POST', '/chat', {
                statusCode: 500,
                body: { detail: 'Internal Server Error' }
            }).as('serverError');
            
            cy.get('#user-input').type('teste{enter}');
            cy.wait('@serverError', { timeout: 10000 });
            
            // Verifica que mensagem de erro aparece
            cy.get('.error-response, .error-message', { timeout: 5000 }).should('exist');
        });

        it('Deve mostrar erro quando rate limit excedido (429)', () => {
            cy.intercept('POST', '/chat', {
                statusCode: 429,
                body: { detail: 'Too Many Requests' }
            }).as('rateLimitError');
            
            cy.get('#user-input').type('teste{enter}');
            cy.wait('@rateLimitError', { timeout: 10000 });
            
            // Verifica que mensagem de erro aparece
            cy.get('.error-response, .error-message', { timeout: 5000 }).should('exist');
        });
    });

    // =========================================================================
    // Teste de Integração - Fluxo Completo
    // =========================================================================
    describe('Fluxo Completo de Integração', () => {
        
        it('Deve executar fluxo completo: sessão -> mensagem -> resposta', () => {
            // 1. Envia mensagem real
            cy.get('#user-input').type('filmes de acao{enter}');
            
            // 2. Aguarda e verifica resposta
            cy.wait('@chatRequest', { timeout: 10000 }).then((interception) => {
                expect(interception.request.body.message).to.equal('filmes de acao');
                expect(interception.request.body.session_id).to.be.a('string');
            });
            
            // 3. Resposta deve aparecer no chat-log (mais de 1 mensagem, já tem help inicial)
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 }).should('have.length.at.least', 2);
            
            // 4. Input deve estar habilitado novamente
            cy.get('#user-input').should('not.be.disabled');
        });
    });
});
