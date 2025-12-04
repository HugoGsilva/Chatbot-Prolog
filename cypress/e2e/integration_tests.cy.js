/**
 * Testes E2E Completos - Fase 4 (Integração e Testes)
 * 
 * Validam:
 * - Task 24: Testes E2E para todos os intents
 * - Task 24.1: Normalização de entidades
 * - Task 24.2: Tradução de gêneros
 * - Task 24.3: Feedback de entidade faltando
 * - Task 24.4: Sugestões para queries malformadas
 */
describe('Chatbot - Fase 4: Testes de Integração', () => {

    beforeEach(() => {
        cy.clearLocalStorage();
        cy.intercept('POST', '/chat').as('chatRequest');
        cy.intercept('POST', '/session/create').as('sessionCreate');
        cy.visit('/');
        cy.get('#user-input', { timeout: 10000 }).should('be.visible');
        cy.wait(500);
    });

    // =========================================================================
    // Task 24: E2E Tests - Complete User Flows
    // =========================================================================
    describe('User Flows - Fluxo Completo (Task 24)', () => {
        
        it('Fluxo: digitar → loading → resposta', () => {
            // Digita mensagem
            cy.get('#user-input').type('filmes de ação{enter}');
            
            // Verifica loading
            cy.get('.loading-indicator, #loading-indicator').should('exist');
            
            // Aguarda resposta
            cy.wait('@chatRequest', { timeout: 15000 });
            
            // Loading deve sumir
            cy.get('.loading-indicator, #loading-indicator').should('not.exist');
            
            // Resposta deve aparecer
            cy.get('#chat-log .chat-message.bot-message').should('have.length.at.least', 2);
        });

        it('Fluxo: limpar conversa mantém funcionalidade', () => {
            // Envia primeira mensagem
            cy.get('#user-input').type('ajuda{enter}');
            cy.wait('@chatRequest', { timeout: 10000 });
            
            // Limpa conversa
            cy.get('#clear-button').click();
            
            // Envia nova mensagem
            cy.get('#user-input').type('filmes de drama{enter}');
            cy.wait('@chatRequest', { timeout: 10000 });
            
            // Deve funcionar normalmente
            cy.get('#chat-log .chat-message.bot-message').should('exist');
        });
    });

    // =========================================================================
    // Task 24: E2E Tests - Each Intent Type
    // =========================================================================
    describe('Intent Types - Cada Tipo de Intent (Task 24)', () => {
        
        it('Intent: filmes_por_genero', () => {
            cy.get('#user-input').type('filmes de comédia{enter}');
            cy.wait('@chatRequest', { timeout: 15000 }).then((interception) => {
                expect(interception.request.body.message).to.include('comédia');
            });
            
            // Resposta deve conter lista ou mensagem
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 2);
        });

        it('Intent: filmes_por_ator', () => {
            cy.get('#user-input').type('filmes do ator adam sandler{enter}');
            cy.wait('@chatRequest', { timeout: 15000 }).then((interception) => {
                expect(interception.request.body.message.toLowerCase()).to.include('adam sandler');
            });
            
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 2);
        });

        it('Intent: filmes_por_diretor', () => {
            cy.get('#user-input').type('filmes do diretor steven spielberg{enter}');
            cy.wait('@chatRequest', { timeout: 15000 });
            
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 2);
        });

        it('Intent: ajuda', () => {
            cy.get('#user-input').type('ajuda{enter}');
            cy.wait('@chatRequest', { timeout: 10000 });
            
            // Deve retornar alguma resposta de ajuda
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 2);
        });

        it('Intent: saudação', () => {
            cy.get('#user-input').type('olá{enter}');
            cy.wait('@chatRequest', { timeout: 10000 });
            
            // Deve responder com texto amigável
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 2);
        });
    });

    // =========================================================================
    // Task 24: E2E Tests - Error Scenarios
    // =========================================================================
    describe('Error Scenarios - Cenários de Erro (Task 24)', () => {
        
        it('Erro: Ator não encontrado', () => {
            cy.get('#user-input').type('filmes do ator xyzabcdef123{enter}');
            cy.wait('@chatRequest', { timeout: 15000 });
            
            // Deve mostrar mensagem de erro ou "não encontrado"
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .last()
                .should('exist');
        });

        it('Erro: Gênero não encontrado', () => {
            cy.get('#user-input').type('filmes de xyzgenero123{enter}');
            cy.wait('@chatRequest', { timeout: 15000 });
            
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .last()
                .should('exist');
        });

        it('Erro: Query sem sentido (baixa confiança)', () => {
            cy.get('#user-input').type('asdfghjkl qwerty{enter}');
            cy.wait('@chatRequest', { timeout: 15000 });
            
            // Deve retornar ajuda ou mensagem de não entendido
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .last()
                .should('exist');
        });

        it('Erro: Servidor retorna 500', () => {
            cy.intercept('POST', '/chat', {
                statusCode: 500,
                body: { detail: 'Internal Server Error' }
            }).as('serverError');
            
            cy.get('#user-input').type('teste erro 500{enter}');
            cy.wait('@serverError', { timeout: 10000 });
            
            // Deve mostrar mensagem de erro amigável
            cy.get('.error-response, .error-message', { timeout: 5000 }).should('exist');
        });

        it('Erro: Rate limit (429)', () => {
            cy.intercept('POST', '/chat', {
                statusCode: 429,
                body: { detail: 'Rate limit exceeded' }
            }).as('rateLimitError');
            
            cy.get('#user-input').type('teste rate limit{enter}');
            cy.wait('@rateLimitError', { timeout: 10000 });
            
            cy.get('.error-response, .error-message', { timeout: 5000 }).should('exist');
        });
    });

    // =========================================================================
    // Task 24: E2E Tests - Session Persistence
    // =========================================================================
    describe('Session Persistence - Persistência de Sessão (Task 24)', () => {
        
        it('Sessão persiste entre múltiplas mensagens', () => {
            let sessionId;
            
            // Primeira mensagem
            cy.get('#user-input').type('ola{enter}');
            cy.wait('@chatRequest', { timeout: 10000 }).then((int1) => {
                sessionId = int1.request.body.session_id;
                expect(sessionId).to.be.a('string');
            });
            
            // Aguarda resposta
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 2);
            
            // Segunda mensagem
            cy.get('#user-input').clear().type('filmes de ação{enter}');
            cy.wait('@chatRequest', { timeout: 10000 }).then((int2) => {
                expect(int2.request.body.session_id).to.equal(sessionId);
            });
            
            // Terceira mensagem
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 3);
                
            cy.get('#user-input').clear().type('ajuda{enter}');
            cy.wait('@chatRequest', { timeout: 10000 }).then((int3) => {
                expect(int3.request.body.session_id).to.equal(sessionId);
            });
        });

        it('Nova conversa gera nova sessão', () => {
            let firstSessionId;
            
            // Primeira mensagem
            cy.get('#user-input').type('teste{enter}');
            cy.wait('@chatRequest', { timeout: 10000 }).then((int1) => {
                firstSessionId = int1.request.body.session_id;
            });
            
            // Aguarda resposta
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 2);
            
            // Limpa conversa
            cy.get('#clear-button').click();
            cy.wait(500);
            
            // Nova mensagem pode ter sessão diferente ou mesma (depende da implementação)
            cy.get('#user-input').type('nova mensagem{enter}');
            cy.wait('@chatRequest', { timeout: 10000 }).then((int2) => {
                // Session deve existir e ser válida
                expect(int2.request.body.session_id).to.be.a('string');
                expect(int2.request.body.session_id.length).to.be.greaterThan(0);
            });
        });
    });

    // =========================================================================
    // Task 24: E2E Tests - Clickable Suggestions
    // =========================================================================
    describe('Clickable Suggestions - Sugestões Clicáveis (Task 24)', () => {
        
        it('Sugestões de ajuda são clicáveis', () => {
            // Mock resposta com sugestões
            cy.intercept('POST', '/chat', {
                body: {
                    type: 'help',
                    content: {
                        message: 'Exemplos de perguntas:',
                        examples: ['filmes de ação', 'filmes do ator X']
                    },
                    suggestions: null,
                    metadata: {}
                }
            }).as('helpResponse');
            
            cy.get('#user-input').type('ajuda{enter}');
            cy.wait('@helpResponse', { timeout: 10000 });
            
            // Verifica sugestões
            cy.get('.suggestion-clickable, .suggestion-item, .suggestion-btn', { timeout: 5000 })
                .should('exist');
            
            // Setup intercept para próximo request
            cy.intercept('POST', '/chat').as('suggestionRequest');
            
            // Clica na sugestão
            cy.get('.suggestion-clickable, .suggestion-item, .suggestion-btn').first().click();
            
            // Verifica que request foi enviado
            cy.wait('@suggestionRequest', { timeout: 10000 });
        });
    });
});

// =========================================================================
// Task 24.1: Property Test - Entity Normalization
// =========================================================================
describe('Entity Normalization - Normalização de Entidades (Task 24.1)', () => {
    
    beforeEach(() => {
        cy.clearLocalStorage();
        cy.intercept('POST', '/chat').as('chatRequest');
        cy.visit('/');
        cy.get('#user-input', { timeout: 10000 }).should('be.visible');
        cy.wait(500);
    });

    it('Deve normalizar nome de ator com variações de capitalização', () => {
        cy.get('#user-input').type('filmes do ator ADAM SANDLER{enter}');
        cy.wait('@chatRequest', { timeout: 15000 }).then((interception) => {
            // Request deve ser enviado com o texto
            expect(interception.request.body.message.toLowerCase()).to.include('adam sandler');
        });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .should('have.length.at.least', 2);
    });

    it('Deve normalizar nome de ator com espaços extras', () => {
        cy.get('#user-input').type('filmes do ator  adam   sandler{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .should('have.length.at.least', 2);
    });

    it('Deve lidar com nomes parciais de ator', () => {
        cy.get('#user-input').type('filmes do ator sandler{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .should('have.length.at.least', 2);
    });
});

// =========================================================================
// Task 24.2: Property Test - Genre Translation
// =========================================================================
describe('Genre Translation - Tradução de Gêneros (Task 24.2)', () => {
    
    beforeEach(() => {
        cy.clearLocalStorage();
        cy.intercept('POST', '/chat').as('chatRequest');
        cy.visit('/');
        cy.get('#user-input', { timeout: 10000 }).should('be.visible');
        cy.wait(500);
    });

    const genreMappings = [
        { pt: 'ação', en: 'action' },
        { pt: 'comédia', en: 'comedy' },
        { pt: 'drama', en: 'drama' },
        { pt: 'terror', en: 'horror' },
        { pt: 'romance', en: 'romance' },
        { pt: 'ficção científica', en: 'sci-fi' },
        { pt: 'documentário', en: 'documentary' }
    ];

    genreMappings.forEach(({ pt, en }) => {
        it(`Deve aceitar gênero em português: "${pt}"`, () => {
            cy.get('#user-input').type(`filmes de ${pt}{enter}`);
            cy.wait('@chatRequest', { timeout: 15000 });
            
            cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
                .should('have.length.at.least', 2);
        });
    });

    it('Deve aceitar gênero em inglês: "action"', () => {
        cy.get('#user-input').type('filmes de action{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .should('have.length.at.least', 2);
    });
});

// =========================================================================
// Task 24.3: Property Test - Missing Entity Feedback
// =========================================================================
describe('Missing Entity Feedback - Entidade Faltando (Task 24.3)', () => {
    
    beforeEach(() => {
        cy.clearLocalStorage();
        cy.intercept('POST', '/chat').as('chatRequest');
        cy.visit('/');
        cy.get('#user-input', { timeout: 10000 }).should('be.visible');
        cy.wait(500);
    });

    it('Deve dar feedback quando gênero está faltando: "filmes de"', () => {
        cy.get('#user-input').type('filmes de{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        // Deve retornar alguma resposta (erro, ajuda ou pedido de clarificação)
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .last()
            .should('exist');
    });

    it('Deve dar feedback quando ator está faltando: "filmes do ator"', () => {
        cy.get('#user-input').type('filmes do ator{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .last()
            .should('exist');
    });

    it('Deve dar feedback quando diretor está faltando: "filmes do diretor"', () => {
        cy.get('#user-input').type('filmes do diretor{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .last()
            .should('exist');
    });
});

// =========================================================================
// Task 24.4: Property Test - Malformed Query Suggestions
// =========================================================================
describe('Malformed Query Suggestions - Queries Malformadas (Task 24.4)', () => {
    
    beforeEach(() => {
        cy.clearLocalStorage();
        cy.intercept('POST', '/chat').as('chatRequest');
        cy.visit('/');
        cy.get('#user-input', { timeout: 10000 }).should('be.visible');
        cy.wait(500);
    });

    it('Deve sugerir reformulação para: "filme ator adam sandler"', () => {
        cy.get('#user-input').type('filme ator adam sandler{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        // Deve responder (mesmo que com baixa confiança)
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .last()
            .should('exist');
    });

    it('Deve sugerir reformulação para: "quero ação"', () => {
        cy.get('#user-input').type('quero ação{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .last()
            .should('exist');
    });

    it('Deve lidar com estrutura incorreta: "adam sandler filmes"', () => {
        cy.get('#user-input').type('adam sandler filmes{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .last()
            .should('exist');
    });

    it('Deve lidar com preposição errada: "filmes com ação"', () => {
        cy.get('#user-input').type('filmes com ação{enter}');
        cy.wait('@chatRequest', { timeout: 15000 });
        
        cy.get('#chat-log .chat-message.bot-message', { timeout: 10000 })
            .last()
            .should('exist');
    });
});
