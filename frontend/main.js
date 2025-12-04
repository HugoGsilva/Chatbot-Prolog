/**
 * Frontend Thin Client para o Chatbot Netflix-Prolog.
 * 
 * Fase 3 - Arquitetura Thin Client:
 * - Toda lógica de NLU, roteamento e processamento está no backend
 * - Frontend apenas envia texto bruto e renderiza respostas estruturadas
 * - Sessão gerida pelo servidor com TTL de 24h
 */
document.addEventListener('DOMContentLoaded', () => {
    // --- REFERÊNCIAS DOM ---
    const chatLog = document.getElementById('chat-log');
    const userInput = document.getElementById('user-input');
    const sendButton = document.getElementById('send-button');
    const chatWindow = document.getElementById('chat-window');
    const clearButton = document.getElementById('clear-button');

    // --- CONFIGURAÇÃO ---
    const API_BASE_URL = '';  // Relativo ao mesmo host
    const SESSION_STORAGE_KEY = 'chatbot_session_id';
    const SESSION_TIMESTAMP_KEY = 'chatbot_session_timestamp';

    // --- GESTÃO DE SESSÃO ---
    
    /**
     * Obtém ou cria um session_id.
     * Se não existir no localStorage, solicita ao servidor.
     */
    async function getOrCreateSessionId() {
        let sessionId = localStorage.getItem(SESSION_STORAGE_KEY);
        const timestamp = localStorage.getItem(SESSION_TIMESTAMP_KEY);
        
        // Verifica se sessão existe e não está muito antiga (> 23h)
        if (sessionId && timestamp) {
            const age = Date.now() - parseInt(timestamp, 10);
            const hoursOld = age / (1000 * 60 * 60);
            
            if (hoursOld > 23) {
                console.log('[Session] Sessão com mais de 23h, criando nova...');
                sessionId = null;
            }
        }
        
        if (!sessionId) {
            try {
                const response = await fetch(`${API_BASE_URL}/session/create`, {
                    method: 'POST'
                });
                
                if (response.ok) {
                    const data = await response.json();
                    sessionId = data.session_id;
                    localStorage.setItem(SESSION_STORAGE_KEY, sessionId);
                    localStorage.setItem(SESSION_TIMESTAMP_KEY, Date.now().toString());
                    console.log('[Session] Nova sessão criada:', sessionId);
                } else {
                    // Fallback: gerar ID localmente se servidor falhar
                    sessionId = 'local_' + Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
                    localStorage.setItem(SESSION_STORAGE_KEY, sessionId);
                    localStorage.setItem(SESSION_TIMESTAMP_KEY, Date.now().toString());
                    console.warn('[Session] Fallback para sessão local:', sessionId);
                }
            } catch (error) {
                // Fallback em caso de erro de rede
                sessionId = 'local_' + Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
                localStorage.setItem(SESSION_STORAGE_KEY, sessionId);
                localStorage.setItem(SESSION_TIMESTAMP_KEY, Date.now().toString());
                console.warn('[Session] Erro de rede, usando sessão local:', sessionId);
            }
        }
        
        return sessionId;
    }

    /**
     * Limpa a sessão atual e cria uma nova.
     */
    async function clearSession() {
        const sessionId = localStorage.getItem(SESSION_STORAGE_KEY);
        
        if (sessionId) {
            try {
                await fetch(`${API_BASE_URL}/session/${sessionId}`, {
                    method: 'DELETE'
                });
            } catch (error) {
                console.warn('[Session] Erro ao deletar sessão no servidor:', error);
            }
        }
        
        localStorage.removeItem(SESSION_STORAGE_KEY);
        localStorage.removeItem(SESSION_TIMESTAMP_KEY);
        
        // Limpa o chat visualmente (mantém apenas a mensagem de boas-vindas)
        const welcomeMessage = chatLog.querySelector('.help-text');
        chatLog.innerHTML = '';
        if (welcomeMessage) {
            chatLog.appendChild(welcomeMessage.cloneNode(true));
        }
        
        // Cria nova sessão
        await getOrCreateSessionId();
        
        displaySystemMessage('Conversa limpa. Nova sessão iniciada.');
    }

    // --- INDICADORES DE LOADING ---
    
    /**
     * Mostra indicador de carregamento (typing dots).
     */
    function showLoadingIndicator() {
        const existingLoader = document.getElementById('loading-indicator');
        if (existingLoader) return;
        
        const loader = document.createElement('div');
        loader.id = 'loading-indicator';
        loader.className = 'chat-message bot-message loading-indicator';
        loader.innerHTML = '<span class="dot"></span><span class="dot"></span><span class="dot"></span>';
        loader.setAttribute('aria-label', 'Carregando resposta...');
        chatLog.appendChild(loader);
        scrollToBottom();
    }

    /**
     * Remove indicador de carregamento.
     */
    function hideLoadingIndicator() {
        const loader = document.getElementById('loading-indicator');
        if (loader) {
            loader.remove();
        }
    }

    // --- ENVIO DE MENSAGENS ---

    /**
     * Envia mensagem para o endpoint /chat.
     * @param {string} text - Texto bruto do utilizador
     * @returns {Promise<Object>} - Resposta estruturada do backend
     */
    async function sendMessage(text) {
        const sessionId = await getOrCreateSessionId();
        
        const response = await fetch(`${API_BASE_URL}/chat`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({
                message: text,
                session_id: sessionId
            })
        });
        
        if (!response.ok) {
            if (response.status === 429) {
                throw new Error('Muitas requisições. Aguarde um momento antes de enviar outra mensagem.');
            }
            if (response.status === 422) {
                const errorData = await response.json();
                throw new Error('Mensagem inválida: ' + (errorData.detail?.[0]?.msg || 'erro de validação'));
            }
            throw new Error(`Erro do servidor (${response.status})`);
        }
        
        return await response.json();
    }

    // --- RENDERIZAÇÃO DE RESPOSTAS ---

    /**
     * Renderiza resposta do backend baseado no tipo.
     * @param {Object} response - Resposta estruturada do backend
     */
    function renderResponse(response) {
        const { type, content, suggestions, metadata } = response;
        
        switch (type) {
            case 'list':
                renderListResponse(content, metadata);
                break;
            case 'text':
                renderTextResponse(content);
                break;
            case 'error':
                renderErrorResponse(content, suggestions);
                break;
            case 'help':
                renderHelpResponse(content);
                break;
            case 'clarification':
                renderClarificationResponse(content, suggestions);
                break;
            default:
                // Fallback para qualquer tipo não reconhecido
                renderTextResponse(typeof content === 'string' ? content : JSON.stringify(content));
        }
        
        // Renderiza sugestões se existirem
        if (suggestions && suggestions.length > 0 && type !== 'error' && type !== 'clarification') {
            renderSuggestions(suggestions);
        }
    }

    /**
     * Renderiza resposta do tipo lista (filmes, géneros, etc.)
     */
    function renderListResponse(content, metadata) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'chat-message bot-message list-response';
        
        // Se for array de objetos com 'titulo'
        if (Array.isArray(content)) {
            const items = content.slice(0, 20); // Limita a 20 itens para UI
            
            if (items.length > 0 && typeof items[0] === 'object') {
                // Lista de filmes
                if ('titulo' in items[0]) {
                    const titlesList = items.map(item => item.titulo).join(', ');
                    messageDiv.innerHTML = `
                        <p class="list-header">Encontrados ${content.length} resultado(s):</p>
                        <p class="list-content">${escapeHtml(titlesList)}</p>
                        ${content.length > 20 ? `<p class="list-more">... e mais ${content.length - 20} resultados</p>` : ''}
                    `;
                } 
                // Lista de géneros
                else if ('nome' in items[0]) {
                    const genresList = items.map(g => capitalize(g.nome)).join(', ');
                    messageDiv.innerHTML = `<p>${escapeHtml(genresList)}</p>`;
                }
                else {
                    messageDiv.textContent = JSON.stringify(content);
                }
            } else {
                // Array de strings
                messageDiv.innerHTML = `<p>${escapeHtml(items.join(', '))}</p>`;
            }
        } else if (typeof content === 'object') {
            // Objeto único (ex: contagem)
            if ('genero' in content && 'contagem' in content) {
                messageDiv.innerHTML = `<p>Contagem: <strong>${content.contagem}</strong> filmes</p>`;
            } else {
                messageDiv.textContent = JSON.stringify(content);
            }
        } else {
            messageDiv.textContent = String(content);
        }
        
        chatLog.appendChild(messageDiv);
        scrollToBottom();
    }

    /**
     * Renderiza resposta do tipo texto simples.
     */
    function renderTextResponse(content) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'chat-message bot-message text-response';
        messageDiv.textContent = content;
        chatLog.appendChild(messageDiv);
        scrollToBottom();
    }

    /**
     * Renderiza resposta de erro.
     */
    function renderErrorResponse(content, suggestions) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'chat-message bot-message error-response';
        messageDiv.setAttribute('role', 'alert');
        
        let html = `<p class="error-content">⚠️ ${escapeHtml(content)}</p>`;
        
        if (suggestions && suggestions.length > 0) {
            html += '<ul class="error-suggestions">';
            suggestions.forEach(s => {
                html += `<li>${escapeHtml(s)}</li>`;
            });
            html += '</ul>';
        }
        
        messageDiv.innerHTML = html;
        chatLog.appendChild(messageDiv);
        scrollToBottom();
    }

    /**
     * Renderiza resposta de ajuda.
     */
    function renderHelpResponse(content) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'chat-message bot-message help-response';
        
        if (typeof content === 'object' && content.message && content.examples) {
            let html = `<p class="help-message">${escapeHtml(content.message)}</p>`;
            html += '<ul class="help-examples">';
            content.examples.forEach(ex => {
                html += `<li><code class="suggestion-clickable" data-query="${escapeAttr(ex)}">${escapeHtml(ex)}</code></li>`;
            });
            html += '</ul>';
            messageDiv.innerHTML = html;
        } else {
            messageDiv.textContent = typeof content === 'string' ? content : JSON.stringify(content);
        }
        
        chatLog.appendChild(messageDiv);
        scrollToBottom();
        
        // Adiciona listeners para exemplos clicáveis
        addClickableListeners(messageDiv);
    }

    /**
     * Renderiza resposta de clarificação (baixa confiança).
     */
    function renderClarificationResponse(content, suggestions) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'chat-message bot-message clarification-response';
        
        let html = `<p>${escapeHtml(content)}</p>`;
        
        if (suggestions && suggestions.length > 0) {
            html += '<div class="clarification-suggestions">';
            suggestions.forEach(s => {
                html += `<button class="suggestion-btn" data-query="${escapeAttr(s)}">${escapeHtml(s)}</button>`;
            });
            html += '</div>';
        }
        
        messageDiv.innerHTML = html;
        chatLog.appendChild(messageDiv);
        scrollToBottom();
        
        // Adiciona listeners para sugestões clicáveis
        addClickableListeners(messageDiv);
    }

    /**
     * Renderiza sugestões clicáveis.
     */
    function renderSuggestions(suggestions) {
        const suggestionsDiv = document.createElement('div');
        suggestionsDiv.className = 'suggestions-container';
        
        suggestions.forEach(s => {
            const btn = document.createElement('button');
            btn.className = 'suggestion-btn';
            btn.textContent = s;
            btn.dataset.query = s;
            suggestionsDiv.appendChild(btn);
        });
        
        chatLog.appendChild(suggestionsDiv);
        scrollToBottom();
        
        addClickableListeners(suggestionsDiv);
    }

    /**
     * Adiciona listeners para elementos clicáveis.
     */
    function addClickableListeners(container) {
        container.querySelectorAll('.suggestion-btn, .suggestion-clickable').forEach(el => {
            el.addEventListener('click', async () => {
                const query = el.dataset.query;
                if (query) {
                    userInput.value = query;
                    await handleSendMessage();
                }
            });
        });
    }

    /**
     * Mostra mensagem do utilizador no chat.
     */
    function displayUserMessage(text) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'chat-message user-message';
        messageDiv.textContent = text;
        chatLog.appendChild(messageDiv);
        scrollToBottom();
    }

    /**
     * Mostra mensagem do sistema (informativa).
     */
    function displaySystemMessage(text) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'chat-message system-message';
        messageDiv.textContent = text;
        chatLog.appendChild(messageDiv);
        scrollToBottom();
    }

    // --- UTILIDADES ---

    function scrollToBottom() {
        chatWindow.scrollTop = chatWindow.scrollHeight;
    }

    function escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    function escapeAttr(text) {
        return text.replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function capitalize(str) {
        if (!str) return '';
        return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
    }

    // --- HANDLER PRINCIPAL ---

    /**
     * Handler principal para envio de mensagens.
     */
    async function handleSendMessage() {
        const text = userInput.value.trim();
        if (!text) return;
        
        // Mostra mensagem do utilizador
        displayUserMessage(text);
        userInput.value = '';
        userInput.disabled = true;
        sendButton.disabled = true;
        
        // Mostra indicador de carregamento
        showLoadingIndicator();
        
        try {
            // Envia para o backend
            const response = await sendMessage(text);
            
            // Remove indicador e renderiza resposta
            hideLoadingIndicator();
            renderResponse(response);
            
        } catch (error) {
            hideLoadingIndicator();
            renderErrorResponse(
                error.message || 'Desculpe, ocorreu um erro ao processar sua mensagem.',
                ['Tente novamente', 'Digite "ajuda" para ver comandos disponíveis']
            );
        } finally {
            userInput.disabled = false;
            sendButton.disabled = false;
            userInput.focus();
        }
    }

    // --- INICIALIZAÇÃO ---

    // Event listeners
    sendButton.addEventListener('click', handleSendMessage);
    
    userInput.addEventListener('keydown', (ev) => {
        if (ev.key === 'Enter' && !ev.shiftKey) {
            ev.preventDefault();
            handleSendMessage();
        }
    });

    // Botão de limpar conversa (se existir)
    if (clearButton) {
        clearButton.addEventListener('click', clearSession);
    }

    // Inicializa sessão ao carregar
    getOrCreateSessionId().then(sessionId => {
        console.log('[Init] Sessão ativa:', sessionId);
    });

    // Focus no input
    userInput.focus();
});
