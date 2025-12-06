/**
 * Frontend Thin Client Moderno para o Chatbot Netflix-Prolog - Versão Intermediária
 * 
 * Features:
 * - Markdown rendering com marked.js
 * - Copy to clipboard em mensagens do bot
 * - Indicador de latência em tempo real
 * - Quick actions dinâmicas
 * - Animações suaves de entrada
 * - Status de conexão visual
 * - Scroll inteligente
 */
document.addEventListener('DOMContentLoaded', () => {
    // --- REFERÊNCIAS DOM ---
    const chatLog = document.getElementById('chat-log');
    const userInput = document.getElementById('user-input');
    const sendButton = document.getElementById('send-button');
    const chatWindow = document.getElementById('chat-window');
    const clearButton = document.getElementById('clear-button');
    const statusDot = document.getElementById('status-dot');
    const statusText = document.getElementById('status-text');
    const latencyIndicator = document.getElementById('latency-indicator');
    const quickActionsContainer = document.getElementById('quick-actions');

    // --- CONFIGURAÇÃO ---
    const API_BASE_URL = '';  // Relativo ao mesmo host
    const SESSION_STORAGE_KEY = 'chatbot_session_id';
    const SESSION_TIMESTAMP_KEY = 'chatbot_session_timestamp';

    // Configurar marked.js
    if (typeof marked !== 'undefined') {
        marked.setOptions({
            breaks: true,
            gfm: true,
            headerIds: false,
            mangle: false
        });
    }

    // --- GESTÃO DE ESTADO ---
    let isUserScrolling = false;
    let scrollTimeout = null;
    let lastRequestTime = 0;

    // Detectar quando usuário está scrollando manualmente
    chatWindow.addEventListener('scroll', () => {
        const isAtBottom = chatWindow.scrollHeight - chatWindow.scrollTop <= chatWindow.clientHeight + 50;
        isUserScrolling = !isAtBottom;
        
        clearTimeout(scrollTimeout);
        scrollTimeout = setTimeout(() => {
            isUserScrolling = false;
        }, 1000);
    });

    // --- GESTÃO DE SESSÃO ---
    
    /**
     * Obtém ou cria um session_id.
     */
    async function getOrCreateSessionId() {
        let sessionId = localStorage.getItem(SESSION_STORAGE_KEY);
        const timestamp = localStorage.getItem(SESSION_TIMESTAMP_KEY);
        
        if (sessionId && timestamp) {
            const age = Date.now() - parseInt(timestamp, 10);
            const hoursOld = age / (1000 * 60 * 60);
            
            if (hoursOld > 23) {
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
                    updateStatus(true);
                } else {
                    sessionId = 'local_' + Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
                    localStorage.setItem(SESSION_STORAGE_KEY, sessionId);
                    localStorage.setItem(SESSION_TIMESTAMP_KEY, Date.now().toString());
                    updateStatus(false, 'Sessão local');
                }
            } catch (error) {
                sessionId = 'local_' + Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
                localStorage.setItem(SESSION_STORAGE_KEY, sessionId);
                localStorage.setItem(SESSION_TIMESTAMP_KEY, Date.now().toString());
                updateStatus(false, 'Offline');
            }
        } else {
            updateStatus(true);
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
                console.warn('[Session] Erro ao deletar sessão:', error);
            }
        }
        
        localStorage.removeItem(SESSION_STORAGE_KEY);
        localStorage.removeItem(SESSION_TIMESTAMP_KEY);
        
        // Limpa o chat (mantém apenas welcome message)
        const welcomeMessage = chatLog.querySelector('.welcome-message');
        chatLog.innerHTML = '';
        if (welcomeMessage) {
            chatLog.appendChild(welcomeMessage.cloneNode(true));
        }
        
        await getOrCreateSessionId();
        
        displaySystemMessage('✨ Nova conversa iniciada');
    }

    // --- STATUS DE CONEXÃO ---
    
    /**
     * Atualiza o indicador de status de conexão.
     */
    function updateStatus(isConnected, message = null) {
        if (isConnected) {
            statusDot.classList.add('connected');
            statusText.textContent = message || 'Conectado';
        } else {
            statusDot.classList.remove('connected');
            statusText.textContent = message || 'Desconectado';
        }
    }

    /**
     * Atualiza o indicador de latência.
     */
    function updateLatency(ms) {
        if (ms < 500) {
            latencyIndicator.innerHTML = `⚡ ${ms}ms`;
            latencyIndicator.style.color = 'var(--accent-success)';
        } else if (ms < 1500) {
            latencyIndicator.innerHTML = `⏱️ ${ms}ms`;
            latencyIndicator.style.color = 'var(--accent-warning)';
        } else {
            latencyIndicator.innerHTML = `🐌 ${ms}ms`;
            latencyIndicator.style.color = 'var(--accent-error)';
        }
    }

    // --- INDICADORES DE LOADING ---
    
    /**
     * Mostra indicador de carregamento.
     */
    function showLoadingIndicator() {
        const existingLoader = document.getElementById('loading-indicator');
        if (existingLoader) return;
        
        const wrapper = document.createElement('div');
        wrapper.className = 'message-wrapper bot-wrapper';
        wrapper.id = 'loading-indicator';
        
        const avatar = document.createElement('div');
        avatar.className = 'avatar bot-avatar';
        avatar.textContent = '🤖';
        
        const messageContent = document.createElement('div');
        messageContent.className = 'message-content';
        
        const bubble = document.createElement('div');
        bubble.className = 'message-bubble bot-bubble loading-indicator';
        bubble.innerHTML = '<span class="dot"></span><span class="dot"></span><span class="dot"></span>';
        bubble.setAttribute('aria-label', 'Processando...');
        
        messageContent.appendChild(bubble);
        wrapper.appendChild(avatar);
        wrapper.appendChild(messageContent);
        chatLog.appendChild(wrapper);
        
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
     */
    async function sendMessage(text) {
        const sessionId = await getOrCreateSessionId();
        const startTime = Date.now();
        
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
        
        const latency = Date.now() - startTime;
        updateLatency(latency);
        
        if (!response.ok) {
            updateStatus(false, 'Erro de conexão');
            if (response.status === 429) {
                throw new Error('Muitas requisições. Aguarde um momento.');
            }
            if (response.status === 422) {
                const errorData = await response.json();
                throw new Error('Mensagem inválida: ' + (errorData.detail?.[0]?.msg || 'erro de validação'));
            }
            throw new Error(`Erro do servidor (${response.status})`);
        }
        
        updateStatus(true);
        return await response.json();
    }

    // --- RENDERIZAÇÃO DE RESPOSTAS ---

    /**
     * Renderiza resposta do backend baseado no tipo.
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
                renderTextResponse(typeof content === 'string' ? content : JSON.stringify(content));
        }
        
        if (suggestions && suggestions.length > 0 && type !== 'error' && type !== 'clarification') {
            renderSuggestions(suggestions);
        }
    }

    /**
     * Renderiza resposta de texto com markdown.
     */
    function renderTextResponse(content) {
        const wrapper = createMessageWrapper('bot');
        const messageText = wrapper.querySelector('.message-text');
        
        // Renderizar markdown
        if (typeof marked !== 'undefined' && typeof DOMPurify !== 'undefined') {
            const rawHtml = marked.parse(content);
            const cleanHtml = DOMPurify.sanitize(rawHtml);
            messageText.innerHTML = cleanHtml;
        } else {
            messageText.textContent = content;
        }
        
        chatLog.appendChild(wrapper);
        addCopyButton(wrapper);
        scrollToBottom();
    }

    /**
     * Renderiza resposta de lista.
     */
    function renderListResponse(content, metadata) {
        const wrapper = createMessageWrapper('bot');
        const messageText = wrapper.querySelector('.message-text');
        
        let textContent = '';
        
        if (Array.isArray(content)) {
            const items = content.slice(0, 20);
            
            if (items.length > 0 && typeof items[0] === 'object') {
                if ('titulo' in items[0]) {
                    textContent = `**Encontrados ${content.length} resultado(s):**\n\n`;
                    textContent += items.map(item => `• ${item.titulo}`).join('\n');
                    if (content.length > 20) {
                        textContent += `\n\n*... e mais ${content.length - 20} resultados*`;
                    }
                } else if ('nome' in items[0]) {
                    textContent = items.map(g => capitalize(g.nome)).join(', ');
                } else {
                    textContent = JSON.stringify(content);
                }
            } else {
                textContent = items.join(', ');
            }
        } else if (typeof content === 'object') {
            if ('genero' in content && 'contagem' in content) {
                textContent = `📊 Encontrei **${content.contagem}** filmes`;
            } else {
                textContent = JSON.stringify(content);
            }
        } else {
            textContent = String(content);
        }
        
        // Renderizar markdown
        if (typeof marked !== 'undefined' && typeof DOMPurify !== 'undefined') {
            const rawHtml = marked.parse(textContent);
            const cleanHtml = DOMPurify.sanitize(rawHtml);
            messageText.innerHTML = cleanHtml;
        } else {
            messageText.textContent = textContent;
        }
        
        chatLog.appendChild(wrapper);
        addCopyButton(wrapper);
        scrollToBottom();
    }

    /**
     * Renderiza resposta de erro.
     */
    function renderErrorResponse(content, suggestions) {
        const wrapper = createMessageWrapper('bot');
        const bubble = wrapper.querySelector('.message-bubble');
        const messageText = wrapper.querySelector('.message-text');
        
        bubble.style.background = 'rgba(239, 68, 68, 0.15)';
        bubble.style.border = '1px solid rgba(239, 68, 68, 0.3)';
        
        let textContent = `⚠️ ${content}`;
        
        if (suggestions && suggestions.length > 0) {
            textContent += '\n\n**Sugestões:**\n';
            textContent += suggestions.map(s => `• ${s}`).join('\n');
        }
        
        if (typeof marked !== 'undefined' && typeof DOMPurify !== 'undefined') {
            const rawHtml = marked.parse(textContent);
            const cleanHtml = DOMPurify.sanitize(rawHtml);
            messageText.innerHTML = cleanHtml;
        } else {
            messageText.textContent = textContent;
        }
        
        chatLog.appendChild(wrapper);
        scrollToBottom();
    }

    /**
     * Renderiza resposta de ajuda.
     */
    function renderHelpResponse(content) {
        const wrapper = createMessageWrapper('bot');
        const messageText = wrapper.querySelector('.message-text');
        
        let textContent = '';
        
        if (typeof content === 'object' && content.message && content.examples) {
            textContent = `${content.message}\n\n`;
            
            if (Array.isArray(content.examples)) {
                content.examples.forEach(ex => {
                    textContent += `• \`${ex}\`\n`;
                });
            } else if (typeof content.examples === 'object') {
                for (const [category, examples] of Object.entries(content.examples)) {
                    textContent += `**${category}:**\n`;
                    if (Array.isArray(examples)) {
                        examples.forEach(ex => {
                            textContent += `• \`${ex}\`\n`;
                        });
                    }
                    textContent += '\n';
                }
            }
        } else {
            textContent = typeof content === 'string' ? content : JSON.stringify(content);
        }
        
        if (typeof marked !== 'undefined' && typeof DOMPurify !== 'undefined') {
            const rawHtml = marked.parse(textContent);
            const cleanHtml = DOMPurify.sanitize(rawHtml);
            messageText.innerHTML = cleanHtml;
        } else {
            messageText.textContent = textContent;
        }
        
        chatLog.appendChild(wrapper);
        addCopyButton(wrapper);
        addClickableCodeListeners(wrapper);
        scrollToBottom();
    }

    /**
     * Renderiza resposta de clarificação.
     */
    function renderClarificationResponse(content, suggestions) {
        const wrapper = createMessageWrapper('bot');
        const messageText = wrapper.querySelector('.message-text');
        
        let textContent = `${content}\n\n`;
        
        if (suggestions && suggestions.length > 0) {
            textContent += '**Você quis dizer:**\n';
            suggestions.forEach(s => {
                textContent += `• ${s}\n`;
            });
        }
        
        if (typeof marked !== 'undefined' && typeof DOMPurify !== 'undefined') {
            const rawHtml = marked.parse(textContent);
            const cleanHtml = DOMPurify.sanitize(rawHtml);
            messageText.innerHTML = cleanHtml;
        } else {
            messageText.textContent = textContent;
        }
        
        chatLog.appendChild(wrapper);
        scrollToBottom();
        
        if (suggestions && suggestions.length > 0) {
            renderSuggestions(suggestions);
        }
    }

    /**
     * Cria estrutura base de mensagem.
     */
    function createMessageWrapper(sender) {
        const wrapper = document.createElement('div');
        wrapper.className = `message-wrapper ${sender}-wrapper`;
        
        const avatar = document.createElement('div');
        avatar.className = `avatar ${sender}-avatar`;
        avatar.textContent = sender === 'bot' ? '🤖' : '👤';
        
        const messageContent = document.createElement('div');
        messageContent.className = 'message-content';
        
        const bubble = document.createElement('div');
        bubble.className = `message-bubble ${sender}-bubble`;
        
        const messageText = document.createElement('div');
        messageText.className = 'message-text';
        
        bubble.appendChild(messageText);
        messageContent.appendChild(bubble);
        wrapper.appendChild(avatar);
        wrapper.appendChild(messageContent);
        
        return wrapper;
    }

    /**
     * Adiciona botão de copiar à mensagem do bot.
     */
    function addCopyButton(wrapper) {
        const messageContent = wrapper.querySelector('.message-content');
        const messageText = wrapper.querySelector('.message-text');
        
        if (!messageContent || !messageText) {
            console.warn('[Copy] Elementos não encontrados para adicionar botão de copiar');
            return;
        }
        
        const copyBtn = document.createElement('button');
        copyBtn.className = 'copy-btn';
        copyBtn.innerHTML = '📋 Copiar';
        copyBtn.setAttribute('aria-label', 'Copiar mensagem');
        
        copyBtn.addEventListener('click', async () => {
            const text = messageText.textContent || messageText.innerText;
            
            try {
                // Verifica se a API Clipboard está disponível
                if (navigator.clipboard && navigator.clipboard.writeText) {
                    await navigator.clipboard.writeText(text);
                } else {
                    // Fallback para navegadores antigos
                    const textarea = document.createElement('textarea');
                    textarea.value = text;
                    textarea.style.position = 'fixed';
                    textarea.style.opacity = '0';
                    document.body.appendChild(textarea);
                    textarea.select();
                    document.execCommand('copy');
                    document.body.removeChild(textarea);
                }
                
                copyBtn.innerHTML = '✓ Copiado';
                copyBtn.classList.add('copied');
                
                setTimeout(() => {
                    copyBtn.innerHTML = '📋 Copiar';
                    copyBtn.classList.remove('copied');
                }, 2000);
            } catch (err) {
                console.error('Erro ao copiar:', err);
                copyBtn.innerHTML = '✗ Erro';
                setTimeout(() => {
                    copyBtn.innerHTML = '📋 Copiar';
                }, 2000);
            }
        });
        
        messageContent.appendChild(copyBtn);
    }

    /**
     * Renderiza sugestões clicáveis.
     */
    function renderSuggestions(suggestions) {
        const wrapper = document.createElement('div');
        wrapper.className = 'message-wrapper bot-wrapper';
        
        const suggestionsDiv = document.createElement('div');
        suggestionsDiv.className = 'suggestions-container';
        
        suggestions.forEach(s => {
            const btn = document.createElement('button');
            btn.className = 'suggestion-btn';
            btn.textContent = s;
            btn.dataset.query = s;
            suggestionsDiv.appendChild(btn);
        });
        
        wrapper.appendChild(document.createElement('div')); // Spacer for avatar
        wrapper.appendChild(suggestionsDiv);
        
        chatLog.appendChild(wrapper);
        scrollToBottom();
        
        addClickableListeners(wrapper);
    }

    /**
     * Adiciona listeners para elementos clicáveis.
     */
    function addClickableListeners(container) {
        container.querySelectorAll('.suggestion-btn').forEach(el => {
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
     * Adiciona listeners para código clicável em ajuda.
     */
    function addClickableCodeListeners(container) {
        container.querySelectorAll('code').forEach(el => {
            el.classList.add('suggestion-clickable');
            el.style.cursor = 'pointer';
            el.addEventListener('click', async () => {
                const query = el.textContent;
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
        const wrapper = createMessageWrapper('user');
        const messageText = wrapper.querySelector('.message-text');
        messageText.textContent = text;
        chatLog.appendChild(wrapper);
        scrollToBottom();
    }

    /**
     * Mostra mensagem do sistema.
     */
    function displaySystemMessage(text) {
        const wrapper = document.createElement('div');
        wrapper.className = 'message-wrapper';
        
        const messageDiv = document.createElement('div');
        messageDiv.className = 'message-bubble';
        messageDiv.style.background = 'transparent';
        messageDiv.style.border = 'none';
        messageDiv.style.color = 'var(--text-muted)';
        messageDiv.style.fontSize = '0.85rem';
        messageDiv.style.fontStyle = 'italic';
        messageDiv.style.textAlign = 'center';
        messageDiv.style.alignSelf = 'center';
        messageDiv.style.maxWidth = '100%';
        messageDiv.textContent = text;
        
        wrapper.appendChild(messageDiv);
        chatLog.appendChild(wrapper);
        scrollToBottom();
    }

    // --- UTILIDADES ---

    function scrollToBottom() {
        if (!isUserScrolling) {
            chatWindow.scrollTo({
                top: chatWindow.scrollHeight,
                behavior: 'smooth'
            });
        }
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
        
        displayUserMessage(text);
        userInput.value = '';
        userInput.disabled = true;
        sendButton.disabled = true;
        
        showLoadingIndicator();
        
        try {
            const response = await sendMessage(text);
            hideLoadingIndicator();
            renderResponse(response);
        } catch (error) {
            console.error('[Error]', error);
            hideLoadingIndicator();
            renderErrorResponse(
                error.message || 'Desculpe, ocorreu um erro ao processar sua mensagem.',
                ['Tente novamente', 'Digite "ajuda" para ver comandos']
            );
        } finally {
            userInput.disabled = false;
            sendButton.disabled = false;
            userInput.focus();
        }
    }

    // --- QUICK ACTIONS ---
    
    /**
     * Configura quick actions.
     */
    function setupQuickActions() {
        quickActionsContainer.querySelectorAll('.quick-action-btn').forEach(btn => {
            btn.addEventListener('click', async () => {
                const query = btn.dataset.query;
                if (query) {
                    userInput.value = query;
                    await handleSendMessage();
                }
            });
        });
    }

    // --- INICIALIZAÇÃO ---

    sendButton.addEventListener('click', handleSendMessage);
    
    userInput.addEventListener('keydown', (ev) => {
        if (ev.key === 'Enter' && !ev.shiftKey) {
            ev.preventDefault();
            handleSendMessage();
        }
    });

    if (clearButton) {
        clearButton.addEventListener('click', clearSession);
    }

    setupQuickActions();
    getOrCreateSessionId();
    userInput.focus();
});
