document.addEventListener('DOMContentLoaded', () => {
    // Referências DOM (IDs ANTIGOS, que funcionam com o HTML do Prompt 1)
    const chatLog = document.getElementById('chat-log');
    const userInput = document.getElementById('user-input');
    const sendButton = document.getElementById('send-button');
    const chatWindow = document.getElementById('chat-window'); // Para o scroll

    // --- BANCO DE INTENÇÕES (Híbrido - Completo) ---
    const intentPrototypes = [
        // Novas intenções compostas (V2) — posicionadas antes das genéricas
        {
            phrase: "filme de GENERO com ATOR",
            // RegEx solto: procura "...de [genero] com [ator]" (com "ator/atriz" opcional)
            regex: /de\s+(.+)\s+com\s+(?:(ator|atriz)\s+)?(.+)$/i,
            // Template para o novo endpoint V2
            template: (matches) => `/recomendar/ator-e-genero?genero=${encodeURIComponent(matches[1])}&ator=${encodeURIComponent(matches[3])}`
        },
        {
            phrase: "filme de GENERO e GENERO",
            // RegEx solto: procura "...de [genero1] e [genero2]"
            regex: /de\s+(.+)\s+e\s+(.+)$/i,
            // Template para o novo endpoint V2
            template: (matches) => `/recomendar/dois-generos?genero1=${encodeURIComponent(matches[1])}&genero2=${encodeURIComponent(matches[2])}`
        },
        {
            phrase: "contar filmes de GENERO em ANO",
            regex: /de\s+(.+)\s+em\s+(\d{4})$/i,
            template: (matches) => `/contar-filmes?genero=${encodeURIComponent(matches[1])}&ano=${matches[2]}`
        },
        {
            phrase: "recomendar com base em ATOR",
            regex: /(com\s+base\s+em|baseado\s+em|do|de|pelo|da)\s+(?:(ator|atriz)\s+)?(.+)$/i,
            template: (matches) => `/recomendar-por-ator/${encodeURIComponent(matches[3])}`
        },
        {
            phrase: "filmes por ATOR",
            regex: /(por|do|de|pelo|da)\s+(?:(ator|atriz)\s+)?(.+)$/i,
            template: (matches) => `/filmes-por-ator/${encodeURIComponent(matches[3])}` 
        },
        {
            phrase: "filme com ator ATOR",
            regex: /com\s+(ator|atriz)\s+(.+)$/i,
            template: (matches) => `/filmes-por-ator/${encodeURIComponent(matches[2])}`
        },
        {
            phrase: "gênero do FILME",
            regex: /(do|de)\s+(.+)$/i,
            template: (matches) => `/genero-do-filme/${encodeURIComponent(matches[2])}`
        },
        {
            phrase: "filmes de GENERO",
            regex: /de\s+(?!ator\s|atriz\s)(.+)$/i,
            template: (matches) => `/filmes-por-genero/${encodeURIComponent(matches[1])}`
        }
    ];

    // --- INICIALIZAR O FUSE.JS ---
    const fuseOptions = {
      keys: ['phrase'],
      threshold: 0.6 // Nível de tolerância
    };
    const fuse = new Fuse(intentPrototypes, fuseOptions);

    // --- GESTÃO DE SESSÃO ---
    function generateSessionId() {
      return 'sess_' + Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
    }
    const SESSION_ID = generateSessionId();

    // --- FUNÇÃO DE DISPLAY (AJUSTADA PARA O NOVO CSS) ---
    function displayMessage(sender, message) {
        // CRIA O <p> (Mapeado para .chat-message)
        const p = document.createElement('p');
        
        // Adiciona as classes do NOVO CSS
        p.classList.add('chat-message');
        p.classList.add(sender === 'User' ? 'user-message' : 'bot-message');

        // Formata a resposta (JSON vs. Texto)
        let textContent;
        if (typeof message === 'object' && message !== null) {
            if (message.detail) {
                textContent = `Desculpe: ${message.detail}`; // Erro FastAPI
            } else if (Array.isArray(message) && message.length > 0 && 'titulo' in message[0]) {
                textContent = message.map(m => m.titulo).join(', '); // Lista de filmes
            } else if ('nome' in message) {
                textContent = message.nome; // Género
            } else if ('genero' in message && 'contagem' in message) {
                textContent = `Contagem (${message.genero}, ${message.ano}): ${message.contagem}`; // Contagem
            } else {
                textContent = JSON.stringify(message);
            }
        } else {
            textContent = String(message); // Texto normal
        }
        
        // Trata o erro 404 que vem como string
        if (sender === 'Bot' && textContent.includes('{"detail":')) {
            try {
                const errJson = JSON.parse(textContent);
                textContent = `Desculpe: ${errJson.detail}`;
            } catch(e) { /* ignora */ }
        }

        // Adiciona o texto ao <p>
        p.textContent = textContent; // (O seu CSS novo não usa <strong>)
        chatLog.appendChild(p);
        
        // Faz o scroll da janela correta
        chatWindow.scrollTop = chatWindow.scrollHeight;
    }

    // --- FUNÇÃO DE ROTEAMENTO (Inalterada) ---
    function routeIntent(message) {
        const msg = message.trim();
        const results = fuse.search(msg);
        if (!results.length) return null;
        const bestIntent = results[0].item;
        const matches = msg.match(bestIntent.regex);
        if (!matches) return null;
        return bestIntent.template(matches);
    }

    // --- FUNÇÃO DE ENVIO (Inalterada) ---
    async function handleSendMessage() {
        const text = userInput.value.trim();
        if (!text) return;

        displayMessage('User', text);
        userInput.value = '';

        let url = routeIntent(text);

        if (!url) {
            displayMessage('Bot', 'Não entendi. Tente: "filmes por [ATOR]"');
            return;
        }
        // Anexa session_id de forma robusta (suporta URLs já com query)
        url += (url.includes('?') ? `&session_id=${SESSION_ID}` : `?session_id=${SESSION_ID}`);

        try {
            const resp = await fetch(url);
            if (!resp.ok) {
                // Lógica de erro limpa (do prompt anterior)
                let errorMsg = `Erro ${resp.status}.`;
                const errText = await resp.text();
                try {
                    const errJson = JSON.parse(errText);
                    if (errJson && errJson.detail) {
                        errorMsg = `Desculpe: ${errJson.detail}`;
                    } else {
                        errorMsg = errText;
                    }
                } catch (e) {
                    errorMsg = `Ocorreu um erro no servidor (Erro ${resp.status}).`;
                }
                displayMessage('Bot', errorMsg);
                return;
            }
            const data = await resp.json();
            displayMessage('Bot', data);
        } catch (e) {
            displayMessage('Bot', 'Desculpe, ocorreu um erro.');
        }
    }

    // --- LISTENERS (OS ANTIGOS, SEM <form>) ---
    // Estes listeners funcionam com o HTML do Prompt 1
    sendButton.addEventListener('click', handleSendMessage);
    userInput.addEventListener('keydown', (ev) => {
        if (ev.key === 'Enter') {
            ev.preventDefault(); // Evita qualquer comportamento padrão de "Enter"
            handleSendMessage();
        }
    });
});