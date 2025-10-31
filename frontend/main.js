document.addEventListener('DOMContentLoaded', () => {
  const chatLog = document.getElementById('chat-log');
  const userInput = document.getElementById('user-input');
  const sendButton = document.getElementById('send-button');

  // --- ETAPA 2: BANCO DE INTENÇÕES ---
  // Lista de todas as intenções que o bot conhece.
  // 'phrase' é o que o Fuse.js usa para o fuzzy match.
  // 'regex' é o que usamos para extrair a entidade DEPOIS que o match for encontrado.
  const intentPrototypes = [
    {
      phrase: "contar filmes de GENERO em ANO",
      regex: /^contar\s+filmes\s+de\s+(.+)\s+em\s+(\d{4})$/i,
      template: (matches) => `/contar-filmes?genero=${encodeURIComponent(matches[1])}&ano=${matches[2]}`
    },
    {
      phrase: "recomendar com base em ATOR",
      // Este RegEx aceita várias formas de pedir recomendação
      regex: /^(recomendar|recomendação)\s+(com\s+base\s+em|baseado\s+em|do|de|pelo)\s+(.+)$/i,
      template: (matches) => `/recomendar-por-ator/${encodeURIComponent(matches[3])}`
    },
    {
      phrase: "filmes por ATOR",
      // Este RegEx aceita "filmes por/do/de/pelo" e "filme" (singular)
      regex: /^filmes?\s+(por|do|de|pelo)\s+(.+)$/i,
      template: (matches) => `/filmes-por-ator/${encodeURIComponent(matches[2])}`
    },
    {
      phrase: "gênero do FILME",
      regex: /^(g[eê]nero)\s+(do|de)\s+(.+)$/i,
      template: (matches) => `/genero-do-filme/${encodeURIComponent(matches[3])}`
    },
    {
      phrase: "filmes de GENERO",
      // Este RegEx aceita "filme de" (singular)
      // NOTA: É muito parecido com "filmes por ATOR", mas o fuzzy matching vai ajudar
      regex: /^filmes?\s+de\s+(.+)$/i,
      template: (matches) => `/filmes-por-genero/${encodeURIComponent(matches[1])}`
    }
  ];

  // --- ETAPA 3: INICIALIZAR O FUSE.JS ---
  const fuseOptions = {
    keys: ['phrase'],  // Procurar apenas na chave "phrase"
    threshold: 0.5     // Nível de tolerância (0.0 = exato, 1.0 = tudo)
  };
  const fuse = new Fuse(intentPrototypes, fuseOptions);

  function generateSessionId() {
    return 'sess_' + Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
  }

  const SESSION_ID = generateSessionId();

  function displayMessage(sender, message) {
    const p = document.createElement('p');
    const strong = document.createElement('strong');
    strong.textContent = sender + ': ';
    p.appendChild(strong);

    let textContent;
    if (Array.isArray(message)) {
      if (message.length > 0 && typeof message[0] === 'object' && 'titulo' in message[0]) {
        textContent = message.map(m => m.titulo).join(', ');
      } else {
        textContent = JSON.stringify(message);
      }
    } else if (typeof message === 'object' && message !== null) {
      if ('nome' in message) {
        textContent = message.nome;
      } else if ('genero' in message && 'ano' in message && 'contagem' in message) {
        textContent = `Contagem (${message.genero}, ${message.ano}): ${message.contagem}`;
      } else {
        textContent = JSON.stringify(message);
      }
    } else {
      textContent = String(message);
    }

    p.appendChild(document.createTextNode(textContent));
    chatLog.appendChild(p);
    chatLog.scrollTop = chatLog.scrollHeight;
  }

  function routeIntent(message) {
    const msg = message.trim();

    // contar filmes de <genero> em <ano>
    let m = msg.match(/^contar\s+filmes\s+de\s+(.+)\s+em\s+(\d{4})$/i);
    if (m) {
      return { endpoint: '/contar-filmes', params: { genero: m[1], ano: m[2] } };
    }

    // recomendar com base em <ator>
    m = msg.match(/^recomendar\s+com\s+base\s+em\s+(.+)$/i);
    if (m) {
      return { endpoint: `/recomendar-por-ator/${encodeURIComponent(m[1])}`, params: {} };
    }

    // filmes por <ator>
    m = msg.match(/^filmes\s+por\s+(.+)$/i);
    if (m) {
      return { endpoint: `/filmes-por-ator/${encodeURIComponent(m[1])}`, params: {} };
    }

    // gênero/genero do <filme>
    m = msg.match(/^(g[eê]nero)\s+do\s+(.+)$/i);
    if (m) {
      return { endpoint: `/genero-do-filme/${encodeURIComponent(m[2])}`, params: {} };
    }

    // filmes de <genero>
    m = msg.match(/^filmes\s+de\s+(.+)$/i);
    if (m) {
      return { endpoint: `/filmes-por-genero/${encodeURIComponent(m[1])}`, params: {} };
    }

    return null;
  }

  async function handleSendMessage() {
    const text = userInput.value.trim();
    if (!text) return;

    displayMessage('User', text);
    userInput.value = '';

    const route = routeIntent(text);
    if (!route) {
      displayMessage('Bot', 'Não entendi. Tente: "filmes por PENELOPE GUINESS"');
      return;
    }

    let url = route.endpoint;
    if (url === '/contar-filmes') {
      const params = new URLSearchParams({ ...route.params, session_id: SESSION_ID });
      url = `${url}?${params.toString()}`;
    } else {
      const joiner = url.includes('?') ? '&' : '?';
      url = `${url}${joiner}session_id=${encodeURIComponent(SESSION_ID)}`;
    }

    try {
      const resp = await fetch(url);
      if (!resp.ok) {
        const errText = await resp.text();
        displayMessage('Bot', `Erro ${resp.status}: ${errText}`);
        return;
      }
      const data = await resp.json();
      displayMessage('Bot', data);
    } catch (e) {
      displayMessage('Bot', 'Desculpe, ocorreu um erro.');
    }
  }

  sendButton.addEventListener('click', handleSendMessage);
  userInput.addEventListener('keydown', (ev) => {
    if (ev.key === 'Enter') handleSendMessage();
  });
});