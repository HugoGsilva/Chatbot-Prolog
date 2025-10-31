document.addEventListener('DOMContentLoaded', () => {
  const chatLog = document.getElementById('chat-log');
  const userInput = document.getElementById('user-input');
  const sendButton = document.getElementById('send-button');

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