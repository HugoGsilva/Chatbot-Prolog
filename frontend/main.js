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
      // Procura por "de [genero] em [ano]" (permite "contar flmes de...")
      regex: /de\s+(.+)\s+em\s+(\d{4})$/i,
      template: (matches) => `/contar-filmes?genero=${encodeURIComponent(matches[1])}&ano=${matches[2]}`
    },
    {
      phrase: "recomendar com base em ATOR",
      // Este RegEx aceita várias formas de pedir recomendação
      regex: /(com\s+base\s+em|baseado\s+em|do|de|pelo)\s+(.+)$/i,
      template: (matches) => `/recomendar-por-ator/${encodeURIComponent(matches[2])}`
    },
    {
      phrase: "filmes por ATOR",
      // Este RegEx aceita "filmes por/do/de/pelo" e "filme" (singular)
      regex: /(por|do|de|pelo)\s+(.+)$/i,
      template: (matches) => `/filmes-por-ator/${encodeURIComponent(matches[2])}`
    },
    {
      phrase: "gênero do FILME",
      // Procura por "...do/de [filme]"
      regex: /(do|de)\s+(.+)$/i,
      template: (matches) => `/genero-do-filme/${encodeURIComponent(matches[2])}`
    },
    {
      phrase: "filmes de GENERO",
      // Este RegEx aceita "filme de" (singular)
      // NOTA: É muito parecido com "filmes por ATOR", mas o fuzzy matching vai ajudar
      regex: /filmes?\s+de\s+(.+)$/i,
      template: (matches) => `/filmes-por-genero/${encodeURIComponent(matches[1])}`
    }
  ];

  // --- ETAPA 3: INICIALIZAR O FUSE.JS ---
  const fuseOptions = {
    keys: ['phrase'],  // Procurar apenas na chave "phrase"
    threshold: 0.6     // Nível de tolerância (0.0 = exato, 1.0 = tudo)
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

    // 1. FASE FUZZY (Nível 1): Encontrar a INTENÇÃO mais próxima
    const results = fuse.search(msg);

    if (!results.length) {
      return null; // Não encontrou nenhuma intenção provável
    }

    const bestIntent = results[0].item; // O protótipo vencedor

    // 2. FASE REGEX (Nível 1.5): Extrair as ENTIDADES
    const matches = msg.match(bestIntent.regex);

    if (!matches) {
      // A intenção era provável, mas a estrutura estava incompleta
      return null;
    }

    // 3. SUCESSO: Construir a URL da API
    const url = bestIntent.template(matches);

    return url; // Retorna a URL final
  }

  async function handleSendMessage() {
    const text = userInput.value.trim();
    if (!text) return;

    displayMessage('User', text);
    userInput.value = '';

    // 1. Obter a URL completa (já processada pelo Nível 1 Híbrido)
    let url = routeIntent(text);

    if (!url) {
      displayMessage('Bot', 'Não entendi. Tente: "filmes por PENELOPE GUINESS"');
      return;
    }

    // 2. Adicionar o session_id
    if (url.startsWith('/contar-filmes')) {
      // O /contar-filmes já tem ?genero=...&ano=...
      url += `&session_id=${SESSION_ID}`;
    } else {
      // Os outros endpoints (path params) precisam de um ?
      url += `?session_id=${SESSION_ID}`;
    }

    // 3. Fazer o Fetch (esta parte permanece igual)
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