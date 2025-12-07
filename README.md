# 🤖 Chatbot Netflix-Prolog

Este projeto é um chatbot completo capaz de responder a perguntas sobre a base de dados de filmes Netflix. Utiliza uma arquitetura **Thin Client** que combina a lógica de inferência do **SWI-Prolog** com um backend **FastAPI** (Python), um frontend **Angular** moderno e **Redis** para gestão de sessões.

## 🏗️ Arquitetura Thin Client

O chatbot implementa uma arquitetura moderna de **Thin Client** onde todo o processamento de linguagem natural (NLU) é realizado no servidor:

```
┌─────────────────────────────────────────────────────────────────┐
│                    FRONTEND (Angular 17)                        │
│   • UI moderna com componentes standalone                      │
│   • Envia texto bruto para POST /chat                          │
│   • Renderiza respostas por tipo (text, list, error, help)     │
│   • Gerencia sessão via SessionService + localStorage          │
│   • Tema escuro/claro com ThemeService                         │
│   • Markdown rendering, rate limit countdown, animations       │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     BACKEND (FastAPI)                           │
│   • Toda lógica NLU/intent recognition                         │
│   • Correção ortográfica (SymSpell, 133k termos)               │
│   • Fuzzy matching para entidades (thefuzz)                    │
│   • Processamento Prolog                                       │
│   • Rate limiting (IP: 20/min, Session: 10/min)                │
│   • Gerenciamento de sessões (Redis, TTL 24h)                  │
└─────────────────────────────────────────────────────────────────┘
```

### Vantagens da Arquitetura Thin Client:
- **Segurança**: Lógica de negócio protegida no servidor
- **Manutenibilidade**: Atualizações de NLU não requerem deploy de frontend
- **Performance**: Frontend otimizado com build Angular
- **Consistência**: Mesma lógica para todos os clientes
- **Type Safety**: TypeScript no frontend para melhor manutenção

---

## 🏛️ Serviços Docker

O sistema é orquestrado com `docker-compose` e utiliza 5 serviços principais:

- `mysql`: Servidor MySQL 8.0 que armazena os dados do catálogo Netflix.
- `db-init`: Serviço one-shot que espera o `mysql` ficar saudável e depois executa os scripts `.sql` para criar o schema e popular os dados.
- `redis`: Broker/cache para armazenar o histórico de conversas (sessões) do chatbot.
- `app` (Backend): Aplicação principal (Python/FastAPI) que:
  - Expõe o endpoint unificado `POST /chat` para todas as interações.
  - Implementa NLU completo: intent detection, entity extraction, spell correction.
  - Persiste o histórico de sessão no `redis` (TTL 24h).
  - Aplica rate limiting por IP (20/min) e por sessão (10/min).
  - No startup (via `lifespan`):
    1. Conecta ao Redis e carrega as caches de NLU pré-calculadas.
    2. Inicializa o SpellCorrector com vocabulário de 133k+ termos.
    3. Inicia o motor SWI-Prolog (via `pyswip`).
    4. Carrega as regras (`prolog/rules/inferencia.pl`) e os factos (`prolog/knowledge/imdb_kb.pl`).
    5. Inicia o servidor Uvicorn na porta 8000.
- `frontend` (Angular): Aplicação Angular 17 servida via Nginx que:
  - UI moderna com tema escuro/claro
  - Componentes standalone (sem NgModule)
  - TypeScript para type safety
  - Markdown rendering, copy button, rate limit countdown
  - Build otimizado (~500KB gzip)
  - Servido na porta 80

Notas de logs:
- Os logs de arranque confirmam caches e ligação ao Redis; o serviço `app` está configurado com `PYTHONUNBUFFERED=1` para evitar buffering e mostrar mensagens em tempo real.

---

## 🚀 Como Executar (Deployment)

Este projeto está preparado para "one-click setup" usando Docker Compose.

**Pré-requisitos:**
- Docker
- Docker Compose

### 1) Iniciar o Ambiente (Produção)

### 2) Aceder ao Chatbot

Após os serviços arrancarem (MySQL saudável, caches carregadas), abra no navegador:

```
http://localhost
```

**Portas disponíveis:**
- Frontend Angular: http://localhost (porta 80)
- Backend API: http://localhost:8000
- API Docs: http://localhost:8000/docs 2) Aceder ao Chatbot

Após os serviços arrancarem (MySQL saudável, caches carregadas), abra no navegador:

```
http://localhost:8000
```

### 3) Consultar Logs (opcional)

Para acompanhar o arranque da aplicação e ver o carregamento de caches:

```bash
docker compose logs -f app
```

### 4) Parar o Ambiente

Para parar e remover os contêineres:

```bash
docker compose down
```

---

## 🧪 Como Executar os Testes

Este projeto tem 3 níveis de testes.

### Testes Unitários (Prolog)

```bash
docker compose run --rm app swipl -q -s tests/unit/test_inferencia.pl -g run_tests -t halt
```

### Testes Unitários + Integração (Python/Backend)

```bash
docker compose run --rm app python -m pytest
```

### Testes End-to-End (Frontend - Cypress)

Certifique-se de que o ambiente está a correr (`docker compose up -d`). Na raiz do projeto:

```bash
npm install
npx cypress run
```

Para depuração interativa:

```bash
npx cypress open
```

---

## 📡 API Endpoints

### Endpoint Principal

#### `POST /chat`
Endpoint unificado para todas as interações do chatbot.

**Request:**
```json
{
  "message": "filmes de ação",
  "session_id": "uuid-da-sessao"
}
```

**Response:**
```json
{
  "type": "list",
  "content": [
    {"titulo": "Die Hard", "ano": 1988},
    {"titulo": "The Matrix", "ano": 1999}
  ],
  "suggestions": ["filmes de comédia", "filmes de drama"],
  "metadata": {
    "intent": "filmes_por_genero",
    "confidence": 0.95,
    "processing_time_ms": 150
  }
}
```

**Tipos de Resposta:**
- `text`: Mensagem simples de texto
- `list`: Lista de resultados (filmes, atores, etc.)
- `error`: Erro com sugestões de correção
- `help`: Ajuda com exemplos de uso
- `clarification`: Pedido de clarificação (baixa confiança)

### Endpoints de Sessão

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| `POST` | `/session/create` | Cria nova sessão no servidor |
| `GET` | `/session/{id}/history` | Obtém histórico da sessão |
| `DELETE` | `/session/{id}` | Encerra sessão |

### Rate Limiting

- **Por IP**: 20 requisições/minuto
- **Por Sessão**: 10 requisições/minuto
- Retorna `429 Too Many Requests` quando excedido

---

## ⚙️ Comandos de Exemplo

O bot entende os seguintes padrões em linguagem natural (com tolerância a erros de digitação):

| Intenção | Exemplos |
|----------|----------|
| Filmes por gênero | `filmes de ação`, `filmes de comédia`, `movies de drama` |
| Filmes por ator | `filmes do ator Tom Hanks`, `filmes com Adam Sandler` |
| Filmes por diretor | `filmes do diretor Steven Spielberg` |
| Gênero de um filme | `gênero do filme Matrix`, `qual o gênero de Titanic` |
| Recomendação | `recomende um filme de terror`, `sugira drama` |
| Ajuda | `ajuda`, `help`, `o que você pode fazer` |
| Saudação | `olá`, `oi`, `bom dia` |

**Recursos de NLU:**
- ✅ Correção ortográfica automática (SymSpell)
- ✅ Fuzzy matching para nomes (thefuzz, 85% similaridade)
- ✅ Tradução de gêneros PT↔EN
- ✅ Normalização de capitalização
- ✅ Detecção de confiança (0.0-1.0)

---

## 📂 Estrutura de Pastas

```
├── app/                    # Backend FastAPI
│   ├── main.py            # Endpoints e lifespan
│   ├── nlu_engine.py      # Motor de NLU
│   ├── intent_router.py   # Roteador de intenções
│   ├── response_formatter.py  # Formatador de respostas
│   ├── spell_corrector.py # Correção ortográfica
│   ├── rate_limiter.py    # Rate limiting
│   ├── session_manager.py # Gerenciamento de sessões
│   ├── prolog_service.py  # Interface com Prolog
│   └── schemas.py         # Modelos Pydantic
├── frontend/              # Frontend Thin Client
│   ├── index.html         # UI do chatbot
│   ├── main.js           # Cliente JavaScript
│   └── style.css         # Estilos
├── prolog/               # Lógica Prolog
│   ├── rules/inferencia.pl    # Regras de inferência
│   └── knowledge/imdb_kb.pl   # Base de conhecimento
├── cypress/              # Testes E2E
│   └── e2e/
│       ├── thin_client.cy.js      # Testes do frontend
│       ├── integration_tests.cy.js # Testes de integração
│       └── performance_tests.cy.js # Testes de performance
├── tests/                # Testes Python
├── data_netflix/         # Pipeline ETL
├── docker-compose.yml    # Orquestração
└── Dockerfile           # Build da aplicação
```

---

## 🧪 Testes

O projeto possui 3 níveis de testes com **57+ testes Cypress** e testes Python.

### Testes E2E (Cypress)

```bash
# Ambiente deve estar rodando
docker compose up -d

# Instalar dependências
npm install

# Executar todos os testes
npx cypress run

# Modo interativo
npx cypress open
```

**Suites de Teste:**
- `thin_client.cy.js` - 14 testes (arquitetura thin client)
- `integration_tests.cy.js` - 33 testes (todos os intents, erros, sessões)
- `performance_tests.cy.js` - 10 testes (tempos de resposta, concorrência)

### Testes Python

```bash
docker compose run --rm app python -m pytest tests/ -v
```

---

## ✅ Estado

| Fase | Descrição | Status |
|------|-----------|--------|
| 1 | Backend Foundation | ✅ Completa |
| 2 | Unified Chat Endpoint | ✅ Completa |
| 3 | Frontend Migration (Thin Client) | ✅ Completa |
| 4 | Integration and Testing | ✅ Completa |
| 5 | Cleanup and Deployment | 🔄 Em progresso |

---

## 🗒️ Notas Técnicas

- **API**: FastAPI servida na porta `8000` (mesmo host que frontend)
- **Sessões**: TTL de 24 horas, armazenadas no Redis
- **Rate Limiting**: Sliding window algorithm via Redis sorted sets
- **NLU**: SymSpell para correção (< 5ms), thefuzz para fuzzy matching (85% threshold)
- **Prolog**: Timeout de 2s para queries, execução via ThreadPoolExecutor
- **Gêneros**: Suporte a português e inglês com tradução automática