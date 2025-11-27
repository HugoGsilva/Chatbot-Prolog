# 🤖 Chatbot Sakila-Prolog

Este projeto é um chatbot completo capaz de responder a perguntas sobre a base de dados de filmes Sakila. Utiliza uma arquitetura híbrida que combina a lógica de inferência do **SWI-Prolog** com um backend **FastAPI** (Python), um frontend **JavaScript** e **Redis** para gestão de sessões.

O chatbot implementa NLU (Processamento de Linguagem Natural) em dois níveis:
- **Nível 1 (Frontend):** Um router de intenção com tolerância a erros de digitação (via `Fuse.js`) para reconhecer intenções mesmo com typos, por exemplo: "flmes por...".
- **Nível 2 (Backend):** Um resolvedor de entidades (via `thefuzz`/similar) que corrige typos nas entidades e suporta sinónimos, por exemplo: "penlope" → "PENELOPE GUINESS" e "acao" → "Action".

---

## 🏛️ Arquitetura

O sistema é orquestrado com `docker-compose` e utiliza 4 serviços principais:

- `mysql`: Servidor MySQL 8.0 que armazena os dados do Sakila.
- `db-init`: Serviço one-shot que espera o `mysql` ficar saudável e depois executa os scripts `.sql` para criar o schema e popular os dados.
- `redis`: Broker/cache para armazenar o histórico de conversas (sessões) do chatbot.
- `app` (Core): Aplicação principal (Python/FastAPI) que:
  - Serve o frontend (`frontend/index.html`, `frontend/style.css`, `frontend/main.js`).
  - Expõe a API REST (por exemplo, `/filmes-por-ator`, `/filmes-por-genero`, `/genero-do-filme`, `/recomendar-por-ator`).
  - Persiste o histórico de sessão no `redis`.
  - No startup (via `lifespan`):
    1. Conecta ao Redis e carrega as caches de NLU pré-calculadas.
    2. Inicia o motor SWI-Prolog (via `pyswip`).
    3. Carrega as regras (`prolog/rules/inferencia.pl`) e os factos (`prolog/knowledge/imdb_kb.pl`).
    4. Inicia o servidor Uvicorn na porta 8000.

Notas de logs:
- Os logs de arranque confirmam caches e ligação ao Redis; o serviço `app` está configurado com `PYTHONUNBUFFERED=1` para evitar buffering e mostrar mensagens em tempo real.

---

## 🚀 Como Executar (Deployment)

Este projeto está preparado para "one-click setup" usando Docker Compose.

**Pré-requisitos:**
- Docker
- Docker Compose

### 1) Iniciar o Ambiente (Produção)

Este comando constrói a imagem (Dockerfile multi-stage) e inicia os 4 serviços em segundo plano.

```bash
docker compose up --build -d
```

### 2) Aceder ao Chatbot

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

## ⚙️ Comandos de Exemplo (NLU Híbrido)

O bot entende os seguintes padrões (e variações com typos):

- `filmes por [ATOR]` (ex.: `flmes por penlope`)
- `recomendar por [ATOR]` (ex.: `recomendação do penelope`)
- `filmes de [GENERO]` (ex.: `filmes de acao`, `flmes de actn`)
- `genero do [FILME]` (ex.: `genero de acdemy dinossaur`)
- `contar filmes de [GENERO] em [ANO]` (ex.: `contar flmes de comdy em 2006`)

---

## 📂 Estrutura de Pastas (Resumo)

- `app/` — FastAPI e serviços (NLU, sessão, Prolog service).
- `frontend/` — `index.html`, `main.js`, `style.css` do chatbot.
- `prolog/` — Regras (`rules/inferencia.pl`) e conhecimento (`knowledge/imdb_kb.pl`).
- `data_netflix/` — Pipeline ETL (CSV → MySQL → Prolog → Redis).
- `cypress/` — Testes E2E.
- `cypress/` — Testes E2E.
- `docker-compose.yml` — Orquestração de serviços.

---

## ✅ Estado

Todas as fases (1–4: implementação; 5: empacotamento) estão concluídas e validadas. Esta documentação finaliza a Etapa 7 da Fase 5.

---

## 🗒️ Notas

- A API FastAPI é servida no mesmo host que o frontend (porta `8000`).
- O resolvedor de Nível 2 aceita português e inglês para géneros, realizando a tradução automática para o formato esperado pelo Prolog.
- Logs de arranque mostram Uvicorn e carregamento das caches; caso necessário, utilize `docker compose logs -f app`.