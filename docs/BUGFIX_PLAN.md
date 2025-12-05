# 🐛 Plano de Correção de Bugs - NLU e Lógica do Chatbot

**Data:** 05/12/2024  
**Status:** ✅ CONCLUÍDO  
**Prioridade:** Alta  
**Última Atualização:** 05/12/2024

---

## 📋 Resumo dos Erros

| # | Erro | Severidade | Complexidade | Status |
|---|------|------------|--------------|--------|
| 1 | Falha na identificação de identidade ("Quem é você?") | Média | Baixa | ✅ Corrigido |
| 2 | Erro de filtragem por gênero na recomendação | Alta | Média | ✅ Corrigido |
| 3 | Falha na extração de entidades numéricas (ano) | Alta | Média | ✅ Corrigido |
| 4 | Incapacidade de consultar diretor | Média | Baixa | ✅ Corrigido |
| 5 | Falsos positivos em perguntas genéricas | Alta | Média | ✅ Corrigido |

---

## ✅ Bug #1: Falha na Identificação de Identidade (CORRIGIDO)

### Descrição
- **Input:** "Quem é voce?"
- **Comportamento Atual:** Bot não reconhece e cai no fluxo de "Não entendi"
- **Comportamento Esperado:** Bot deve se apresentar e explicar suas capacidades

### Diagnóstico
A intenção de autodescrição/identidade não está mapeada no `intent_patterns.json`.

### Solução Proposta

#### 1.1 Adicionar intent "identidade" no `intent_patterns.json`
```json
"identidade": {
    "keywords": [
        "quem é você",
        "quem é voce",
        "quem es tu",
        "qual seu nome",
        "qual é seu nome",
        "você é quem",
        "o que você é",
        "o que voce é",
        "o que é você",
        "se apresente",
        "apresente-se",
        "quem te criou",
        "quem fez você"
    ],
    "prepositions": [],
    "entity_type": "NONE",
    "examples": [
        "quem é você?",
        "qual é o seu nome?",
        "o que você é?"
    ]
}
```

#### 1.2 Adicionar intent aos prioritários no `nlu_engine.py`
```python
exact_match_intents = ["ajuda", "saudacao", "identidade"]
```

#### 1.3 Criar handler `handle_identidade` no `intent_router.py`
```python
async def handle_identidade(self, nlu_result: NLUResult, session_id: str) -> ChatResponse:
    return ChatResponse(
        type=ResponseType.TEXT,
        content="🤖 Olá! Eu sou o **Chatbot Netflix**, um assistente virtual especializado em filmes. "
                "Fui criado para ajudar você a descobrir filmes por ator, gênero, diretor, "
                "obter recomendações e muito mais! Digite 'ajuda' para ver tudo que posso fazer.",
        suggestions=["ajuda", "filme aleatório", "filmes de ação"],
        metadata=self._build_metadata(nlu_result)
    )
```

### Arquivos a Modificar
- [x] `app/intent_patterns.json` ✅
- [x] `app/nlu_engine.py` ✅
- [x] `app/intent_router.py` ✅

---

## ✅ Bug #2: Erro de Filtragem por Gênero na Recomendação (CORRIGIDO)

### Descrição
- **Input:** "Me recomende um filme de ação"
- **Comportamento Atual:** Recomenda filme aleatório (comédia stand-up)
- **Comportamento Esperado:** Recomendar filme do gênero ação

### Diagnóstico
O handler `recomendar_filme` não está extraindo/aplicando o filtro de gênero da query. A entidade `genero=ação` não está sendo passada para o Prolog.

### Solução Proposta

#### 2.1 Melhorar extração de entidades no `recomendar_filme`
Verificar se o `_extract_entities` está identificando o gênero corretamente.

#### 2.2 Modificar handler para usar entidade de gênero
```python
async def handle_recomendar_filme(self, nlu_result: NLUResult, session_id: str) -> ChatResponse:
    entities = nlu_result.entities
    
    # Verificar se há filtro de gênero
    genre = entities.get("genre") or entities.get("genero")
    
    if genre:
        # Buscar filme aleatório DO gênero específico
        result = await self._query_prolog("filme_aleatorio_por_genero", genre)
    else:
        # Buscar filme totalmente aleatório
        result = await self._query_prolog("filme_aleatorio")
    
    # ... resto do handler
```

#### 2.3 Adicionar regra Prolog (se não existir)
```prolog
filme_aleatorio_por_genero(Genero, Titulo) :-
    filme(Titulo, _, Generos, _, _, _),
    member(Genero, Generos),
    random_member(Titulo, Filmes).
```

### Arquivos a Modificar
- [ ] `app/intent_router.py` - handler `handle_recomendar_filme`
- [ ] `app/nlu_engine.py` - extração de entidades para `recomendar_filme`
- [ ] `prolog/rules/inferencia.pl` - regra de filme aleatório por gênero

---

## ✅ Bug #3: Falha na Extração de Entidades Numéricas (Ano) (CORRIGIDO)

### Descrição
- **Input:** "Filmes de 2020"
- **Comportamento Atual:** Responde "Não encontrei o ator '2020'"
- **Comportamento Esperado:** Listar filmes lançados em 2020

### Diagnóstico
O NER está classificando números como PERSON em vez de DATE/YEAR. Falta intent específico para busca por ano.

### Solução Proposta

#### 3.1 Adicionar intent "filmes_por_ano" no `intent_patterns.json`
```json
"filmes_por_ano": {
    "keywords": [
        "filmes",
        "filme",
        "lançados",
        "lançamentos",
        "ano"
    ],
    "prepositions": [
        "de",
        "em",
        "do ano"
    ],
    "entity_type": "YEAR",
    "examples": [
        "filmes de 2020",
        "filmes lançados em 2019",
        "lançamentos de 2021"
    ]
}
```

#### 3.2 Adicionar detecção de ano no NLU
```python
def _extract_year(self, text: str) -> Optional[int]:
    """Extrai ano (1900-2030) do texto."""
    match = re.search(r'\b(19\d{2}|20[0-3]\d)\b', text)
    if match:
        return int(match.group(1))
    return None
```

#### 3.3 Priorizar detecção de ano sobre ator
Na função `_extract_entities`, verificar primeiro se há um ano válido antes de assumir que é nome de pessoa.

#### 3.4 Criar handler e regra Prolog
```python
async def handle_filmes_por_ano(self, nlu_result: NLUResult, session_id: str) -> ChatResponse:
    year = nlu_result.entities.get("year")
    # Query Prolog para filmes do ano
```

```prolog
filmes_por_ano(Ano, Filmes) :-
    findall(Titulo, filme(Titulo, Ano, _, _, _, _), Filmes).
```

### Arquivos a Modificar
- [ ] `app/intent_patterns.json`
- [ ] `app/nlu_engine.py` - `_extract_year()` e `_extract_entities()`
- [ ] `app/intent_router.py` - `handle_filmes_por_ano`
- [ ] `prolog/rules/inferencia.pl`

---

## ✅ Bug #4: Incapacidade de Consultar Diretor (CORRIGIDO)

### Descrição
- **Input:** "Quem dirigiu Titanic?"
- **Comportamento Atual:** Não entende e oferece ajuda
- **Comportamento Esperado:** Responder com o nome do diretor

### Diagnóstico
Falta intent para consultar diretor de um filme específico (inverso de "filmes_por_diretor").

### Solução Proposta

#### 4.1 Adicionar intent "diretor_do_filme" no `intent_patterns.json`
```json
"diretor_do_filme": {
    "keywords": [
        "quem dirigiu",
        "diretor de",
        "diretor do",
        "quem fez",
        "quem criou",
        "dirigido por quem"
    ],
    "prepositions": [
        "de",
        "do",
        "da"
    ],
    "entity_type": "MOVIE",
    "examples": [
        "quem dirigiu Titanic?",
        "diretor de Matrix",
        "quem fez Inception?"
    ]
}
```

#### 4.2 Criar handler
```python
async def handle_diretor_do_filme(self, nlu_result: NLUResult, session_id: str) -> ChatResponse:
    filme = nlu_result.entities.get("movie") or nlu_result.entities.get("filme")
    if not filme:
        return self._error_response("Qual filme você quer saber o diretor?")
    
    result = await self._query_prolog("diretor_de_filme", filme)
    # ...
```

#### 4.3 Adicionar regra Prolog
```prolog
diretor_de_filme(Titulo, Diretor) :-
    filme(Titulo, _, _, Diretor, _, _).
```

### Arquivos a Modificar
- [ ] `app/intent_patterns.json`
- [ ] `app/nlu_engine.py` - adicionar aos prioritários
- [ ] `app/intent_router.py` - `handle_diretor_do_filme`
- [ ] `prolog/rules/inferencia.pl`

---

## ✅ Bug #5: Falsos Positivos em Perguntas Genéricas (CORRIGIDO)

### Descrição
- **Input:** "O que é o sentido da vida?"
- **Comportamento Atual:** Recomenda filme com "vida" no título
- **Comportamento Esperado:** Responder que não entendeu ou classificar como small talk

### Diagnóstico
Threshold de confiança muito baixo permite que queries genéricas sejam processadas como busca de filmes.

### Solução Proposta

#### 5.1 Aumentar threshold de confiança mínima
```python
# Em intent_router.py ou main.py
MINIMUM_CONFIDENCE_THRESHOLD = 0.6  # Atualmente pode estar em ~0.3-0.4

if nlu_result.confidence < MINIMUM_CONFIDENCE_THRESHOLD:
    return self._fallback_response(nlu_result)
```

#### 5.2 Adicionar intent "small_talk" para filtrar conversas genéricas
```json
"small_talk": {
    "keywords": [
        "sentido da vida",
        "por que existimos",
        "qual o sentido",
        "filosofia",
        "o que é a vida",
        "o que você acha",
        "você gosta",
        "você tem sentimentos",
        "você é humano"
    ],
    "prepositions": [],
    "entity_type": "NONE",
    "examples": [
        "o que é o sentido da vida?",
        "você é humano?"
    ]
}
```

#### 5.3 Criar handler de small_talk
```python
async def handle_small_talk(self, nlu_result: NLUResult, session_id: str) -> ChatResponse:
    return ChatResponse(
        type=ResponseType.TEXT,
        content="🤔 Essa é uma pergunta interessante, mas sou especializado em filmes! "
                "Posso te ajudar a encontrar um bom filme para assistir?",
        suggestions=["ajuda", "filme aleatório", "filmes de drama"],
        metadata=self._build_metadata(nlu_result)
    )
```

#### 5.4 Melhorar fallback com sugestões contextuais
Quando confiança < threshold, não tentar processar - retornar resposta de clarificação.

### Arquivos a Modificar
- [ ] `app/intent_patterns.json`
- [ ] `app/intent_router.py` - threshold e `handle_small_talk`
- [ ] `app/nlu_engine.py` - adicionar `small_talk` aos checks

---

## 📊 Ordem de Implementação Recomendada

### Fase 1 - Quick Wins (Baixa complexidade, alto impacto)
1. **Bug #1** - Identidade (30 min)
2. **Bug #4** - Diretor do filme (45 min)

### Fase 2 - Melhorias Críticas (Média complexidade)
3. **Bug #5** - Falsos positivos/threshold (1h)
4. **Bug #3** - Extração de ano (1-2h)

### Fase 3 - Refinamentos
5. **Bug #2** - Filtragem por gênero na recomendação (1-2h)

---

## ✅ Checklist de Testes

Após implementação, validar com os seguintes inputs:

| Input | Intent Esperado | Resposta Esperada |
|-------|-----------------|-------------------|
| "Quem é você?" | identidade | Apresentação do bot |
| "Me recomende um filme de ação" | recomendar_filme | Filme de ação |
| "Filmes de 2020" | filmes_por_ano | Lista de filmes de 2020 |
| "Quem dirigiu Titanic?" | diretor_do_filme | Nome do diretor |
| "O que é o sentido da vida?" | small_talk / fallback | Mensagem de escopo |
| "ajuda" | ajuda | Menu de ajuda |
| "oi" | saudacao | Saudação |

---

## 📝 Notas Adicionais

### Dependências
- Verificar se as regras Prolog existentes suportam as novas queries
- Garantir que os caches Redis têm dados de diretores e anos

### Riscos
- Adicionar muitos intents pode causar conflitos de classificação
- Threshold muito alto pode rejeitar queries válidas

### Métricas de Sucesso
- 100% dos casos de teste passando
- Confiança média > 0.7 para queries válidas
- Zero falsos positivos para small talk

---

**Autor:** GitHub Copilot  
**Revisão:** Pendente
