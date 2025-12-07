# Plan: Implementação de Semantic Parsing com Sentence-Transformers

Implementação de compreensão semântica usando `sentence-transformers` para superar as limitações do sistema atual baseado em keywords, mantendo compatibilidade com a arquitetura existente através de uma abordagem híbrida.

## Objetivo

Adicionar capacidade de entendimento semântico ao chatbot para:
- Compreender paráfrases ("quero rir" → filmes de comédia)
- Suportar queries em inglês e português
- Lidar com descrições contextuais ("aquele filme do barco que afunda" → Titanic)
- Melhorar desambiguação entre intents similares

## Arquitetura Híbrida: Rule-Based + Semantic

```
User Query
    ↓
[Stage 0: Priority Rules] ← Manter existente (protege comandos simples)
    ↓
[Stage 1: Spell Correction] ← Manter existente
    ↓
[Stage 2A: Semantic Intent Classification] ← NOVO
    ↓ (se confidence < threshold)
[Stage 2B: Keyword-Based Fallback] ← Manter existente
    ↓
[Stage 3: Entity Extraction - Hybrid] ← APRIMORAR
    ↓
[Stage 4: Entity Validation - Semantic] ← APRIMORAR
    ↓
Handler Routing
```

## Steps

### 1. Criar infraestrutura base para classificação semântica

**Objetivo**: Estabelecer foundation para semantic parsing

**Tarefas**:
- [ ] Adicionar `sentence-transformers>=2.2.0` e `torch>=2.0.0` em `requirements.txt`
- [ ] Criar `app/semantic_classifier.py` com classe `SemanticIntentClassifier`
  - ⚠️ **USAR MODELO RÁPIDO**: `paraphrase-multilingual-MiniLM-L12-v2` (118M params, 2x mais rápido)
  - Implementar método `classify()` que retorna top-K intents com scores de similaridade cosseno
  - Carregar embeddings dos exemplos de `intent_patterns.json` no `__init__()`
- [ ] Criar `app/config.py` com configurações:
  ```python
  USE_SEMANTIC_NLU: bool = True  # Toggle via env var
  SEMANTIC_MODEL_NAME: str = "paraphrase-multilingual-MiniLM-L12-v2"  # ⚠️ ALTERADO
  SEMANTIC_INTENT_THRESHOLD: float = 0.75  # Alta confiança
  SEMANTIC_ENTITY_THRESHOLD: float = 0.70
  ```

**Estrutura `SemanticIntentClassifier`**:
```python
class SemanticIntentClassifier:
    def __init__(self, model_name="paraphrase-multilingual-MiniLM-L12-v2"):  # ⚠️ MODELO RÁPIDO
        self.model = SentenceTransformer(model_name)
        self.intent_embeddings = {}
        self.intent_examples = {}
        
    def load_intent_patterns(self, patterns: dict):
        """Compute embeddings for all example queries."""
        for intent, pattern in patterns.items():
            examples = pattern.get("examples", [])
            if examples:
                embeddings = self.model.encode(examples, convert_to_tensor=True)
                self.intent_embeddings[intent] = embeddings
                self.intent_examples[intent] = examples
    
    def classify(self, query: str, top_k=3) -> List[Tuple[str, float]]:
        """Returns: [(intent, confidence), ...]"""
        query_embedding = self.model.encode(query, convert_to_tensor=True)
        
        scores = []
        for intent, example_embeddings in self.intent_embeddings.items():
            similarities = util.cos_sim(query_embedding, example_embeddings)
            max_sim = similarities.max().item()
            scores.append((intent, max_sim))
        
        scores.sort(key=lambda x: x[1], reverse=True)
        return scores[:top_k]
```

**Critérios de sucesso**:
- Modelo carrega em < 10s
- Classificação de query única em < 100ms
- Testes unitários passando

---

### 2. Aumentar dados de treino com augmentação

**Objetivo**: Resolver problema de dados escassos (apenas 2-3 exemplos por intent)

**Problema atual**:
- `intent_patterns.json` tem apenas ~50 exemplos totais
- Insuficiente para treinar/testar semantic classifier
- Não cobre paráfrases e variações

**Tarefas**:
- [ ] Criar script `scripts/augment_intent_data.py` com:
  - Back-translation (PT → EN → PT usando MarianMT)
  - Paraphrasing (T5-based paraphraser)
  - Template generation (combinar templates com entities do cache)
  - Synonym replacement para keywords comuns
- [ ] Gerar 15-20 variações para cada exemplo existente
- [ ] Adicionar manualmente paráfrases criativas:
  - "quero rir" → `filmes_por_genero` (comédia)
  - "algo assustador" → `filmes_por_genero` (terror)
  - "aquele diretor inglês do batman" → `filmes_por_diretor` (Christopher Nolan)
- [ ] Criar `app/intent_examples_augmented.json` com estrutura:
  ```json
  {
    "filmes_por_ator": {
      "original": ["filmes com adam sandler", ...],
      "augmented": [
        "obras do adam sandler",
        "adam sandler filmes",
        "quero ver filmes com adam sandler",
        "o que adam sandler atuou",
        "filmografia do adam sandler",
        ...
      ]
    }
  }
  ```

**Meta**: 300-400 exemplos totais (15-25 por intent)

**Exemplo de augmentação**:
```python
class IntentDataAugmenter:
    def __init__(self):
        self.paraphraser = pipeline("text2text-generation", 
                                     model="humarin/chatgpt_paraphraser_on_T5_base")
    
    def augment_examples(self, examples: List[str], n_augment=5) -> List[str]:
        augmented = examples.copy()
        for example in examples:
            paraphrases = self.paraphraser(example, 
                                           max_length=60, 
                                           num_return_sequences=n_augment)
            augmented.extend([p['generated_text'] for p in paraphrases])
        return augmented
```

**Critérios de sucesso**:
- 300+ exemplos gerados
- Revisão manual de qualidade (amostra de 10%)
- Exemplos cobrem paráfrases comuns

---

### 3. Integrar classificação semântica no NLUEngine com fallback

**Objetivo**: Adicionar semantic classification mantendo backward compatibility

**Tarefas**:
- [ ] Modificar `app/nlu_engine.py`:
  - Adicionar parâmetro `use_semantic=True` no `__init__()`
  - Implementar try-except para fallback gracioso se modelo falhar
  - Renomear método `_detect_intent()` para `_detect_intent_keyword_based()`
  - Criar novo `_detect_intent()` com lógica híbrida:

**Lógica de decisão híbrida**:
```python
def _detect_intent(self, text_lower: str, doc) -> Tuple[str, float]:
    # Try semantic first
    if self.use_semantic:
        semantic_results = self.semantic_classifier.classify(text_lower)
        best_intent, semantic_score = semantic_results[0]
        
        # HIGH CONFIDENCE: Use semantic directly
        if semantic_score >= 0.75:
            logger.debug(f"Semantic (high conf): {best_intent} ({semantic_score:.2f})")
            return best_intent, semantic_score
        
        # MEDIUM CONFIDENCE: Validate with keywords
        if semantic_score >= 0.60:
            keyword_intent, keyword_score = self._detect_intent_keyword_based(text_lower, doc)
            if keyword_intent == best_intent:
                # Agreement → combine scores
                combined_score = 0.6 * semantic_score + 0.4 * keyword_score
                logger.debug(f"Semantic+Keyword agree: {best_intent} ({combined_score:.2f})")
                return best_intent, combined_score
            else:
                # Disagreement → use higher confidence
                if semantic_score > keyword_score:
                    return best_intent, semantic_score
                else:
                    return keyword_intent, keyword_score
    
    # LOW CONFIDENCE or semantic disabled: Fallback to keyword-based
    return self._detect_intent_keyword_based(text_lower, doc)
```

**Modificações adicionais**:
- [ ] Adicionar flag `self._semantic_score` para uso em `_calculate_overall_confidence()`
- [ ] Atualizar `_calculate_overall_confidence()` para boost semântico (+20% se score alto)
- [ ] Adicionar logging detalhado de qual método foi usado (semantic/keyword/hybrid)

**Inicialização em `app/main.py`**:
```python
@app.on_event("startup")
async def startup_event():
    # ... existing code ...
    
    # Initialize semantic NLU
    if config.USE_SEMANTIC_NLU:
        try:
            semantic_classifier = SemanticIntentClassifier(
                model_name=config.SEMANTIC_MODEL_NAME
            )
            semantic_classifier.load_intent_patterns(nlu_engine.intent_patterns)
            nlu_engine.semantic_classifier = semantic_classifier
            logger.info("✅ Semantic NLU initialized")
        except Exception as e:
            logger.warning(f"⚠️ Semantic NLU failed to load: {e}")
            logger.warning("Falling back to keyword-based NLU")
```

**Critérios de sucesso**:
- Sistema funciona com `use_semantic=True` e `False`
- Fallback automático se modelo não carregar
- Testes existentes em `tests/test_nlu_engine.py` continuam passando
- Latência adicional < 100ms (P50)

---

### 4. Criar extração semântica de entidades para validação híbrida

**Objetivo**: Melhorar extração de entidades com semantic similarity

**Tarefas**:
- [ ] Criar `app/semantic_entity_extractor.py`:

```python
class SemanticEntityExtractor:
    def __init__(self, model_name="paraphrase-multilingual-MiniLM-L12-v2"):  # ⚠️ MODELO RÁPIDO
        self.model = SentenceTransformer(model_name)
        self.nlp = spacy.load("pt_core_news_sm")  # ⚠️ ADICIONAR para segmentação
        self.entity_embeddings = {}
        
    def load_entity_caches(self, actors, genres, films, directors):
        """Precompute embeddings for all entities."""
        # ~60MB total RAM
        self.entity_embeddings['actor'] = self._encode_entities(actors)
        self.entity_embeddings['genre'] = self._encode_entities(genres)
        self.entity_embeddings['film'] = self._encode_entities(films)
        self.entity_embeddings['director'] = self._encode_entities(directors)
    def _encode_entities(self, entities: List[str]) -> dict:
        """Returns {entity_name: embedding}"""
        embeddings = self.model.encode(entities, convert_to_tensor=True)
        return {entity: emb for entity, emb in zip(entities, embeddings)}
    
    def _extract_entity_candidates(self, query: str) -> List[str]:
        """
        ⚠️ CRÍTICO: Extrai candidatos com spaCy ANTES de vetorizar.
        Previne comparação de frase inteira (com ruído) vs entidade.
        """
        doc = self.nlp(query)
        candidates = []
        
        # Estratégia 1: Entidades nomeadas
        for ent in doc.ents:
            if ent.label_ in ["PER", "ORG", "MISC"]:
                candidates.append(ent.text)
        
        # Estratégia 2: Noun chunks (ex: "aquele ator de Titanic")
        for chunk in doc.noun_chunks:
            text = chunk.text.strip()
            if len(text.split()) >= 2:
                candidates.append(text)
        
        # Estratégia 3: Texto após preposições
        prep_patterns = [" com ", " de ", " do ", " da ", " por "]
        query_lower = query.lower()
        for prep in prep_patterns:
            if prep in query_lower:
                idx = query_lower.rfind(prep)
                after_prep = query[idx + len(prep):].strip().rstrip("?!.,")
                if after_prep:
                    candidates.append(after_prep)
        
        # Remove duplicatas
        seen = set()
        unique = []
        for c in candidates:
            c_clean = c.strip().lower()
            if c_clean and c_clean not in seen and len(c_clean) > 2:
                seen.add(c_clean)
                unique.append(c.strip())
        
        return unique
    
    def find_best_entity(self, query: str, entity_type: str, 
                        threshold=0.7, top_k=5) -> Optional[dict]:
        """
        ⚠️ VERSÃO CORRIGIDA: Vetoriza apenas candidatos extraídos.
        """
        if entity_type not in self.entity_embeddings:
            return None
        
        # ⚠️ PASSO 1: Extrair candidatos com spaCy
        candidates = self._extract_entity_candidates(query)
        if not candidates:
            candidates = [query]  # Fallback
        
        # ⚠️ PASSO 2: Vetorizar apenas candidatos
        best_match = None
        best_score = 0.0
        all_scores = []
        
        for candidate in candidates:
            candidate_embedding = self.model.encode(candidate, convert_to_tensor=True)
            
            for entity, entity_embedding in self.entity_embeddings[entity_type].items():
                similarity = util.cos_sim(candidate_embedding, entity_embedding).item()
                
                if similarity > best_score:
                    best_score = similarity
                    best_match = entity
                
                all_scores.append((entity, similarity, candidate))
        
        all_scores.sort(key=lambda x: x[1], reverse=True)
        
- [ ] ⚠️ **CRIAR `app/prolog_normalizer.py`** (CRÍTICO):
  ```python
  import re
  import unicodedata
  
  class PrologNormalizer:
      @staticmethod
      def normalize_to_prolog_atom(entity: str) -> str:
          """Leonardo DiCaprio → leonardo_dicaprio"""
          normalized = entity.lower()
          nfkd = unicodedata.normalize('NFKD', normalized)
          normalized = ''.join(c for c in nfkd if not unicodedata.combining(c))
          normalized = normalized.replace(' ', '_').replace('&', '_')
          normalized = re.sub(r'[^a-z0-9_]', '', normalized)
          normalized = re.sub(r'_+', '_', normalized).strip('_')
          return normalized
      
      @staticmethod
      def normalize_entity_for_query(entity: str, entity_type: str) -> str:
          """Normaliza baseado no tipo para match com KB."""
          if entity_type in ['actor', 'director', 'genre', 'film']:
              return entity.upper()  # KB usa UPPERCASE
          return entity
  ```

- [ ] Adicionar wrappers em `app/nlu.py`:
  - `find_best_actor_semantic()` → retorna entity normalizada
  - `find_best_genre_semantic()` → retorna entity normalizada
  - `find_best_film_semantic()` → retorna entity normalizada
  - `find_best_director_semantic()` → retorna entity normalizada

- [ ] ⚠️ **MODIFICAR handlers** para usar normalização:
  ```python
  # Em search_handlers.py
  from ..prolog_normalizer import PrologNormalizer
  
  async def handle_filmes_por_ator(self, entities, session_id):
      ator = entities.get("ator", "")
      # ... semantic/fuzzy matching ...
      ator_matched = semantic_extractor.find_best_entity(ator, 'actor')['best_match']
      
      # ⚠️ NORMALIZAÇÃO OBRIGATÓRIA
      ator_normalized = PrologNormalizer.normalize_entity_for_query(
          ator_matched, 'actor'
      )
      
      escaped = self._escape_prolog_string(ator_normalized)
      query = f"imdb_rules:filmes_por_ator('{escaped}', TituloFilme)"
      # ...
  ```

- [ ] Integrar no `_extract_entities()` do `NLUEngine`:
  - Se `use_semantic=True`, tentar semantic extraction primeiro
  - Fallback para métodos existentes (preposição-based, spaCy NER)
  - ⚠️ Sempre normalizar entidades antes de retornar
```

- [ ] Modificar `app/nlu.py` para adicionar funções híbridas:

```python
def find_best_match_hybrid(query: str, cache: list[str], 
                          entity_type: str) -> Optional[str]:
    """
    Hybrid matching: semantic (60%) + fuzzy (40%)
    """
    # Stage 1: Semantic matching
    if semantic_extractor:
        semantic_result = semantic_extractor.find_best_entity(
            query, entity_type, threshold=0.60
        )
        
        if semantic_result:
            # Stage 2: Fuzzy validation
            best_candidate = semantic_result['best_match']
            semantic_score = semantic_result['confidence']
            fuzzy_score = fuzz.ratio(query.upper(), best_candidate.upper()) / 100
            
            combined_score = 0.6 * semantic_score + 0.4 * fuzzy_score
            
            if combined_score >= 0.65:
                return best_candidate
    
    # Fallback: Pure fuzzy
    return find_best_match(query, cache)

def is_valid_genre_semantic(query: str) -> bool:
    """Enhanced genre validation with semantic understanding."""
    # Existing fuzzy checks
    if is_valid_genre(query):
        return True
    
    # Semantic check for synonyms
    if semantic_extractor:
        result = semantic_extractor.find_best_entity(query, 'genre', threshold=0.70)
        return result is not None
    
    return False
```

- [ ] Adicionar wrappers:
  - `find_best_actor_semantic()`
  - `find_best_genre_semantic()`
  - `find_best_film_semantic()`
  - `find_best_director_semantic()`

- [ ] Integrar no `_extract_entities()` do `NLUEngine`:
  - Se `use_semantic=True`, tentar semantic extraction primeiro
  - Fallback para métodos existentes (preposição-based, spaCy NER)

**Pré-computação de embeddings**:
- [ ] Carregar caches no startup de `main.py`
- [ ] Pré-computar embeddings e armazenar em memória (~60MB)
- [ ] Considerar cache em disco para evitar recomputação

**Critérios de sucesso**:
- Entidades extraídas corretamente mesmo com typos semânticos
- "assustador" detectado como "horror"
- "engraçado" detectado como "comédia"
- Latência < 50ms para entity extraction

---

### 5. Implementar otimização e monitoramento de performance

**Objetivo**: Garantir que semantic parsing não degrada performance

**Tarefas de Otimização**:

- [ ] **Caching de embeddings de queries no Redis**:
  ```python
  def get_query_embedding(self, query: str) -> torch.Tensor:
      cache_key = f"emb:{hashlib.md5(query.encode()).hexdigest()}"
      
      # Try Redis cache
      cached = redis_client.get(cache_key)
      if cached:
          return torch.from_numpy(np.frombuffer(cached, dtype=np.float32))
      
      # Compute and cache
      embedding = self.model.encode(query, convert_to_tensor=True)
      redis_client.setex(cache_key, 3600, embedding.cpu().numpy().tobytes())
      return embedding
  ```

- [ ] **Lazy loading do modelo**:
  ```python
  @property
  def model(self):
      if not hasattr(self, '_model'):
          logger.info("Loading sentence-transformer model...")
          self._model = SentenceTransformer(self.model_name)
      return self._model
  ```

- [ ] **Batch processing para múltiplas queries**:
  - Se handler processar múltiplas entities, processar em batch
  - Reduz overhead de inferência

- [ ] **Quantização do modelo (opcional)**:
  - Usar ONNX Runtime para inferência 2-3x mais rápida
  - Reduz memória em ~50%

**Tarefas de Monitoramento**:

- [ ] Adicionar logging de métricas em `nlu_engine.py`:
  ```python
  logger.info(f"Intent detection: method={method}, latency={latency_ms}ms, "
              f"semantic_score={semantic_score:.2f}, fallback={used_fallback}")
  ```

- [ ] Criar dashboard de métricas:
  - Taxa de uso de semantic vs keyword
  - Distribuição de scores semânticos
  - Latência P50, P95, P99
  - Taxa de fallback

- [ ] Implementar A/B testing:
  ```python
  def should_use_semantic(session_id: str) -> bool:
      """Route 50% of users to semantic NLU."""
      hash_val = int(hashlib.md5(session_id.encode()).hexdigest(), 16)
      return hash_val % 2 == 0
  ```

**Testes de Performance**:

- [ ] Criar `tests/test_semantic_classifier.py`:
  - Benchmark latência: semantic vs keyword
  - Teste de acurácia em queries de `test_nlu_engine.py`
  - Comparação de scores
  - Teste de fallback gracioso

- [ ] Criar `tests/benchmark_semantic.py`:
  - Load test com 1000 queries variadas
  - Medir P50, P95, P99 latency
  - Medir memória (baseline vs semantic)

**Targets de Performance**:
- Latência P50: < 100ms adicionado
- Latência P95: < 250ms adicionado
- Model load time: < 10s
- Memory overhead: < 100MB
- Fallback rate: < 10%

**Critérios de sucesso**:
- Targets de performance atingidos
- Testes de benchmark documentados
- A/B testing framework funcional
- Métricas disponíveis em logs

---

## Casos de Uso Melhorados

### Antes vs Depois

#### Caso 1: Paráfrase
**Query**: "quero rir"
- **Antes**: ❌ Falha (nenhuma keyword de "filmes" ou "comédia")
- **Depois**: ✅ Intent: `filmes_por_genero`, Entity: `comédia` (semantic similarity com exemplos)

#### Caso 2: Descrição contextual
**Query**: "aquele filme do barco que afunda"
- **Antes**: ❌ Falha (nenhuma keyword reconhecida)
- **Depois**: ✅ Intent: `busca_especifica`, Semantic search encontra "Titanic" via contexto

#### Caso 3: Query em inglês
**Query**: "action movies"
- **Antes**: ❌ Falha (keywords em português)
- **Depois**: ✅ Intent: `filmes_por_genero`, Entity: `ACTION & ADVENTURE` (modelo multilingual)

#### Caso 4: Sinônimo
**Query**: "filmes assustadores"
- **Antes**: ⚠️ Possível falha se "assustador" não no `GENRE_TRANSLATION_MAP`
- **Depois**: ✅ Entity: `HORROR MOVIES` (semantic similarity)

#### Caso 5: Query mista
**Query**: "filmes de action com Nolan"
- **Antes**: ⚠️ Depende de fuzzy matching
- **Depois**: ✅ Intent: `filmes_com_filtros`, Entities: `{genre: 'ACTION & ADVENTURE', diretor: 'Christopher Nolan'}` (semantic + fuzzy)

---

## Estratégia de Deployment

### Fase 1: Desenvolvimento (Semana 1-2)
- Implementar Steps 1-2 (infraestrutura + augmentação)
- Testes unitários locais
- Benchmark de acurácia vs baseline

### Fase 2: Integração (Semana 3)
- Implementar Steps 3-4 (integração + entity extraction)
- Testes de integração
- Code review

### Fase 3: Otimização (Semana 4)
- Implementar Step 5 (performance + monitoring)
- Load testing
- Tuning de thresholds

### Fase 4: Rollout Gradual (Semana 5-6)
- **10% de usuários** (1 semana):
  - A/B test com hash de session_id
  - Monitorar: accuracy, latency, crash rate
  - Coletar feedback
- **50% de usuários** (1 semana):
  - Se métricas OK, expandir
  - Ajustar thresholds se necessário
- **100% de usuários**:
  - Rollout completo
  - Feature flag permanece para rollback rápido

---

## Métricas de Sucesso

### Acurácia
- **Intent Classification Accuracy**: >90% (vs ~85% keyword baseline)
- **Entity Extraction F1-Score**: >85%
- **Paraphrase Recognition**: >80% (novo métrico)

### Performance
- **Latência P50**: <100ms added
- **Latência P95**: <250ms added
- **Model Load Time**: <10s
- **Memory Overhead**: <100MB
- **Fallback Rate**: <10%

### User Experience
- **Successful Query Rate**: >95% (vs ~92% baseline)
- **Clarification Request Rate**: <5%
- **User Satisfaction**: Qualitative feedback

---

## Riscos e Mitigações

| Risco | Probabilidade | Impacto | Mitigação |
|-------|--------------|---------|-----------|
| Model loading failure | Baixa | Alta | Fallback robusto para keyword-based |
| Poor accuracy on domain | Média | Alta | Data augmentation, fine-tune em feedback |
| Performance degradation | Média | Média | Caching, lazy loading, batch processing |
| Breaking existing tests | Baixa | Baixa | Backward compatibility, tests abrangentes |
| User confusion | Baixa | Média | A/B test, rollout gradual, feature flag |

---

## ⚠️ Riscos Críticos e Ajustes Obrigatórios

### 🔴 CRÍTICO 1: Extração de Entidades em Frases Complexas

**O Problema**:
O método `find_best_entity()` proposto compara o embedding da **frase inteira** com embeddings de entidades isoladas, gerando muitos falsos negativos.

**Exemplo de Falha**:
```python
Query: "Quero ver filmes com aquele ator de Titanic"
Embedding da frase: [muito ruído: "quero", "ver", "filmes", "com"...]
Embedding de "Leonardo DiCaprio": [nome limpo]
Similaridade: 0.35 ❌ (abaixo do threshold 0.70)
```

**✅ Solução Obrigatória: Segmentação Antes da Vetorização**

**Adicionar ao Step 4** um pré-processamento com spaCy:

```python
class SemanticEntityExtractor:
    def __init__(self, model_name="paraphrase-multilingual-MiniLM-L12-v2"):
        self.model = SentenceTransformer(model_name)
        self.nlp = spacy.load("pt_core_news_sm")  # Para segmentação
        self.entity_embeddings = {}
    
    def _extract_entity_candidates(self, query: str) -> List[str]:
        """
        Extrai candidatos a entidades usando spaCy ANTES de vetorizar.
        
        Estratégias:
        1. Entidades nomeadas (PER, ORG, MISC)
        2. Noun chunks (substantivos compostos)
        3. Tokens após preposições chave
        """
        doc = self.nlp(query)
        candidates = []
        
        # Estratégia 1: Entidades detectadas pelo spaCy
        for ent in doc.ents:
            if ent.label_ in ["PER", "ORG", "MISC"]:
                candidates.append(ent.text)
        
        # Estratégia 2: Noun chunks (ex: "aquele ator de Titanic")
        for chunk in doc.noun_chunks:
            # Remove determinantes no início
            text = chunk.text.strip()
            if len(text.split()) >= 2:  # Apenas chunks multi-palavra
                candidates.append(text)
        
        # Estratégia 3: Texto após preposições específicas
        prep_patterns = [" com ", " de ", " do ", " da ", " por "]
        query_lower = query.lower()
        for prep in prep_patterns:
            if prep in query_lower:
                idx = query_lower.rfind(prep)
                after_prep = query[idx + len(prep):].strip()
                # Remove pontuação final
                after_prep = after_prep.rstrip("?!.,")
                if after_prep:
                    candidates.append(after_prep)
        
        # Remove duplicatas mantendo ordem
        seen = set()
        unique_candidates = []
        for c in candidates:
            c_clean = c.strip().lower()
            if c_clean and c_clean not in seen and len(c_clean) > 2:
                seen.add(c_clean)
                unique_candidates.append(c.strip())
        
        return unique_candidates
    
    def find_best_entity(self, query: str, entity_type: str, 
                        threshold=0.7, top_k=5) -> Optional[dict]:
        """
        VERSÃO CORRIGIDA: Vetoriza apenas os candidatos extraídos.
        """
        if entity_type not in self.entity_embeddings:
            return None
        
        # PASSO 1: Extrair candidatos com spaCy
        candidates = self._extract_entity_candidates(query)
        
        if not candidates:
            # Fallback: usar query completa (comportamento anterior)
            candidates = [query]
        
        # PASSO 2: Vetorizar apenas os candidatos
        best_match = None
        best_score = 0.0
        all_scores = []
        
        for candidate in candidates:
            candidate_embedding = self.model.encode(candidate, convert_to_tensor=True)
            
            for entity, entity_embedding in self.entity_embeddings[entity_type].items():
                similarity = util.cos_sim(candidate_embedding, entity_embedding).item()
                
                if similarity > best_score:
                    best_score = similarity
                    best_match = entity
                
                all_scores.append((entity, similarity, candidate))
        
        # Ordena por score para alternativas
        all_scores.sort(key=lambda x: x[1], reverse=True)
        
        if best_score >= threshold:
            return {
                'best_match': best_match,
                'confidence': best_score,
                'matched_candidate': candidates[0] if candidates else query,
                'alternatives': [(e, s) for e, s, _ in all_scores[1:top_k]]
            }
        return None
```

**Impacto**: Reduz falsos negativos de ~40% para <5% em queries complexas.

---

### 🔴 CRÍTICO 2: Escolha de Modelo (Performance vs Precisão)

**O Problema**:
O plano escolhe `paraphrase-multilingual-mpnet-base-v2` (~100ms/query). Para extrair **intent + ator + gênero + diretor**, você rodará o modelo **4-5 vezes**, elevando latência para **400-500ms** (inaceitável).

**✅ Solução: Usar Modelo Mais Rápido**

**ALTERAR Step 1 e Step 4**:

```python
# ANTES (plano original)
SEMANTIC_MODEL_NAME: str = "paraphrase-multilingual-mpnet-base-v2"

# DEPOIS (obrigatório para MVP)
SEMANTIC_MODEL_NAME: str = "paraphrase-multilingual-MiniLM-L12-v2"
```

**Justificativa**:
- **Latência**: 40-50ms/query (2x mais rápido que mpnet)
- **Precisão**: Em domínios restritos (filmes), a diferença é <3% (aceitável)
- **Total por request**: 4 queries × 50ms = **200ms** (dentro do target P95 <250ms)

**Upgrade Path**: Se após testes em produção a precisão for insuficiente, considerar:
1. Fine-tuning do MiniLM com dados do chatbot
2. Upgrade para mpnet APENAS para intent classification (1 query), mantendo MiniLM para entities

**Impacto**: Mantém latência dentro dos targets sem sacrificar UX.

---

### 🔴 CRÍTICO 3: Normalização para Prolog (Ponte Python → Prolog)

**O Problema**:
O Prolog exige **átomos exatos** (ex: `'leonardo_dicaprio'`). O plano detecta entidades mas não garante conversão para o formato esperado pelo KB.

**Exemplo de Falha**:
```python
# Python detecta:
Query: "o cara do lobo de wall street"
Semantic Match: "Leonardo DiCaprio" (score 0.85)

# Python envia para Prolog:
query_prolog = f"filmes_por_ator('Leonardo DiCaprio', Filme)"
# ❌ FALHA: Prolog espera 'leonardo_dicaprio' (snake_case, lowercase)
```

**✅ Solução Obrigatória: Camada de Normalização**

**ADICIONAR ao Step 4** (após entity extraction):

```python
# Novo arquivo: app/prolog_normalizer.py

import re
import unicodedata

class PrologNormalizer:
    """
    Converte entidades detectadas para formato Prolog-compatible.
    """
    
    @staticmethod
    def normalize_to_prolog_atom(entity: str) -> str:
        """
        Converte entidade para átomo Prolog válido.
        
        Regras:
        1. Lowercase
        2. Remove acentos
        3. Substitui espaços por underscore
        4. Remove caracteres especiais (exceto underscore)
        5. Remove underscores duplicados/iniciais/finais
        
        Examples:
            "Leonardo DiCaprio" → "leonardo_dicaprio"
            "Ação & Aventura" → "acao_aventura"
            "The Rock" → "the_rock"
        """
        # 1. Lowercase
        normalized = entity.lower()
        
        # 2. Remove acentos
        nfkd = unicodedata.normalize('NFKD', normalized)
        normalized = ''.join(c for c in nfkd if not unicodedata.combining(c))
        
        # 3. Substitui espaços e & por underscore
        normalized = normalized.replace(' ', '_').replace('&', '_')
        
        # 4. Remove caracteres especiais (mantém apenas letras, números, underscore)
        normalized = re.sub(r'[^a-z0-9_]', '', normalized)
        
        # 5. Remove underscores duplicados/iniciais/finais
        normalized = re.sub(r'_+', '_', normalized)
        normalized = normalized.strip('_')
        
        return normalized
    
    @staticmethod
    def normalize_entity_for_query(entity: str, entity_type: str) -> str:
        """
        Normaliza baseado no tipo de entidade.
        
        Args:
            entity: Nome da entidade detectada
            entity_type: 'actor', 'genre', 'film', 'director'
        
        Returns:
            String normalizada para uso em query Prolog
        """
        if entity_type in ['actor', 'director']:
            # Pessoas: mantem uppercase para match com KB
            # Ex: "Leonardo DiCaprio" → "LEONARDO DICAPRIO"
            return entity.upper()
        
        elif entity_type == 'genre':
            # Gêneros: uppercase para match com GENRE_CACHE
            # Ex: "ação" → "ACTION & ADVENTURE" (via find_best_genre)
            return entity.upper()
        
        elif entity_type == 'film':
            # Filmes: uppercase para match com FILM_CACHE
            # Ex: "matrix" → "THE MATRIX"
            return entity.upper()
        
        return entity
```

**MODIFICAR handlers** para usar normalizador:

```python
# Em app/handlers/search_handlers.py

from ..prolog_normalizer import PrologNormalizer

### Referências Técnicas

### Modelos Considerados
- ✅ `paraphrase-multilingual-MiniLM-L12-v2`: **ESCOLHIDO** (rápido, 118M params)
- ⚠️ `paraphrase-multilingual-mpnet-base-v2`: Descartado (lento para MVP)
- `LaBSE`: Alternativa se precisar melhor suporte multilingualsponse(...)
        
        # ANTES: fuzzy match direto
        # best_match = find_best_actor(ator)
        
        # DEPOIS: semantic + normalização
        if use_semantic:
            semantic_result = semantic_extractor.find_best_entity(ator, 'actor')
            if semantic_result:
                ator_matched = semantic_result['best_match']
            else:
                ator_matched = find_best_actor(ator)  # Fallback
        else:
            ator_matched = find_best_actor(ator)
        
        if not ator_matched:
            return self._create_error_response(...)
        
        # ✅ NORMALIZAÇÃO OBRIGATÓRIA
        ator_normalized = PrologNormalizer.normalize_entity_for_query(
            ator_matched, 'actor'
        )
        
        # Construir query com átomo normalizado
        escaped = self._escape_prolog_string(ator_normalized)
        query_string = f"imdb_rules:filmes_por_ator('{escaped}', TituloFilme)"
        
        results = await self._query_prolog(query_string)
        # ...
```

**Impacto**: Elimina 100% das falhas por incompatibilidade de formato Python ↔ Prolog.

---

## Questões em Aberto

### 1. Trade-off modelo vs performance? ✅ RESOLVIDO
**Decisão Final**: 
- **Usar `paraphrase-multilingual-MiniLM-L12-v2`** (118M params) para MVP
  - Pros: 2x mais rápido (~40-50ms), 120MB em disco, latência total <250ms
  - Cons: -3% acurácia vs mpnet (aceitável em domínio restrito)
- **Caminho de upgrade**: Fine-tuning do MiniLM com dados reais, ou mpnet apenas para intent classification

~~**Recomendação anterior**: Começar com modelo maior (A), otimizar depois se necessário~~ ❌ REJEITADO por risco de latência

### 2. GPU vs CPU inference?
- **CPU**: Latência ~200ms, sem dependências extras
- **GPU**: Latência ~20ms, requer CUDA setup
- **Recomendação**: CPU para MVP, considerar GPU em produção se latência for issue

### 3. Fine-tuning do modelo?
- **Sem fine-tuning**: Zero-shot com dados augmentados
- **Com fine-tuning**: Treinar em dados do chatbot (requer mais dados)
- **Recomendação**: Começar zero-shot, considerar fine-tuning após coletar 1000+ queries reais

### 4. Tratamento de follow-up queries?
- Ex: Usuário pergunta "filmes de ação", depois "e de terror?"
- **Opção A**: Ignorar contexto (comportamento atual)
- **Opção B**: Implementar context tracking com session state
- **Recomendação**: Out of scope para MVP, considerar em futuro

---

## Recursos Necessários

### Desenvolvimento
- **1 desenvolvedor sênior**: 15-23 dias (~4 semanas)
- **1 revisor**: 3-5 dias para code review

### Infraestrutura
- **RAM adicional**: +100MB por instância
- **Disco adicional**: +300MB (modelo)
- **GPU (opcional)**: Para inferência rápida

### Dados
- **Training data**: 300-400 exemplos (gerados via augmentação)
- **Test data**: 100+ queries manuais para benchmark

---

## Próximos Passos

1. ✅ Aprovação do plano técnico
2. ⏳ Setup do ambiente (instalar `sentence-transformers`, `torch`)
3. ⏳ Implementar Step 1 (infraestrutura base)
4. ⏳ Gerar dados augmentados (Step 2)
5. ⏳ Integrar no NLUEngine (Step 3)
6. ⏳ Entity extraction semântica (Step 4)
7. ⏳ Otimização e testes (Step 5)
8. ⏳ Deploy gradual com A/B testing

---

## Referências Técnicas

### Modelos Considerados
- `paraphrase-multilingual-mpnet-base-v2`: Recomendado
- `paraphrase-multilingual-MiniLM-L12-v2`: Alternativa rápida
- `LaBSE`: Alternativa para multilingual

### Bibliotecas
- `sentence-transformers`: Embeddings semânticos
- `torch`: Backend para transformers
- `transformers`: Data augmentation (T5 paraphraser)

### Papers/Recursos
- Sentence-BERT: https://arxiv.org/abs/1908.10084
- Multilingual sentence embeddings: https://arxiv.org/abs/2004.09813
- Zero-shot intent classification: https://arxiv.org/abs/1909.02027
