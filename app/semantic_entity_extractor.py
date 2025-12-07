"""
Semantic Entity Extractor - Step 4

Extrai entidades de queries usando combinação de:
1. Segmentação linguística (spaCy noun chunks/entities)
2. Embeddings semânticos (sentence-transformers)
3. Fuzzy matching (thefuzz) como fallback

Resolve o problema de comparar embeddings de query completa vs entidades,
extraindo candidatos específicos antes de fazer matching.
"""

import logging
from typing import List, Tuple, Optional, Dict
from thefuzz import fuzz

logger = logging.getLogger(__name__)


class SemanticEntityExtractor:
    """
    Extrator de entidades usando análise semântica e linguística.
    
    Pipeline:
    1. Segmenta query com spaCy (noun chunks, named entities, prepositions)
    2. Para cada candidato, calcula similarity com entidades conhecidas
    3. Retorna melhor match acima de threshold
    4. Fallback para fuzzy matching se semantic falhar
    """
    
    def __init__(self, nlp, semantic_classifier=None, threshold: float = 0.70):
        """
        Inicializa extrator.
        
        Args:
            nlp: Modelo spaCy carregado
            semantic_classifier: SemanticIntentClassifier com modelo embeddings
            threshold: Threshold mínimo para aceitar match semântico (0.0-1.0)
        """
        self.nlp = nlp
        self._semantic_classifier = semantic_classifier
        self.threshold = threshold
    
    def _extract_entity_candidates(self, text: str, doc=None) -> List[str]:
        """
        Extrai candidatos a entidades do texto usando análise linguística.
        
        Estratégias:
        1. Named entities do spaCy (PERSON, ORG, etc)
        2. Noun chunks (substantivos compostos)
        3. Texto após preposições comuns ("de", "por", "do", "da")
        4. Texto completo (último recurso)
        
        Args:
            text: Texto original da query
            doc: Doc do spaCy (opcional, se já processado)
            
        Returns:
            Lista de strings candidatas (ordenadas por prioridade)
        """
        if doc is None:
            doc = self.nlp(text)
        
        candidates = []
        text_lower = text.lower()
        
        # 1. Named Entities (maior prioridade)
        for ent in doc.ents:
            candidate = ent.text.strip()
            if len(candidate) >= 2:
                candidates.append(candidate)
                logger.debug(f"NER candidate: '{candidate}' (label={ent.label_})")
        
        # 2. Noun chunks (substantivos compostos)
        for chunk in doc.noun_chunks:
            candidate = chunk.text.strip()
            if len(candidate) >= 2 and candidate not in candidates:
                candidates.append(candidate)
                logger.debug(f"Noun chunk candidate: '{candidate}'")
        
        # 3. Texto após preposições (muito comum em PT)
        prepositions = [
            (" de ", " da ", " do "),  # Grupo 1: posse/origem
            (" por ", " pelo ", " pela "),  # Grupo 2: agente
            (" com ", " em ", " no ", " na ")  # Grupo 3: localização/modo
        ]
        
        for prep_group in prepositions:
            for prep in prep_group:
                if prep in text_lower:
                    # Pega a última ocorrência para evitar palavras de função
                    idx = text_lower.rfind(prep)
                    after_prep = text[idx + len(prep):].strip()
                    
                    # Remove pontuação final
                    after_prep = after_prep.rstrip("?!.,;")
                    
                    # Remove stopwords comuns no final
                    for stop in [" e ", " ou ", " que ", " em "]:
                        if stop in after_prep.lower():
                            after_prep = after_prep[:after_prep.lower().index(stop)].strip()
                    
                    if len(after_prep) >= 2 and after_prep not in candidates:
                        candidates.append(after_prep)
                        logger.debug(f"Prep candidate: '{after_prep}' (after '{prep.strip()}')")
                    break  # Só primeira match por grupo
        
        # 4. Se não encontrou nada, usa texto completo (menos stopwords iniciais)
        if not candidates:
            # Remove palavras interrogativas e de função no início
            stopwords_initial = [
                "quais", "qual", "quem", "o que", "que", "quantos", "quantas",
                "me", "mostre", "liste", "busque", "encontre", "ache"
            ]
            
            clean_text = text.strip().rstrip("?!.")
            words = clean_text.split()
            
            while words and words[0].lower() in stopwords_initial:
                words.pop(0)
            
            if words:
                candidate = " ".join(words)
                if len(candidate) >= 2:
                    candidates.append(candidate)
                    logger.debug(f"Full text candidate: '{candidate}'")
        
        return candidates
    
    def find_best_match_semantic(
        self, 
        query_text: str, 
        known_entities: List[str],
        top_k: int = 5,
        use_cache: bool = True
    ) -> Optional[Tuple[str, float]]:
        """
        Encontra melhor match semântico entre query e entidades conhecidas.
        
        Args:
            query_text: Texto da query (ou candidato extraído)
            known_entities: Lista de entidades conhecidas (ex: ACTOR_CACHE)
            top_k: Quantos top matches considerar
            use_cache: Se True, usa cache para embeddings de entidades
            
        Returns:
            Tupla (entity, confidence) ou None se nenhum match acima de threshold
        """
        if not self._semantic_classifier or not known_entities:
            return None
        
        # Extrai candidatos da query
        candidates = self._extract_entity_candidates(query_text)
        
        if not candidates:
            logger.debug("Nenhum candidato extraído para matching semântico")
            return None
        
        # Prepara cache de entidades
        entity_cache = None
        if use_cache:
            from .embedding_cache import get_entity_cache
            entity_cache = get_entity_cache()
            
            # Verifica se as entidades já estão em cache
            cache_key = str(sorted(known_entities[:100]))  # Usa primeiras 100 como key
            cached_embeddings = entity_cache.get(cache_key)
            
            if cached_embeddings is not None:
                logger.debug(f"Cache HIT para entidades (size={len(known_entities)})")
        
        best_entity = None
        best_score = 0.0
        
        # Para cada candidato, busca melhor match nas entidades conhecidas
        for candidate in candidates:
            logger.debug(f"Testando candidato: '{candidate}'")
            
            # Gera embedding do candidato (usa cache de queries)
            try:
                candidate_embedding = None
                if use_cache:
                    from .embedding_cache import get_query_cache
                    query_cache = get_query_cache()
                    candidate_embedding = query_cache.get(candidate)
                
                if candidate_embedding is None:
                    candidate_embedding = self._semantic_classifier.model.encode(
                        candidate, 
                        convert_to_tensor=True
                    )
                    if use_cache:
                        query_cache.put(candidate, candidate_embedding)
                
                # Gera embeddings das entidades conhecidas (batch) - com cache
                batch_size = 100
                top_matches = []
                
                for i in range(0, len(known_entities), batch_size):
                    batch = known_entities[i:i+batch_size]
                    
                    # Tenta buscar batch do cache
                    batch_embeddings = None
                    if use_cache and entity_cache:
                        batch_key = str(sorted(batch))
                        batch_embeddings = entity_cache.get(batch_key)
                    
                    # Se não está em cache, calcula
                    if batch_embeddings is None:
                        batch_embeddings = self._semantic_classifier.model.encode(
                            batch,
                            convert_to_tensor=True
                        )
                        
                        # Armazena no cache
                        if use_cache and entity_cache:
                            entity_cache.put(batch_key, batch_embeddings)
                    
                    # Calcula cosine similarity
                    from sentence_transformers import util
                    similarities = util.cos_sim(candidate_embedding, batch_embeddings)[0]
                    
                    # Pega top-K deste batch
                    for j, score in enumerate(similarities):
                        entity = batch[j]
                        top_matches.append((entity, float(score)))
                
                # Ordena todos os matches e pega top-K global
                top_matches.sort(key=lambda x: x[1], reverse=True)
                top_matches = top_matches[:top_k]
                
                # Pega o melhor
                if top_matches:
                    entity, score = top_matches[0]
                    logger.debug(f"  Top match: '{entity}' (score={score:.3f})")
                    
                    if score > best_score:
                        best_score = score
                        best_entity = entity
            
            except Exception as e:
                logger.warning(f"Erro ao calcular similarity para '{candidate}': {e}")
                continue
        
        # Retorna se acima do threshold
        if best_score >= self.threshold:
            logger.info(f"✅ Semantic match: '{best_entity}' (score={best_score:.3f})")
            return best_entity, best_score
        else:
            logger.debug(f"Score {best_score:.3f} abaixo de threshold {self.threshold}")
            return None
    
    def find_best_match_fuzzy(
        self,
        query_text: str,
        known_entities: List[str],
        threshold: int = 80
    ) -> Optional[Tuple[str, float]]:
        """
        Fallback: usa fuzzy matching tradicional.
        
        Args:
            query_text: Texto da query
            known_entities: Lista de entidades conhecidas
            threshold: Threshold fuzzy (0-100)
            
        Returns:
            Tupla (entity, confidence) ou None
        """
        if not known_entities:
            return None
        
        # Extrai candidatos
        candidates = self._extract_entity_candidates(query_text)
        
        if not candidates:
            candidates = [query_text]
        
        best_entity = None
        best_score = 0
        
        for candidate in candidates:
            candidate_lower = candidate.lower().strip()
            
            for entity in known_entities:
                entity_lower = entity.lower().strip()
                
                # Fuzzy ratio
                score = fuzz.ratio(candidate_lower, entity_lower)
                
                # Bonus para substring match (comum em nomes)
                if candidate_lower in entity_lower or entity_lower in candidate_lower:
                    score = min(100, score + 10)
                
                if score > best_score:
                    best_score = score
                    best_entity = entity
        
        if best_score >= threshold:
            confidence = best_score / 100.0
            logger.info(f"✅ Fuzzy match: '{best_entity}' (score={best_score})")
            return best_entity, confidence
        
        return None
    
    def find_best_match_hybrid(
        self,
        query_text: str,
        known_entities: List[str],
        semantic_threshold: float = 0.70,
        fuzzy_threshold: int = 80
    ) -> Optional[Tuple[str, float, str]]:
        """
        Matching híbrido: tenta semantic primeiro, fallback para fuzzy.
        
        Args:
            query_text: Texto da query
            known_entities: Lista de entidades conhecidas
            semantic_threshold: Threshold para semantic matching
            fuzzy_threshold: Threshold para fuzzy matching
            
        Returns:
            Tupla (entity, confidence, method) onde method é 'semantic' ou 'fuzzy'
            ou None se nenhum match
        """
        # 1. Tenta semantic primeiro
        if self._semantic_classifier:
            semantic_result = self.find_best_match_semantic(
                query_text, 
                known_entities, 
                top_k=5
            )
            
            if semantic_result:
                entity, confidence = semantic_result
                return entity, confidence, 'semantic'
        
        # 2. Fallback para fuzzy
        fuzzy_result = self.find_best_match_fuzzy(
            query_text,
            known_entities,
            threshold=fuzzy_threshold
        )
        
        if fuzzy_result:
            entity, confidence = fuzzy_result
            return entity, confidence, 'fuzzy'
        
        # 3. Nenhum match
        logger.debug(f"Nenhum match híbrido encontrado para: '{query_text}'")
        return None


def get_semantic_entity_extractor(nlp, semantic_classifier=None, threshold: float = 0.70):
    """
    Factory para criar SemanticEntityExtractor.
    
    Args:
        nlp: Modelo spaCy
        semantic_classifier: SemanticIntentClassifier (opcional)
        threshold: Threshold para matching semântico
        
    Returns:
        SemanticEntityExtractor
    """
    return SemanticEntityExtractor(nlp, semantic_classifier, threshold)
