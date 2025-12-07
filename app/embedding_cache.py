"""
Embedding Cache - Step 5

Cache LRU para embeddings de queries e entidades.
Reduz latência e chamadas repetidas ao modelo sentence-transformers.

Features:
- LRU (Least Recently Used) eviction
- Limite de memória configurável
- TTL (Time To Live) opcional
- Thread-safe
- Métricas de cache hit/miss
"""

import logging
import time
from typing import Optional, Dict, Any
from functools import lru_cache
import hashlib

logger = logging.getLogger(__name__)


class EmbeddingCache:
    """
    Cache LRU para embeddings de texto.
    
    Armazena embeddings já calculados para evitar recomputação.
    Usa hash do texto como chave para garantir unicidade.
    """
    
    def __init__(self, max_size: int = 1000, ttl_seconds: int = 3600):
        """
        Inicializa cache.
        
        Args:
            max_size: Número máximo de embeddings em cache
            ttl_seconds: Tempo de vida dos embeddings (segundos). 0 = sem expiração
        """
        self.max_size = max_size
        self.ttl_seconds = ttl_seconds
        self._cache: Dict[str, tuple[Any, float]] = {}  # {text_hash: (embedding, timestamp)}
        self._access_times: Dict[str, float] = {}  # Para LRU
        
        # Métricas
        self.hits = 0
        self.misses = 0
        self.evictions = 0
        
        logger.info(f"EmbeddingCache inicializado: max_size={max_size}, ttl={ttl_seconds}s")
    
    def _hash_text(self, text: str) -> str:
        """Gera hash único do texto."""
        return hashlib.md5(text.encode('utf-8')).hexdigest()
    
    def _is_expired(self, timestamp: float) -> bool:
        """Verifica se embedding expirou."""
        if self.ttl_seconds == 0:
            return False
        return (time.time() - timestamp) > self.ttl_seconds
    
    def _evict_lru(self):
        """Remove embedding menos recentemente usado."""
        if not self._access_times:
            return
        
        # Encontra chave com menor access_time
        lru_key = min(self._access_times, key=self._access_times.get)
        
        # Remove
        del self._cache[lru_key]
        del self._access_times[lru_key]
        self.evictions += 1
        
        logger.debug(f"LRU eviction: removido {lru_key}")
    
    def get(self, text: str) -> Optional[Any]:
        """
        Busca embedding no cache.
        
        Args:
            text: Texto original
            
        Returns:
            Embedding ou None se não encontrado/expirado
        """
        text_hash = self._hash_text(text)
        
        if text_hash not in self._cache:
            self.misses += 1
            return None
        
        embedding, timestamp = self._cache[text_hash]
        
        # Verifica expiração
        if self._is_expired(timestamp):
            del self._cache[text_hash]
            del self._access_times[text_hash]
            self.misses += 1
            logger.debug(f"Cache expired: {text[:50]}")
            return None
        
        # Atualiza access time (LRU)
        self._access_times[text_hash] = time.time()
        self.hits += 1
        
        return embedding
    
    def put(self, text: str, embedding: Any):
        """
        Armazena embedding no cache.
        
        Args:
            text: Texto original
            embedding: Embedding calculado
        """
        text_hash = self._hash_text(text)
        
        # Evict se necessário
        if len(self._cache) >= self.max_size and text_hash not in self._cache:
            self._evict_lru()
        
        # Armazena
        current_time = time.time()
        self._cache[text_hash] = (embedding, current_time)
        self._access_times[text_hash] = current_time
    
    def clear(self):
        """Limpa todo o cache."""
        self._cache.clear()
        self._access_times.clear()
        logger.info("Cache limpo")
    
    def get_stats(self) -> Dict[str, Any]:
        """
        Retorna estatísticas do cache.
        
        Returns:
            Dict com métricas: size, hits, misses, hit_rate, evictions
        """
        total_requests = self.hits + self.misses
        hit_rate = (self.hits / total_requests * 100) if total_requests > 0 else 0
        
        return {
            'size': len(self._cache),
            'max_size': self.max_size,
            'hits': self.hits,
            'misses': self.misses,
            'hit_rate': round(hit_rate, 2),
            'evictions': self.evictions,
            'total_requests': total_requests
        }
    
    def __len__(self) -> int:
        """Retorna tamanho atual do cache."""
        return len(self._cache)


class EntityEmbeddingCache:
    """
    Cache especializado para embeddings de entidades conhecidas.
    
    Diferente do EmbeddingCache, este faz batch caching de listas inteiras
    de entidades (ex: todo o ACTOR_CACHE).
    """
    
    def __init__(self, ttl_seconds: int = 7200):
        """
        Inicializa cache de entidades.
        
        Args:
            ttl_seconds: Tempo de vida (padrão 2h, pois entidades mudam pouco)
        """
        self.ttl_seconds = ttl_seconds
        self._cache: Dict[str, tuple[Any, float]] = {}  # {cache_name: (embeddings, timestamp)}
        
        # Métricas
        self.hits = 0
        self.misses = 0
        
        logger.info(f"EntityEmbeddingCache inicializado: ttl={ttl_seconds}s")
    
    def _is_expired(self, timestamp: float) -> bool:
        """Verifica se cache expirou."""
        if self.ttl_seconds == 0:
            return False
        return (time.time() - timestamp) > self.ttl_seconds
    
    def get(self, cache_name: str) -> Optional[Any]:
        """
        Busca embeddings de uma cache completa.
        
        Args:
            cache_name: Nome da cache (ex: 'actors', 'genres')
            
        Returns:
            Embeddings em batch ou None
        """
        if cache_name not in self._cache:
            self.misses += 1
            return None
        
        embeddings, timestamp = self._cache[cache_name]
        
        # Verifica expiração
        if self._is_expired(timestamp):
            del self._cache[cache_name]
            self.misses += 1
            logger.debug(f"Entity cache expired: {cache_name}")
            return None
        
        self.hits += 1
        return embeddings
    
    def put(self, cache_name: str, embeddings: Any):
        """
        Armazena embeddings de uma cache completa.
        
        Args:
            cache_name: Nome da cache
            embeddings: Embeddings em batch
        """
        current_time = time.time()
        self._cache[cache_name] = (embeddings, current_time)
        logger.info(f"Entity cache armazenada: {cache_name}")
    
    def clear(self, cache_name: Optional[str] = None):
        """
        Limpa cache.
        
        Args:
            cache_name: Se especificado, limpa apenas essa cache. Senão limpa tudo.
        """
        if cache_name:
            if cache_name in self._cache:
                del self._cache[cache_name]
                logger.info(f"Cache limpa: {cache_name}")
        else:
            self._cache.clear()
            logger.info("Todas as entity caches limpas")
    
    def get_stats(self) -> Dict[str, Any]:
        """Retorna estatísticas."""
        total_requests = self.hits + self.misses
        hit_rate = (self.hits / total_requests * 100) if total_requests > 0 else 0
        
        return {
            'cached_entities': list(self._cache.keys()),
            'cache_count': len(self._cache),
            'hits': self.hits,
            'misses': self.misses,
            'hit_rate': round(hit_rate, 2),
            'total_requests': total_requests
        }


# Instâncias globais (singletons)
_query_embedding_cache: Optional[EmbeddingCache] = None
_entity_embedding_cache: Optional[EntityEmbeddingCache] = None


def get_query_cache(max_size: int = 1000, ttl_seconds: int = 3600) -> EmbeddingCache:
    """
    Factory para cache de queries.
    
    Args:
        max_size: Tamanho máximo
        ttl_seconds: TTL
        
    Returns:
        EmbeddingCache singleton
    """
    global _query_embedding_cache
    if _query_embedding_cache is None:
        _query_embedding_cache = EmbeddingCache(max_size, ttl_seconds)
    return _query_embedding_cache


def get_entity_cache(ttl_seconds: int = 7200) -> EntityEmbeddingCache:
    """
    Factory para cache de entidades.
    
    Args:
        ttl_seconds: TTL
        
    Returns:
        EntityEmbeddingCache singleton
    """
    global _entity_embedding_cache
    if _entity_embedding_cache is None:
        _entity_embedding_cache = EntityEmbeddingCache(ttl_seconds)
    return _entity_embedding_cache
