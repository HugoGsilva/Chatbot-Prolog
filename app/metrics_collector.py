"""
Metrics Collector - Step 5

Coleta e agrega métricas de performance do sistema NLU:
- Latência de classificação (intent e entity)
- Taxa de fallback (semantic → keyword)
- Cache hit rate
- Accuracy estimada (baseada em confidence)
- A/B testing results
"""

import logging
import time
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from datetime import datetime
import statistics

logger = logging.getLogger(__name__)


@dataclass
class ClassificationMetric:
    """Métrica de uma classificação individual."""
    timestamp: float
    intent: str
    confidence: float
    method: str  # 'semantic', 'keyword', 'hybrid'
    latency_ms: float
    used_cache: bool = False
    fallback: bool = False  # True se fez fallback para keyword


@dataclass
class EntityExtractionMetric:
    """Métrica de extração de entidade."""
    timestamp: float
    entity_type: str
    method: str  # 'semantic', 'fuzzy', 'hybrid'
    latency_ms: float
    confidence: float
    used_cache: bool = False


class MetricsCollector:
    """
    Coletor de métricas para monitoramento de performance.
    
    Armazena métricas em memória com limite de retenção.
    Calcula estatísticas agregadas on-demand.
    """
    
    def __init__(self, max_metrics: int = 10000):
        """
        Inicializa coletor.
        
        Args:
            max_metrics: Número máximo de métricas retidas em memória
        """
        self.max_metrics = max_metrics
        
        # Métricas de classificação
        self.classification_metrics: List[ClassificationMetric] = []
        
        # Métricas de entidades
        self.entity_metrics: List[EntityExtractionMetric] = []
        
        # Contadores A/B testing
        self.ab_test_results: Dict[str, Dict[str, int]] = {
            'semantic': {'success': 0, 'failure': 0},
            'keyword': {'success': 0, 'failure': 0}
        }
        
        logger.info(f"MetricsCollector inicializado: max_metrics={max_metrics}")
    
    def record_classification(
        self,
        intent: str,
        confidence: float,
        method: str,
        latency_ms: float,
        used_cache: bool = False,
        fallback: bool = False
    ):
        """
        Registra métrica de classificação de intent.
        
        Args:
            intent: Intent detectada
            confidence: Confiança (0.0-1.0)
            method: Método usado ('semantic', 'keyword', 'hybrid')
            latency_ms: Latência em ms
            used_cache: Se usou cache de embeddings
            fallback: Se fez fallback para keyword
        """
        metric = ClassificationMetric(
            timestamp=time.time(),
            intent=intent,
            confidence=confidence,
            method=method,
            latency_ms=latency_ms,
            used_cache=used_cache,
            fallback=fallback
        )
        
        self.classification_metrics.append(metric)
        
        # Limita tamanho
        if len(self.classification_metrics) > self.max_metrics:
            self.classification_metrics.pop(0)
        
        logger.debug(f"Metric recorded: {intent} ({method}, {latency_ms:.1f}ms)")
    
    def record_entity_extraction(
        self,
        entity_type: str,
        method: str,
        latency_ms: float,
        confidence: float = 1.0,
        used_cache: bool = False
    ):
        """
        Registra métrica de extração de entidade.
        
        Args:
            entity_type: Tipo (actor, director, genre, film)
            method: Método ('semantic', 'fuzzy', 'hybrid')
            latency_ms: Latência em ms
            confidence: Confiança do match
            used_cache: Se usou cache
        """
        metric = EntityExtractionMetric(
            timestamp=time.time(),
            entity_type=entity_type,
            method=method,
            latency_ms=latency_ms,
            confidence=confidence,
            used_cache=used_cache
        )
        
        self.entity_metrics.append(metric)
        
        # Limita tamanho
        if len(self.entity_metrics) > self.max_metrics:
            self.entity_metrics.pop(0)
    
    def record_ab_test_result(self, method: str, success: bool):
        """
        Registra resultado de A/B test.
        
        Args:
            method: 'semantic' ou 'keyword'
            success: Se foi bem-sucedido (confidence >= threshold)
        """
        if method in self.ab_test_results:
            key = 'success' if success else 'failure'
            self.ab_test_results[method][key] += 1
    
    def get_classification_stats(self, last_n: Optional[int] = None) -> Dict[str, Any]:
        """
        Calcula estatísticas de classificação.
        
        Args:
            last_n: Se especificado, considera apenas últimas N métricas
            
        Returns:
            Dict com estatísticas agregadas
        """
        metrics = self.classification_metrics[-last_n:] if last_n else self.classification_metrics
        
        if not metrics:
            return {
                'total_classifications': 0,
                'avg_latency_ms': 0,
                'avg_confidence': 0,
                'method_distribution': {},
                'fallback_rate': 0,
                'cache_hit_rate': 0
            }
        
        # Latências
        latencies = [m.latency_ms for m in metrics]
        
        # Confidências
        confidences = [m.confidence for m in metrics]
        
        # Distribuição de métodos
        method_counts = {}
        for m in metrics:
            method_counts[m.method] = method_counts.get(m.method, 0) + 1
        
        # Taxa de fallback
        fallbacks = sum(1 for m in metrics if m.fallback)
        fallback_rate = (fallbacks / len(metrics) * 100) if metrics else 0
        
        # Cache hit rate
        cache_hits = sum(1 for m in metrics if m.used_cache)
        cache_hit_rate = (cache_hits / len(metrics) * 100) if metrics else 0
        
        # Intents mais comuns
        intent_counts = {}
        for m in metrics:
            intent_counts[m.intent] = intent_counts.get(m.intent, 0) + 1
        top_intents = sorted(intent_counts.items(), key=lambda x: x[1], reverse=True)[:5]
        
        return {
            'total_classifications': len(metrics),
            'avg_latency_ms': round(statistics.mean(latencies), 2),
            'p50_latency_ms': round(statistics.median(latencies), 2),
            'p95_latency_ms': round(statistics.quantiles(latencies, n=20)[18], 2) if len(latencies) >= 20 else 0,
            'p99_latency_ms': round(statistics.quantiles(latencies, n=100)[98], 2) if len(latencies) >= 100 else 0,
            'avg_confidence': round(statistics.mean(confidences), 3),
            'method_distribution': method_counts,
            'fallback_rate': round(fallback_rate, 2),
            'cache_hit_rate': round(cache_hit_rate, 2),
            'top_intents': dict(top_intents)
        }
    
    def get_entity_stats(self, last_n: Optional[int] = None) -> Dict[str, Any]:
        """
        Calcula estatísticas de extração de entidades.
        
        Args:
            last_n: Se especificado, considera apenas últimas N métricas
            
        Returns:
            Dict com estatísticas
        """
        metrics = self.entity_metrics[-last_n:] if last_n else self.entity_metrics
        
        if not metrics:
            return {
                'total_extractions': 0,
                'avg_latency_ms': 0,
                'method_distribution': {},
                'cache_hit_rate': 0
            }
        
        latencies = [m.latency_ms for m in metrics]
        confidences = [m.confidence for m in metrics]
        
        method_counts = {}
        for m in metrics:
            method_counts[m.method] = method_counts.get(m.method, 0) + 1
        
        entity_type_counts = {}
        for m in metrics:
            entity_type_counts[m.entity_type] = entity_type_counts.get(m.entity_type, 0) + 1
        
        cache_hits = sum(1 for m in metrics if m.used_cache)
        cache_hit_rate = (cache_hits / len(metrics) * 100) if metrics else 0
        
        return {
            'total_extractions': len(metrics),
            'avg_latency_ms': round(statistics.mean(latencies), 2),
            'p50_latency_ms': round(statistics.median(latencies), 2),
            'avg_confidence': round(statistics.mean(confidences), 3),
            'method_distribution': method_counts,
            'entity_type_distribution': entity_type_counts,
            'cache_hit_rate': round(cache_hit_rate, 2)
        }
    
    def get_ab_test_stats(self) -> Dict[str, Any]:
        """
        Calcula estatísticas de A/B testing.
        
        Returns:
            Dict com resultados comparativos
        """
        semantic = self.ab_test_results['semantic']
        keyword = self.ab_test_results['keyword']
        
        total_semantic = semantic['success'] + semantic['failure']
        total_keyword = keyword['success'] + keyword['failure']
        
        semantic_success_rate = (semantic['success'] / total_semantic * 100) if total_semantic > 0 else 0
        keyword_success_rate = (keyword['success'] / total_keyword * 100) if total_keyword > 0 else 0
        
        return {
            'semantic': {
                'total': total_semantic,
                'success': semantic['success'],
                'failure': semantic['failure'],
                'success_rate': round(semantic_success_rate, 2)
            },
            'keyword': {
                'total': total_keyword,
                'success': keyword['success'],
                'failure': keyword['failure'],
                'success_rate': round(keyword_success_rate, 2)
            },
            'improvement': round(semantic_success_rate - keyword_success_rate, 2)
        }
    
    def get_all_stats(self, last_n: Optional[int] = None) -> Dict[str, Any]:
        """
        Retorna todas as estatísticas agregadas.
        
        Args:
            last_n: Janela de métricas (None = todas)
            
        Returns:
            Dict com todas as stats
        """
        return {
            'classification': self.get_classification_stats(last_n),
            'entity_extraction': self.get_entity_stats(last_n),
            'ab_testing': self.get_ab_test_stats(),
            'timestamp': datetime.now().isoformat()
        }
    
    def clear(self):
        """Limpa todas as métricas."""
        self.classification_metrics.clear()
        self.entity_metrics.clear()
        self.ab_test_results = {
            'semantic': {'success': 0, 'failure': 0},
            'keyword': {'success': 0, 'failure': 0}
        }
        logger.info("Métricas limpas")


# Instância global (singleton)
_metrics_collector: Optional[MetricsCollector] = None


def get_metrics_collector(max_metrics: int = 10000) -> MetricsCollector:
    """
    Factory para MetricsCollector.
    
    Args:
        max_metrics: Limite de métricas
        
    Returns:
        MetricsCollector singleton
    """
    global _metrics_collector
    if _metrics_collector is None:
        _metrics_collector = MetricsCollector(max_metrics)
    return _metrics_collector
