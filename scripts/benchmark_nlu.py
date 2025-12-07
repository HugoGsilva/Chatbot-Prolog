#!/usr/bin/env python3
"""
Benchmark NLU Performance

Compara performance entre diferentes modos de NLU:
- Keyword-based (tradicional)
- Semantic-based (embeddings)
- Hybrid (semantic + keyword validation)

Métricas avaliadas:
- Latência (P50, P95, P99)
- Acurácia (intent correto)
- Cache hit rate
- Fallback rate
"""

import sys
import os
import json
import time
import statistics
from typing import List, Dict, Tuple
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from app.nlu_engine import NLUEngine
from app.config import settings


# Sample queries para benchmark (intent esperado, query)
BENCHMARK_QUERIES = [
    # filmes_por_ator
    ("filmes_por_ator", "filmes com tom cruise"),
    ("filmes_por_ator", "quais filmes tem morgan freeman"),
    ("filmes_por_ator", "mostre os filmes de leonardo dicaprio"),
    ("filmes_por_ator", "lista de filmes com brad pitt"),
    
    # filmes_por_genero
    ("filmes_por_genero", "filmes de ação"),
    ("filmes_por_genero", "me mostre comedias"),
    ("filmes_por_genero", "quero ver um terror"),
    ("filmes_por_genero", "lista de filmes de drama"),
    
    # filmes_por_diretor
    ("filmes_por_diretor", "filmes dirigidos por spielberg"),
    ("filmes_por_diretor", "quais filmes do diretor nolan"),
    ("filmes_por_diretor", "mostre obras de tarantino"),
    ("filmes_por_diretor", "lista de filmes de scorsese"),
    
    # recomendar_filme
    ("recomendar_filme", "me recomenda um filme bom"),
    ("recomendar_filme", "sugere algo pra assistir"),
    ("recomendar_filme", "quero uma recomendação"),
    ("recomendar_filme", "indica um filme legal"),
    
    # filme_aleatorio
    ("filme_aleatorio", "filme aleatorio"),
    ("filme_aleatorio", "escolhe um filme qualquer pra mim"),
    ("filme_aleatorio", "surpresa me"),
    
    # info intents
    ("diretor_do_filme", "quem dirigiu inception"),
    ("atores_do_filme", "quem atuou em pulp fiction"),
    ("genero_do_filme", "qual o genero de matrix"),
    
    # utility
    ("ajuda", "ajuda"),
    ("ajuda", "help"),
    ("saudacao", "oi"),
    ("saudacao", "olá"),
    ("contar_filmes", "quantos filmes tem na base"),
]


class BenchmarkResult:
    """Resultado de um benchmark de modo específico."""
    
    def __init__(self, mode: str):
        self.mode = mode
        self.latencies: List[float] = []
        self.correct_intents: int = 0
        self.total_queries: int = 0
        self.fallback_count: int = 0
        
    def add_result(self, latency_ms: float, correct: bool, used_fallback: bool = False):
        """Adiciona resultado de uma query."""
        self.latencies.append(latency_ms)
        self.total_queries += 1
        if correct:
            self.correct_intents += 1
        if used_fallback:
            self.fallback_count += 1
    
    def get_stats(self) -> Dict:
        """Retorna estatísticas agregadas."""
        if not self.latencies:
            return {}
        
        sorted_latencies = sorted(self.latencies)
        n = len(sorted_latencies)
        
        return {
            "mode": self.mode,
            "total_queries": self.total_queries,
            "accuracy": self.correct_intents / self.total_queries if self.total_queries > 0 else 0.0,
            "fallback_rate": self.fallback_count / self.total_queries if self.total_queries > 0 else 0.0,
            "latency_avg_ms": statistics.mean(self.latencies),
            "latency_median_ms": statistics.median(self.latencies),
            "latency_p95_ms": sorted_latencies[int(n * 0.95)] if n > 0 else 0,
            "latency_p99_ms": sorted_latencies[int(n * 0.99)] if n > 0 else 0,
            "latency_min_ms": min(self.latencies),
            "latency_max_ms": max(self.latencies),
        }


def run_benchmark_mode(mode: str, queries: List[Tuple[str, str]], warmup_rounds: int = 2) -> BenchmarkResult:
    """
    Executa benchmark para um modo específico.
    
    Args:
        mode: "keyword", "semantic", ou "hybrid"
        queries: Lista de (expected_intent, query_text)
        warmup_rounds: Número de warmup passes antes do benchmark real
        
    Returns:
        BenchmarkResult com estatísticas
    """
    print(f"\n{'='*60}")
    print(f"  Benchmark Mode: {mode.upper()}")
    print(f"{'='*60}")
    
    # Configurar NLU engine
    use_semantic = mode in ["semantic", "hybrid"]
    
    # Se hybrid, ajustar thresholds
    if mode == "hybrid":
        original_threshold = settings.SEMANTIC_INTENT_THRESHOLD
        settings.SEMANTIC_INTENT_THRESHOLD = 0.75  # High confidence threshold
    
    nlu = NLUEngine(use_semantic=use_semantic)
    
    # Warmup (para carregar modelos e cache)
    print(f"\n[Warmup] Executando {warmup_rounds} rodadas de warmup...")
    for _ in range(warmup_rounds):
        for _, query in queries[:5]:  # Só primeiras 5 para warmup
            nlu.process(query)
    
    # Benchmark real
    print(f"\n[Benchmark] Processando {len(queries)} queries...")
    result = BenchmarkResult(mode)
    
    for i, (expected_intent, query) in enumerate(queries, 1):
        start = time.perf_counter()
        nlu_result = nlu.process(query)
        latency_ms = (time.perf_counter() - start) * 1000
        
        detected_intent = nlu_result.get("intent", "unknown")
        confidence = nlu_result.get("confidence", 0.0)
        
        correct = detected_intent == expected_intent
        used_fallback = confidence < 0.60 and use_semantic
        
        result.add_result(latency_ms, correct, used_fallback)
        
        status = "✅" if correct else "❌"
        print(f"  [{i:2d}/{len(queries)}] {status} {query[:40]:<40} → {detected_intent} ({latency_ms:.1f}ms)")
    
    # Restaurar config original
    if mode == "hybrid":
        settings.SEMANTIC_INTENT_THRESHOLD = original_threshold
    
    return result


def print_comparison_table(results: List[BenchmarkResult]):
    """Imprime tabela comparativa de resultados."""
    print(f"\n{'='*80}")
    print("  COMPARISON TABLE")
    print(f"{'='*80}")
    
    # Header
    print(f"\n{'Metric':<25} {'Keyword':>12} {'Semantic':>12} {'Hybrid':>12} {'Winner':>12}")
    print("-" * 80)
    
    # Organiza resultados por modo
    stats_by_mode = {r.mode: r.get_stats() for r in results}
    
    # Métricas para comparar
    metrics = [
        ("accuracy", "Accuracy", "%", "higher"),
        ("latency_median_ms", "Latency (P50)", "ms", "lower"),
        ("latency_p95_ms", "Latency (P95)", "ms", "lower"),
        ("latency_p99_ms", "Latency (P99)", "ms", "lower"),
        ("fallback_rate", "Fallback Rate", "%", "lower"),
    ]
    
    for metric_key, metric_name, unit, better in metrics:
        values = {}
        for mode in ["keyword", "semantic", "hybrid"]:
            if mode in stats_by_mode:
                val = stats_by_mode[mode].get(metric_key, 0)
                if unit == "%":
                    val *= 100
                values[mode] = val
        
        # Determina winner
        if better == "higher":
            winner = max(values, key=values.get) if values else "-"
        else:
            winner = min(values, key=values.get) if values else "-"
        
        # Formata valores
        if unit == "%":
            kw_str = f"{values.get('keyword', 0):.1f}%" if 'keyword' in values else "-"
            sem_str = f"{values.get('semantic', 0):.1f}%" if 'semantic' in values else "-"
            hyb_str = f"{values.get('hybrid', 0):.1f}%" if 'hybrid' in values else "-"
        else:  # ms
            kw_str = f"{values.get('keyword', 0):.1f}ms" if 'keyword' in values else "-"
            sem_str = f"{values.get('semantic', 0):.1f}ms" if 'semantic' in values else "-"
            hyb_str = f"{values.get('hybrid', 0):.1f}ms" if 'hybrid' in values else "-"
        
        print(f"{metric_name:<25} {kw_str:>12} {sem_str:>12} {hyb_str:>12} {winner:>12}")
    
    print("\n" + "="*80)


def main():
    """Executa benchmark completo."""
    print("\n" + "="*80)
    print("  NLU PERFORMANCE BENCHMARK")
    print("="*80)
    print(f"\nTotal queries: {len(BENCHMARK_QUERIES)}")
    print(f"Warmup rounds: 2")
    print(f"Modes: keyword, semantic, hybrid")
    
    results = []
    
    # 1. Keyword mode
    try:
        result = run_benchmark_mode("keyword", BENCHMARK_QUERIES)
        results.append(result)
        print(f"\n[✅] Keyword benchmark completed")
    except Exception as e:
        print(f"\n[❌] Keyword benchmark failed: {e}")
    
    # 2. Semantic mode
    try:
        result = run_benchmark_mode("semantic", BENCHMARK_QUERIES)
        results.append(result)
        print(f"\n[✅] Semantic benchmark completed")
    except Exception as e:
        print(f"\n[❌] Semantic benchmark failed: {e}")
    
    # 3. Hybrid mode
    try:
        result = run_benchmark_mode("hybrid", BENCHMARK_QUERIES)
        results.append(result)
        print(f"\n[✅] Hybrid benchmark completed")
    except Exception as e:
        print(f"\n[❌] Hybrid benchmark failed: {e}")
    
    # Comparison table
    if len(results) > 1:
        print_comparison_table(results)
    
    # Individual stats
    print(f"\n{'='*80}")
    print("  DETAILED STATISTICS")
    print(f"{'='*80}")
    
    for result in results:
        stats = result.get_stats()
        print(f"\n{stats['mode'].upper()} Mode:")
        print(f"  Accuracy:      {stats['accuracy']*100:.1f}%")
        print(f"  Fallback Rate: {stats['fallback_rate']*100:.1f}%")
        print(f"  Latency (avg): {stats['latency_avg_ms']:.1f}ms")
        print(f"  Latency (P50): {stats['latency_median_ms']:.1f}ms")
        print(f"  Latency (P95): {stats['latency_p95_ms']:.1f}ms")
        print(f"  Latency (P99): {stats['latency_p99_ms']:.1f}ms")
        print(f"  Latency (min): {stats['latency_min_ms']:.1f}ms")
        print(f"  Latency (max): {stats['latency_max_ms']:.1f}ms")
    
    # Save results to JSON
    output_file = Path(__file__).parent.parent / "benchmark_results.json"
    with open(output_file, "w") as f:
        json.dump([r.get_stats() for r in results], f, indent=2)
    
    print(f"\n[💾] Results saved to: {output_file}")
    print("\n" + "="*80 + "\n")


if __name__ == "__main__":
    main()
