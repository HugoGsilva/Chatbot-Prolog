"""
NLU leve para normalização de entidades e fuzzy matching.

Inclui funções de normalização e helpers para encontrar a melhor
correspondência usando thefuzz contra caches carregadas no startup.
"""

from thefuzz import process

from typing import Optional
import json

# --- CACHES NLU GLOBAIS ---
# (Definidas aqui, mas populadas pelo main.py no startup)
ACTOR_CACHE: list[str] = []
GENRE_CACHE: list[str] = []
FILM_CACHE: list[str] = []

# ---------------------------------------------------------------------------
# Tradução PT-BR de Géneros
# ---------------------------------------------------------------------------
# Mapeia a forma PT (normalizada/maiúscula) para a forma EN (que o Prolog espera)
GENRE_TRANSLATION_MAP = {
    "AÇÃO": "Action",
    "ANIMAÇÃO": "Animation",
    "INFANTIL": "Children",
    "CLÁSSICOS": "Classics",
    "COMÉDIA": "Comedy",
    "DOCUMENTÁRIO": "Documentary",
    "DRAMA": "Drama",
    "FAMÍLIA": "Family",
    "ESTRANGEIRO": "Foreign",
    "JOGOS": "Games",
    "TERROR": "Horror",
    "MÚSICA": "Music",
    "NOVO": "New",
    "FICÇÃO CIENTÍFICA": "Sci-Fi",
    "DESPORTO": "Sports",
    "VIAGEM": "Travel",
}

# Criamos uma cache SÓ com as chaves em Português para o fuzzy matching
# Resultado: ["AÇÃO", "ANIMAÇÃO", "INFANTIL", ...]
GENRE_CACHE_PT = list(GENRE_TRANSLATION_MAP.keys())

def normalize_actor_name(name: str) -> str:
    """Normaliza o nome do ator para o formato esperado pelo Prolog.

    Estratégia: converter para maiúsculas. Futuras melhorias podem incluir
    remoção de acentos, trimming, e regras de padronização adicionais.
    """
    return name.upper()


def normalize_genre_name(name: str) -> str:
    """Normaliza o nome do gênero para o formato esperado pelo Prolog.

    Estratégia: capitalizar a primeira letra e manter o restante minúsculo,
    alinhando com valores como "Action", "Comedy", "Sci-Fi".
    """
    return name.capitalize()


def normalize_film_title(title: str) -> str:
    """Normaliza o título do filme para o formato esperado pelo Prolog.

    Estratégia: converter para maiúsculas, alinhando com fatos como
    "ACADEMY DINOSAUR" no Sakila.
    """
    return title.upper()


# ---------------------------------------------------------------------------
# Fuzzy Matching Helpers
# ---------------------------------------------------------------------------

def find_best_match(query: str, cache: list[str], threshold: int = 75) -> Optional[str]:
    """Retorna o melhor match para `query` dentro de `cache` se a pontuação
    atingir o `threshold`; caso contrário, retorna None.

    - Normaliza apenas para comparação (uppercase) antes do matching.
    - Usa `process.extractOne` que devolve `(string_encontrada, pontuação)`.
    """
    if not query or not cache:
        return None

    result = process.extractOne(query.upper(), cache)
    if not result:
        return None

    value, score = result[0], result[1]
    return value if score >= threshold else None


def find_best_actor(query: str) -> Optional[str]:
    """Wrapper específico para atores: consulta a cache global de atores."""
    # Usa a ACTOR_CACHE global definida neste ficheiro
    return find_best_match(query, ACTOR_CACHE)


def find_best_genre(query: str) -> Optional[str]:
    """
    Wrapper bilíngue para géneros.
    1. Tenta um match fuzzy contra a cache PT (GENRE_CACHE_PT).
    2. Se encontrar, traduz para EN (usando GENRE_TRANSLATION_MAP).
    3. Se falhar, tenta um match fuzzy contra a cache EN (GENRE_CACHE) como fallback.
    """
    # Normalizar a query para comparação
    query_upper = query.upper()

    # --- Nível 1: Tentar Match em Português ---
    best_pt_match = find_best_match(query_upper, GENRE_CACHE_PT, threshold=75)

    if best_pt_match:
        # Traduz para o nome que o Prolog espera (EN)
        return GENRE_TRANSLATION_MAP.get(best_pt_match)

    # --- Nível 2: Fallback para Match em Inglês ---
    # Usa a GENRE_CACHE global definida neste ficheiro
    best_en_match = find_best_match(query_upper, GENRE_CACHE, threshold=75)

    return best_en_match


def find_best_film(query: str) -> Optional[str]:
    """Wrapper específico para títulos de filmes: usa a cache global de
    títulos carregada no startup da API para fuzzy matching.
    """
    # Usa a FILM_CACHE global definida neste ficheiro
    return find_best_match(query, FILM_CACHE)