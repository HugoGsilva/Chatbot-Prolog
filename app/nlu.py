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
    Wrapper bilíngue e robusto para géneros.
    - Tenta primeiro um match fuzzy em PT e traduz para EN.
    - Em seguida, normaliza para o label EXATO do KB (GENRE_CACHE) via fuzzy.
    - Se PT falhar, tenta diretamente contra o KB em EN.
    Retorna SEMPRE o label exatamente como existe no KB (tipicamente UPPERCASE),
    garantindo compatibilidade com as regras Prolog.
    """
    query_upper = query.upper()

    # 1) Tenta em Português e traduz para EN
    best_pt = find_best_match(query_upper, GENRE_CACHE_PT, threshold=75)
    if best_pt:
        translated_en = GENRE_TRANSLATION_MAP.get(best_pt)
        if translated_en:
            # 1.1) Mapear para label EXATO do KB (GENRE_CACHE)
            # Usa threshold mais tolerante para variações (singular/plural, sufixos)
            kb_label = find_best_match(translated_en.upper(), GENRE_CACHE, threshold=60)
            if kb_label:
                return kb_label
            # Se não encontrar no KB, devolve a tradução (melhor esforço)
            return translated_en

    # 2) Fallback: procurar diretamente no KB com a query (EN ou PT uppercase)
    best_en = find_best_match(query_upper, GENRE_CACHE, threshold=60)
    return best_en


def find_best_film(query: str) -> Optional[str]:
    """Wrapper específico para títulos de filmes: usa a cache global de
    títulos carregada no startup da API para fuzzy matching.
    """
    # Usa a FILM_CACHE global definida neste ficheiro
    return find_best_match(query, FILM_CACHE)