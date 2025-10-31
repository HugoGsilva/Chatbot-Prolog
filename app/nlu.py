"""
NLU leve para normalização de entidades (atores, gêneros, etc.).
"""

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