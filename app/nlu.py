"""
NLU leve para normalização de entidades (atores, gêneros, etc.).
"""

def normalize_actor_name(name: str) -> str:
    """Normaliza o nome do ator para o formato esperado pelo Prolog.

    Estratégia: converter para maiúsculas. Futuras melhorias podem incluir
    remoção de acentos, trimming, e regras de padronização adicionais.
    """
    return name.upper()