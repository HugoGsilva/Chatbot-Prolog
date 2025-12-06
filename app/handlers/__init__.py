"""
Handlers Package - Handlers modulares para intenções do chatbot

Este pacote contém handlers organizados por categoria:
- info_handlers: Ajuda, saudações, identidade
- search_handlers: Busca por ator, gênero, diretor
- query_handlers: Consultas sobre filmes específicos
- recommendation_handlers: Recomendações e filmes aleatórios
- filter_handlers: Filtros por ano e contagens
"""

from .info_handlers import InfoHandlers
from .search_handlers import SearchHandlers
from .query_handlers import QueryHandlers
from .recommendation_handlers import RecommendationHandlers
from .filter_handlers import FilterHandlers

__all__ = [
    "InfoHandlers",
    "SearchHandlers",
    "QueryHandlers",
    "RecommendationHandlers",
    "FilterHandlers",
]
