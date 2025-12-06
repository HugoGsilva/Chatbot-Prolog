"""
Search Handlers - Handlers para busca de filmes

Handlers para buscas por ator, gênero e diretor.
"""

from typing import Dict

from .base_handler import BaseHandler
from ..schemas import ChatResponse, ResponseType
from ..nlu import find_best_actor, find_best_genre, find_best_director
from ..prolog_service import PrologTimeoutError


class SearchHandlers(BaseHandler):
    """Handlers para busca de filmes."""
    
    async def handle_filmes_por_ator(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """
        Handler para intenção 'filmes_por_ator'.
        
        Busca filmes de um ator usando Prolog.
        """
        ator = entities.get("ator", "")
        
        if not ator:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o nome do ator. Pode reformular?",
                suggestions=["filmes por Tom Hanks", "filmes do Brad Pitt"],
            )
        
        # Normaliza o nome do ator
        best_match = find_best_actor(ator)
        
        if not best_match:
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"Não encontrei o ator '{ator}' na base de dados.",
                suggestions=[f"filmes por {ator.title()}", "filmes de ação"],
            )
        
        # Consulta Prolog com timeout
        try:
            query_string = f"imdb_rules:filmes_por_ator('{best_match}', TituloFilme)"
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei filmes para o ator '{best_match}'.",
            )
        
        # Formata resposta
        filmes = [{"titulo": r["TituloFilme"]} for r in results]
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=filmes,
            suggestions=["filmes de drama", f"gênero de {filmes[0]['titulo']}" if filmes else None],
        )
    
    async def handle_filmes_por_genero(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'filmes_por_genero'."""
        genero = entities.get("genero", "")
        
        if not genero:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o gênero. Pode reformular?",
                suggestions=["filmes de ação", "filmes de comédia", "filmes de drama"],
            )
        
        # Normaliza o gênero
        best_match = find_best_genre(genero)
        
        if not best_match:
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"Não encontrei o gênero '{genero}' na base de dados.",
                suggestions=["filmes de ação", "filmes de drama"],
            )
        
        # Consulta Prolog com timeout
        try:
            genre_query = best_match.upper()
            query_string = f"imdb_rules:filmes_por_genero('{genre_query}', TituloFilme)"
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei filmes do gênero '{best_match}'.",
            )
        
        filmes = [{"titulo": r["TituloFilme"]} for r in results]
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=filmes,
        )
    
    async def handle_filmes_por_diretor(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'filmes_por_diretor'."""
        diretor = entities.get("diretor", "")
        
        if not diretor:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o nome do diretor. Pode reformular?",
                suggestions=["filmes do diretor Spielberg", "filmes do diretor Nolan"],
            )
        
        # Normaliza o nome do diretor
        best_match = find_best_director(diretor)
        
        if not best_match:
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"Não encontrei o diretor '{diretor}' na base de dados.",
                suggestions=["filmes do diretor Spielberg"],
            )
        
        # Consulta Prolog com timeout
        try:
            director_query = best_match.upper()
            query_string = f"imdb_rules:filmes_por_diretor('{director_query}', TituloFilme)"
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei filmes do diretor '{best_match}'.",
            )
        
        filmes = [{"titulo": r["TituloFilme"]} for r in results]
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=filmes,
        )
