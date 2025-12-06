"""
Filter Handlers - Handlers para filtros e contagens

Handlers para filtragem por ano e contagem de filmes.
"""

from typing import Dict

from .base_handler import BaseHandler
from ..schemas import ChatResponse, ResponseType
from ..nlu import find_best_genre
from ..prolog_service import PrologTimeoutError


class FilterHandlers(BaseHandler):
    """Handlers para filtros e contagens."""
    
    async def handle_filmes_por_ano(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'filmes_por_ano' - filmes de 2020."""
        ano = entities.get("ano", "") or entities.get("year", "")
        
        if not ano:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o ano. Pode reformular?",
                suggestions=["filmes de 2020", "filmes de 2019"],
            )
        
        # Consulta Prolog para buscar filmes do ano
        try:
            query_string = f"imdb_kb:netflix_title(_, TituloFilme, {ano})"
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei filmes do ano {ano}.",
                suggestions=["filmes de 2020", "filme aleatório"],
            )
        
        # Limita a 20 resultados
        filmes = [{"titulo": r["TituloFilme"]} for r in results[:20]]
        total = len(results)
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=filmes,
            suggestions=[f"filmes de {int(ano)-1}", f"filmes de {int(ano)+1}"] if ano.isdigit() else [],
            metadata={"total_encontrados": total, "exibindo": len(filmes)}
        )
    
    async def handle_contar_filmes(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'contar_filmes'."""
        genero = entities.get("genero", "")
        ano = entities.get("ano", "")
        
        if not genero:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o gênero para a contagem.",
                suggestions=["quantos filmes de ação", "contar filmes de drama em 2020"],
            )
        
        # Normaliza gênero
        best_genre = find_best_genre(genero)
        
        if not best_genre:
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"Não encontrei o gênero '{genero}'.",
            )
        
        genre_query = best_genre.upper()
        
        if ano:
            # Contagem por gênero e ano
            query_string = f"imdb_rules:contar_filmes_por_genero_e_ano('{genre_query}', {ano}, Contagem)"
        else:
            # Contagem apenas por gênero (sem ano)
            query_string = f"imdb_rules:contar_filmes_por_genero('{genre_query}', Contagem)"
        
        try:
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results or "Contagem" not in results[0]:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Erro ao realizar a contagem.",
            )
        
        contagem = results[0]["Contagem"]
        
        if ano:
            message = f"Encontrei **{contagem}** filmes de {best_genre} em {ano}."
        else:
            message = f"Encontrei **{contagem}** filmes de {best_genre}."
        
        return ChatResponse(
            type=ResponseType.TEXT,
            content=message,
        )
    
    async def handle_filmes_com_filtros(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'filmes_com_filtros' - consultas complexas."""
        # TODO: Implementar lógica para múltiplos filtros
        return ChatResponse(
            type=ResponseType.TEXT,
            content="Funcionalidade de filtros múltiplos ainda não implementada.",
            suggestions=["filmes de ação", "filmes do Tom Hanks"],
        )
