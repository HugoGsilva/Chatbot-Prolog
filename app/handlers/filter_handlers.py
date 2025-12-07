"""
Filter Handlers - Handlers para filtros e contagens

Handlers para filtragem por ano e contagem de filmes.
"""

from typing import Dict

from .base_handler import BaseHandler
from ..schemas import ChatResponse, ResponseType
from ..nlu import find_best_genre, find_best_actor
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
        
        # Consulta Prolog para buscar filmes do ano (apenas Movies)
        try:
            query_string = f"imdb_kb:netflix_title(_, TituloFilme, {ano}, 'Movie')"
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
    
    async def handle_filmes_por_genero(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'filmes_por_genero' - filmes de ação, comédia, etc."""
        genero = entities.get("genero", "")
        
        if not genero:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o gênero. Pode reformular?",
                suggestions=["filmes de ação", "filmes de comédia", "filmes de terror"],
            )
        
        # Normaliza gênero
        best_genre = find_best_genre(genero)
        
        if not best_genre:
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"Não encontrei o gênero '{genero}'. Tente: ação, comédia, drama, terror, suspense, romance.",
                suggestions=["filmes de ação", "filmes de comédia", "filmes de drama"],
            )
        
        genre_query = best_genre.upper()
        
        # Consulta Prolog para buscar filmes do gênero
        try:
            query_string = f"imdb_rules:filmes_por_genero('{genre_query}', Titulo)"
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei filmes de {best_genre}.",
                suggestions=["filmes de ação", "filme aleatório"],
            )
        
        # Limita a 20 resultados
        filmes = [{"titulo": r["Titulo"]} for r in results[:20]]
        total = len(results)
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=filmes,
            suggestions=[f"filmes de {best_genre}", "filme aleatório"],
            metadata={"total_encontrados": total, "exibindo": len(filmes), "genero": best_genre}
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
            message = f"📊 Encontrei **{contagem}** filmes de **{best_genre}** em **{ano}**."
            suggestions = [f"filmes de {best_genre} em {ano}", "filme aleatório"]
        else:
            message = f"📊 Encontrei **{contagem}** filmes de **{best_genre}**."
            suggestions = [f"filmes de {best_genre}", "filme aleatório"]
        
        return ChatResponse(
            type=ResponseType.TEXT,
            content=message,
            suggestions=suggestions,
        )
    
    async def handle_filmes_com_filtros(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'filmes_com_filtros' - consultas complexas."""
        ator = entities.get("ator", "")
        genero = entities.get("genero", "")
        
        # Caso 1: Ator e Gênero (Ex: "filmes do Adam Sandler de comédia")
        if ator and genero:
            # Normaliza entidades
            best_actor = find_best_actor(ator)
            best_genre = find_best_genre(genero)
            
            if not best_actor or not best_genre:
                return ChatResponse(
                    type=ResponseType.ERROR,
                    content=f"Não consegui identificar corretamente o ator '{ator}' ou gênero '{genero}'.",
                    suggestions=["filmes de ação", "filmes do Tom Hanks"],
                )
            
            # Consulta Prolog com timeout
            try:
                actor_query = best_actor.upper()
                query_string = f"imdb_rules:recomendar_por_ator_e_genero('{actor_query}', '{best_genre}', TituloFilme)"
                results = await self._query_prolog(query_string)
            except PrologTimeoutError:
                return self._create_timeout_response()
            
            if not results:
                return ChatResponse(
                    type=ResponseType.TEXT,
                    content=f"Não encontrei filmes de {best_genre} com {best_actor}.",
                    suggestions=["filme aleatório", f"filmes de {best_genre}"],
                )
            
            filmes = [{"titulo": r["TituloFilme"]} for r in results]
            
            return ChatResponse(
                type=ResponseType.LIST,
                content=filmes,
                suggestions=[f"outros filmes de {best_actor}", f"outros filmes de {best_genre}"],
                metadata={"total": len(filmes), "filtros": ["ator", "genero"]}
            )
            
        # Caso não tenha filtros suficientes ou suportados
        return ChatResponse(
            type=ResponseType.TEXT,
            content="Por enquanto só suporto filtrar por **Ator + Gênero** (ex: 'filmes do Adam Sandler de comédia').",
            suggestions=["filmes com Adam Sandler e comédia", "filmes de ação"],
        )
