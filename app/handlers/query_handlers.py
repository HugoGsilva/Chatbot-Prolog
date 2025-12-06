"""
Query Handlers - Handlers para consultas sobre filmes

Handlers para consultar gênero, diretor e elenco de um filme específico.
"""

from typing import Dict

from .base_handler import BaseHandler
from ..schemas import ChatResponse, ResponseType
from ..nlu import find_best_film, find_best_film_with_suggestions
from ..prolog_service import PrologTimeoutError


class QueryHandlers(BaseHandler):
    """Handlers para consultas sobre filmes."""
    
    async def handle_genero_do_filme(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'genero_do_filme'."""
        filme = entities.get("filme", "")
        
        if not filme:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o nome do filme. Pode reformular?",
                suggestions=["gênero de Inception", "gênero do Matrix"],
            )
        
        # Tenta match com sugestões
        match_result = find_best_film_with_suggestions(filme)
        best_match = match_result["best_match"]
        
        # Se não encontrou match válido, mostra sugestões
        if not best_match:
            suggestions_text = ""
            if match_result["suggestions"]:
                top_3 = match_result["suggestions"][:3]
                suggestions_list = [f"- {s['title']} (similaridade: {s['score']}%)" for s in top_3]
                suggestions_text = "\n\nTalvez você quis dizer:\n" + "\n".join(suggestions_list)
            
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"❌ Não encontrei o filme **'{filme}'** na base Netflix.{suggestions_text}",
                suggestions=["filme aleatório", "filmes de ação"],
                metadata={"rejected_reason": match_result["rejected_reason"]}
            )
        
        # Consulta Prolog com timeout
        try:
            escaped_match = self._escape_prolog_string(best_match)
            query_string = (
                f"imdb_kb:netflix_title(ID, Titulo, _, _), upcase_atom(Titulo, Upper), "
                f"Upper = '{escaped_match}', imdb_kb:netflix_genre(ID, NomeGenero)"
            )
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei informações de gênero para o filme '{best_match}'.",
            )
        
        generos = [{"nome": r["NomeGenero"]} for r in results]
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=generos,
        )
    
    async def handle_diretor_do_filme(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'diretor_do_filme' - quem dirigiu X?"""
        filme = entities.get("filme", "")
        
        if not filme:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o nome do filme. Pode reformular?",
                suggestions=["quem dirigiu Matrix?", "diretor de Inception"],
            )
        
        # Tenta match com sugestões
        match_result = find_best_film_with_suggestions(filme)
        best_match = match_result["best_match"]
        
        # Se não encontrou match válido, mostra sugestões
        if not best_match:
            suggestions_text = ""
            if match_result["suggestions"]:
                top_3 = match_result["suggestions"][:3]
                suggestions_list = [f"- {s['title']} (similaridade: {s['score']}%)" for s in top_3]
                suggestions_text = "\n\nTalvez você quis dizer:\n" + "\n".join(suggestions_list)
            
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"❌ Não encontrei o filme **'{filme}'** na base Netflix.{suggestions_text}",
                suggestions=["filme aleatório", "filmes de ação"],
                metadata={"rejected_reason": match_result["rejected_reason"]}
            )
        
        # Consulta Prolog para buscar diretor
        try:
            escaped_match = self._escape_prolog_string(best_match)
            query_string = (
                f"imdb_kb:netflix_title(ID, Titulo, _, _), upcase_atom(Titulo, Upper), "
                f"Upper = '{escaped_match}', imdb_kb:netflix_director(ID, Diretor)"
            )
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei informações de diretor para o filme '{best_match}'.",
            )
        
        diretores = list(set([r["Diretor"] for r in results]))
        
        if len(diretores) == 1:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"🎬 O filme **{best_match}** foi dirigido por **{diretores[0]}**.",
                suggestions=[f"filmes do diretor {diretores[0]}", "filme aleatório"],
            )
        else:
            diretores_str = ", ".join(diretores)
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"🎬 O filme **{best_match}** foi dirigido por: **{diretores_str}**.",
                suggestions=["filme aleatório"],
            )
    
    async def handle_atores_do_filme(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'atores_do_filme' - quem atuou em X?"""
        filme = entities.get("filme", "")
        
        if not filme:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não consegui identificar o nome do filme. Pode reformular?",
                suggestions=["quem atuou em Matrix?", "elenco de Inception"],
            )
        
        # Tenta match com sugestões
        match_result = find_best_film_with_suggestions(filme)
        best_match = match_result["best_match"]
        
        # Se não encontrou match válido, mostra sugestões
        if not best_match:
            suggestions_text = ""
            if match_result["suggestions"]:
                top_3 = match_result["suggestions"][:3]
                suggestions_list = [f"- {s['title']} (similaridade: {s['score']}%)" for s in top_3]
                suggestions_text = "\n\nTalvez você quis dizer:\n" + "\n".join(suggestions_list)
            
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"❌ Não encontrei o filme **'{filme}'** na base Netflix.{suggestions_text}",
                suggestions=["filme aleatório", "filmes de ação"],
                metadata={"rejected_reason": match_result["rejected_reason"]}
            )
        
        # Consulta Prolog para buscar atores
        try:
            escaped_match = self._escape_prolog_string(best_match)
            query_string = (
                f"imdb_kb:netflix_title(ID, Titulo, _, _), upcase_atom(Titulo, Upper), "
                f"Upper = '{escaped_match}', imdb_kb:netflix_actor(ID, Ator)"
            )
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei informações de elenco para o filme '{best_match}'.",
            )
        
        atores = list(set([r["Ator"] for r in results]))
        
        # Limita a 10 atores para não sobrecarregar
        atores = atores[:10]
        
        if len(atores) == 1:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"🎬 **{best_match}** tem no elenco: **{atores[0]}**.",
                suggestions=[f"filmes do {atores[0]}", "filme aleatório"],
            )
        else:
            atores_list = [{"nome": ator} for ator in atores]
            return ChatResponse(
                type=ResponseType.LIST,
                content=atores_list,
                suggestions=["filme aleatório"],
                metadata={"filme": best_match, "total_atores": len(atores)}
            )
