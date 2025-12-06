"""
Recommendation Handlers - Handlers para recomendações de filmes

Handlers para recomendações aleatórias e com filtros.
"""

from typing import Dict

from .base_handler import BaseHandler
from ..schemas import ChatResponse, ResponseType
from ..nlu import find_best_actor, find_best_genre
from ..prolog_service import PrologTimeoutError


class RecommendationHandlers(BaseHandler):
    """Handlers para recomendações de filmes."""
    
    async def handle_filme_aleatorio(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler enriquecido para intenção 'filme_aleatorio' com metadados completos."""
        try:
            # Usa regra enriquecida que retorna título, ano, gêneros e diretor
            query_string = "imdb_rules:random_movie_enriched(Titulo, Ano, Generos, Diretor)"
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não foi possível encontrar um filme aleatório.",
            )
        
        filme = results[0]
        titulo = filme["Titulo"]
        ano = filme["Ano"]
        generos = filme["Generos"]
        diretor = filme["Diretor"]
        
        # Formata gêneros (pega top 2 para não poluir)
        generos_texto = ", ".join(generos[:2]) if generos else "N/A"
        
        # Formata diretor (capitaliza para melhor apresentação)
        diretor_texto = diretor.title() if diretor and diretor != "Unknown" else "Diretor não disponível"
        
        # Monta resposta enriquecida
        content = (
            f"🎬 **{titulo}** ({ano})\n\n"
            f"📁 **Gênero:** {generos_texto}\n"
            f"🎭 **Diretor:** {diretor_texto}"
        )
        
        # Sugestões contextuais
        suggestions = ["outro filme aleatório"]
        if generos:
            suggestions.insert(0, f"filmes de {generos[0]}")
        if diretor and diretor != "Unknown":
            suggestions.append(f"filmes do diretor {diretor.split()[0]}")
        
        return ChatResponse(
            type=ResponseType.TEXT,
            content=content,
            suggestions=suggestions,
            metadata={"titulo": titulo, "ano": ano, "generos": generos, "diretor": diretor}
        )
    
    async def handle_recomendar_filme(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção genérica de recomendação."""
        # Tenta identificar o tipo de recomendação baseado nas entidades
        if "ator" in entities and "genero" in entities:
            return await self.handle_recomendar_ator_e_genero(entities, session_id)
        elif "genero1" in entities and "genero2" in entities:
            return await self.handle_recomendar_dois_generos(entities, session_id)
        elif "genero" in entities:
            # Recomendação por gênero - usa filme aleatório do gênero
            genero = entities.get("genero", "")
            best_genre = find_best_genre(genero)
            
            if best_genre:
                try:
                    genre_query = best_genre.upper()
                    # Usa versão enriquecida para gênero específico
                    query_string = f"imdb_rules:random_movie_by_genre_enriched('{genre_query}', Titulo, Ano, Generos, Diretor)"
                    results = await self._query_prolog(query_string)
                    
                    if results:
                        filme = results[0]
                        titulo = filme["Titulo"]
                        ano = filme["Ano"]
                        generos = filme["Generos"]
                        diretor = filme["Diretor"]
                        
                        generos_texto = ", ".join(generos[:2]) if generos else best_genre
                        diretor_texto = diretor.title() if diretor and diretor != "Unknown" else "Diretor não disponível"
                        
                        content = (
                            f"🎬 **{titulo}** ({ano})\n\n"
                            f"📁 **Gênero:** {generos_texto}\n"
                            f"🎭 **Diretor:** {diretor_texto}"
                        )
                        
                        suggestions = [f"outro filme de {best_genre}", "filme aleatório"]
                        if diretor and diretor != "Unknown":
                            suggestions.insert(1, f"filmes do diretor {diretor.split()[0]}")
                        
                        return ChatResponse(
                            type=ResponseType.TEXT,
                            content=content,
                            suggestions=suggestions,
                        )
                except PrologTimeoutError:
                    return self._create_timeout_response()
            
            # Fallback: filme aleatório geral
            return await self.handle_filme_aleatorio(entities, session_id)
        else:
            # Filme aleatório como fallback
            return await self.handle_filme_aleatorio(entities, session_id)
    
    async def handle_recomendar_ator_e_genero(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'recomendar_ator_e_genero'."""
        ator = entities.get("ator", "")
        genero = entities.get("genero", "")
        
        if not ator or not genero:
            missing = []
            if not ator:
                missing.append("ator")
            if not genero:
                missing.append("gênero")
            
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"Faltam informações: {', '.join(missing)}. Pode reformular?",
                suggestions=["filmes de ação com Tom Hanks", "filmes de drama do Brad Pitt"],
            )
        
        # Normaliza entidades
        best_actor = find_best_actor(ator)
        best_genre = find_best_genre(genero)
        
        if not best_actor or not best_genre:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não encontrei o ator ou gênero especificado.",
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
            )
        
        filmes = [{"titulo": r["TituloFilme"]} for r in results]
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=filmes,
        )
    
    async def handle_recomendar_dois_generos(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'recomendar_dois_generos'."""
        genero1 = entities.get("genero1", "")
        genero2 = entities.get("genero2", "")
        
        if not genero1 or not genero2:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Preciso de dois gêneros para fazer a recomendação.",
                suggestions=["filmes de ação e comédia", "filmes de drama e romance"],
            )
        
        # Normaliza gêneros
        best_genre1 = find_best_genre(genero1)
        best_genre2 = find_best_genre(genero2)
        
        if not best_genre1 or not best_genre2:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não encontrei um ou ambos os gêneros especificados.",
            )
        
        # Consulta Prolog com timeout
        try:
            query_string = f"imdb_rules:recomendar_por_dois_generos('{best_genre1}', '{best_genre2}', TituloFilme)"
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.TEXT,
                content=f"Não encontrei filmes que combinam {best_genre1} e {best_genre2}.",
            )
        
        filmes = [{"titulo": r["TituloFilme"]} for r in results]
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=filmes,
        )
