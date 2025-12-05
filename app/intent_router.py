"""
Intent Router - Roteador de Intenções para o Chatbot

Este módulo implementa o roteamento de intenções detectadas pelo NLUEngine
para os handlers apropriados, aplicando lógica de thresholds de confiança.

Responsabilidades:
- Rotear NLUResult para o handler correto baseado na intenção
- Aplicar lógica de confiança (alta, média, baixa)
- Retornar respostas apropriadas para cada caso
"""

import logging
from typing import Callable, Dict, List, Optional, Any

from .schemas import NLUResult, ChatResponse, ResponseType
from .nlu import find_best_actor, find_best_genre, find_best_film, find_best_director
from .prolog_service import prolog_service, PrologTimeoutError
from .session_manager import session_service

logger = logging.getLogger(__name__)


class IntentRouter:
    """
    Roteador de intenções que direciona NLUResults para handlers apropriados.
    
    Aplica lógica de confiança:
    - Alta confiança (≥ 0.7): Executa diretamente
    - Média confiança (0.4 - 0.7): Executa com sugestões
    - Baixa confiança (< 0.4): Retorna ajuda
    """
    
    # Thresholds de confiança
    HIGH_CONFIDENCE = 0.7
    MEDIUM_CONFIDENCE = 0.4
    
    def __init__(self):
        """Inicializa o router com mapeamento de handlers."""
        self._handlers: Dict[str, Callable] = {
            "ajuda": self.handle_ajuda,
            "saudacao": self.handle_saudacao,
            "filmes_por_ator": self.handle_filmes_por_ator,
            "filmes_por_genero": self.handle_filmes_por_genero,
            "filmes_por_diretor": self.handle_filmes_por_diretor,
            "genero_do_filme": self.handle_genero_do_filme,
            "filme_aleatorio": self.handle_filme_aleatorio,
            "recomendar_ator_e_genero": self.handle_recomendar_ator_e_genero,
            "recomendar_dois_generos": self.handle_recomendar_dois_generos,
            "contar_filmes": self.handle_contar_filmes,
            "recomendar_filme": self.handle_recomendar_filme,
            "unknown": self.handle_unknown,
        }
        
        logger.info(f"IntentRouter inicializado com {len(self._handlers)} handlers")
    
    async def route(self, nlu_result: NLUResult, session_id: str) -> ChatResponse:
        """
        Roteia o resultado NLU para o handler apropriado.
        
        Args:
            nlu_result: Resultado do processamento NLU
            session_id: ID da sessão do usuário
            
        Returns:
            ChatResponse estruturada
        """
        intent = nlu_result.intent
        confidence = nlu_result.confidence
        entities = nlu_result.entities
        
        logger.debug(f"Routing intent='{intent}' confidence={confidence:.2f}")
        
        # Verifica threshold de confiança
        if confidence < self.MEDIUM_CONFIDENCE:
            # Baixa confiança - retorna ajuda
            return self._create_help_response(nlu_result)
        
        # Obtém o handler para a intenção
        handler = self._handlers.get(intent, self.handle_unknown)
        
        try:
            # Executa o handler
            response = await handler(entities, session_id)
            
            # Se confiança média, adiciona sugestões
            if confidence < self.HIGH_CONFIDENCE:
                response = self._add_suggestions_to_response(response, nlu_result)
            
            # Adiciona metadados
            response.metadata = {
                "intent": intent,
                "confidence": confidence,
                "entities": entities,
                "corrected_text": nlu_result.corrected_text,
            }
            
            return response
            
        except Exception as e:
            logger.error(f"Erro no handler '{intent}': {e}")
            return self._create_error_response(str(e), nlu_result)
    
    def has_handler(self, intent: str) -> bool:
        """Verifica se existe um handler para a intenção."""
        return intent in self._handlers
    
    def get_supported_intents(self) -> List[str]:
        """Retorna lista de intenções suportadas."""
        return list(self._handlers.keys())
    
    async def _query_prolog(self, query_string: str, timeout: float = 2.0) -> List[Dict]:
        """
        Helper para executar query Prolog com timeout.
        
        Args:
            query_string: Query Prolog
            timeout: Timeout em segundos
            
        Returns:
            Lista de resultados
            
        Raises:
            PrologTimeoutError: Se exceder timeout
        """
        return await prolog_service.query_with_timeout(query_string, timeout)
    
    def _create_timeout_response(self) -> ChatResponse:
        """Cria resposta de erro para timeout do Prolog."""
        return ChatResponse(
            type=ResponseType.ERROR,
            content="A consulta está demorando muito. Tente uma busca mais específica.",
            suggestions=["filmes do Brad Pitt", "filmes de ação", "filme aleatório"],
            metadata={"error_code": "PROLOG_TIMEOUT"}
        )
    
    # =========================================================================
    # HANDLERS DE INTENÇÃO
    # =========================================================================
    
    # =========================================================================
    # HANDLERS DE AJUDA E SAUDAÇÃO
    # =========================================================================
    
    async def handle_ajuda(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """
        Handler para intenção 'ajuda'.
        
        Retorna informações sobre como usar o chatbot.
        """
        help_content = {
            "message": "👋 Olá! Sou o chatbot de filmes Netflix. Posso ajudar você a:",
            "examples": {
                "Buscar por ator": ["filmes do Tom Hanks", "filmes com Adam Sandler"],
                "Buscar por gênero": ["filmes de ação", "filmes de comédia"],
                "Buscar por diretor": ["filmes do Steven Spielberg", "filmes de Christopher Nolan"],
                "Descobrir gênero": ["gênero de Inception", "qual o tipo de Matrix"],
                "Recomendações": ["recomende um filme de terror", "sugira um drama"],
                "Filme aleatório": ["filme aleatório", "me surpreenda"]
            }
        }
        
        return ChatResponse(
            type=ResponseType.HELP,
            content=help_content,
            suggestions=["filmes de ação", "filmes do Tom Hanks", "filme aleatório"],
        )
    
    async def handle_saudacao(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """
        Handler para intenção 'saudacao'.
        
        Responde a saudações do usuário.
        """
        return ChatResponse(
            type=ResponseType.TEXT,
            content="Olá! 👋 Sou o chatbot de filmes Netflix. Como posso ajudar? Digite 'ajuda' para ver o que posso fazer.",
            suggestions=["ajuda", "filmes de ação", "filme aleatório"],
        )
    
    # =========================================================================
    # HANDLERS DE FILMES
    # =========================================================================
    
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
        
        # Normaliza o título do filme
        best_match = find_best_film(filme)
        
        if not best_match:
            return ChatResponse(
                type=ResponseType.ERROR,
                content=f"Não encontrei o filme '{filme}' na base de dados.",
            )
        
        # Consulta Prolog com timeout
        try:
            query_string = (
                f"imdb_kb:netflix_title(ID, Titulo, _), upcase_atom(Titulo, Upper), "
                f"Upper = '{best_match}', imdb_kb:netflix_genre(ID, NomeGenero)"
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
    
    async def handle_filme_aleatorio(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenção 'filme_aleatorio'."""
        try:
            query_string = "imdb_rules:random_movie(TituloFilme)"
            results = await self._query_prolog(query_string)
        except PrologTimeoutError:
            return self._create_timeout_response()
        
        if not results:
            return ChatResponse(
                type=ResponseType.ERROR,
                content="Não foi possível encontrar um filme aleatório.",
            )
        
        titulo = results[0]["TituloFilme"]
        
        return ChatResponse(
            type=ResponseType.TEXT,
            content=f"🎬 Que tal assistir: **{titulo}**?",
            suggestions=[f"gênero de {titulo}", "outro filme aleatório"],
        )
    
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
            return await self.handle_filmes_por_genero(entities, session_id)
        else:
            # Filme aleatório como fallback
            return await self.handle_filme_aleatorio(entities, session_id)
    
    async def handle_unknown(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """Handler para intenções não reconhecidas."""
        return self._create_help_response(
            NLUResult(
                intent="unknown",
                entities=entities,
                confidence=0.0,
                original_text=""
            )
        )
    
    # =========================================================================
    # MÉTODOS AUXILIARES
    # =========================================================================
    
    def _create_help_response(self, nlu_result: NLUResult) -> ChatResponse:
        """
        Cria resposta de ajuda para casos de baixa confiança.
        
        Retorna exemplos organizados por categoria.
        """
        help_content = {
            "message": "Não entendi sua pergunta. Aqui estão alguns exemplos do que posso fazer:",
            "examples": {
                "Por ator": ["filmes por Tom Hanks", "filmes do Brad Pitt"],
                "Por gênero": ["filmes de ação", "filmes de comédia"],
                "Por diretor": ["filmes do diretor Spielberg"],
                "Gênero de filme": ["gênero de Inception", "gênero do Matrix"],
                "Contagem": ["quantos filmes de ação", "contar filmes de drama em 2020"],
                "Aleatório": ["filme aleatório", "recomendar filme"],
            }
        }
        
        return ChatResponse(
            type=ResponseType.HELP,
            content=help_content,
            metadata={
                "intent": nlu_result.intent,
                "confidence": nlu_result.confidence,
                "original_text": nlu_result.original_text,
            }
        )
    
    def _create_error_response(
        self, 
        error_message: str, 
        nlu_result: NLUResult
    ) -> ChatResponse:
        """Cria resposta de erro."""
        return ChatResponse(
            type=ResponseType.ERROR,
            content=f"Ocorreu um erro: {error_message}",
            suggestions=["filme aleatório", "filmes de ação"],
            metadata={
                "intent": nlu_result.intent,
                "error": error_message,
            }
        )
    
    def _add_suggestions_to_response(
        self, 
        response: ChatResponse, 
        nlu_result: NLUResult
    ) -> ChatResponse:
        """
        Adiciona sugestões a uma resposta de média confiança.
        
        Para indicar ao usuário que a interpretação pode não ser a desejada.
        """
        if response.suggestions is None:
            response.suggestions = []
        
        # Adiciona uma mensagem de confirmação se ainda não tiver
        if response.type == ResponseType.LIST:
            # Adiciona "você quis dizer?" implicitamente
            pass
        
        return response


# =============================================================================
# SINGLETON
# =============================================================================

_router_instance: Optional[IntentRouter] = None


def get_intent_router() -> IntentRouter:
    """Retorna a instância singleton do IntentRouter."""
    global _router_instance
    if _router_instance is None:
        _router_instance = IntentRouter()
    return _router_instance
