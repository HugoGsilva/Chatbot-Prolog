"""
Intent Router - Roteador de Intenções para o Chatbot

Este módulo implementa o roteamento de intenções detectadas pelo NLUEngine
para os handlers apropriados, aplicando lógica de thresholds de confiança.

Responsabilidades:
- Rotear NLUResult para o handler correto baseado na intenção
- Aplicar lógica de confiança (alta, média, baixa)
- Retornar respostas apropriadas para cada caso

REFATORADO: Handlers agora estão em módulos separados em app/handlers/
"""

import logging
from typing import Callable, Dict, List, Optional

from .schemas import NLUResult, ChatResponse, ResponseType
from .handlers import (
    InfoHandlers,
    SearchHandlers,
    QueryHandlers,
    RecommendationHandlers,
    FilterHandlers,
)

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
        """Inicializa o router com handlers modulares."""
        # Instancia handlers modulares
        self.info_handlers = InfoHandlers()
        self.search_handlers = SearchHandlers()
        self.query_handlers = QueryHandlers()
        self.recommendation_handlers = RecommendationHandlers()
        self.filter_handlers = FilterHandlers()
        
        # Mapeamento de intenções para handlers
        self._handlers: Dict[str, Callable] = {
            # Info handlers
            "ajuda": self.info_handlers.handle_ajuda,
            "saudacao": self.info_handlers.handle_saudacao,
            "identidade": self.info_handlers.handle_identidade,
            "despedida": self.info_handlers.handle_despedida,
            "small_talk": self.info_handlers.handle_small_talk,
            
            # Search handlers
            "filmes_por_ator": self.search_handlers.handle_filmes_por_ator,
            "filmes_por_genero": self.search_handlers.handle_filmes_por_genero,
            "filmes_por_diretor": self.search_handlers.handle_filmes_por_diretor,
            
            # Query handlers
            "genero_do_filme": self.query_handlers.handle_genero_do_filme,
            "diretor_do_filme": self.query_handlers.handle_diretor_do_filme,
            
            # Recommendation handlers
            "filme_aleatorio": self.recommendation_handlers.handle_filme_aleatorio,
            "recomendar_filme": self.recommendation_handlers.handle_recomendar_filme,
            "recomendar_ator_e_genero": self.recommendation_handlers.handle_recomendar_ator_e_genero,
            "recomendar_dois_generos": self.recommendation_handlers.handle_recomendar_dois_generos,
            
            # Filter handlers
            "filmes_por_ano": self.filter_handlers.handle_filmes_por_ano,
            "contar_filmes": self.filter_handlers.handle_contar_filmes,
            "filmes_com_filtros": self.filter_handlers.handle_filmes_com_filtros,
            
            # Unknown
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
    
    # =========================================================================
    # HANDLER PARA INTENÇÕES DESCONHECIDAS
    # =========================================================================
    
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
