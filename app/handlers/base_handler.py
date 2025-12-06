"""
Base Handler - Classe base para todos os handlers de intenção

Fornece métodos compartilhados para consultar Prolog, criar respostas,
e gerenciar erros.
"""

import logging
from typing import Dict, List

from ..schemas import ChatResponse, ResponseType, NLUResult
from ..prolog_service import prolog_service, PrologTimeoutError

logger = logging.getLogger(__name__)


class BaseHandler:
    """
    Classe base para handlers de intenção.
    
    Fornece métodos utilitários compartilhados por todos os handlers.
    """
    
    def _escape_prolog_string(self, text: str) -> str:
        """
        Escapa caracteres especiais para uso em strings Prolog.
        
        Args:
            text: Texto a ser escapado
            
        Returns:
            Texto com caracteres especiais escapados
        """
        # Escapa aspas simples dobrando-as
        return text.replace("'", "''")
    
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
    
    def _build_metadata(self, nlu_result: NLUResult) -> Dict:
        """
        Constrói metadados para a resposta.
        
        Args:
            nlu_result: Resultado do NLU
            
        Returns:
            Dict com metadados
        """
        return {
            "intent": nlu_result.intent,
            "confidence": nlu_result.confidence,
            "entities": nlu_result.entities,
            "corrected_text": nlu_result.corrected_text,
        }
    
    def _create_error_response(self, error_msg: str) -> ChatResponse:
        """
        Cria resposta de erro genérica.
        
        Args:
            error_msg: Mensagem de erro
            
        Returns:
            ChatResponse de erro
        """
        return ChatResponse(
            type=ResponseType.ERROR,
            content=f"Ocorreu um erro ao processar sua solicitação: {error_msg}",
            suggestions=["ajuda", "filme aleatório"],
            metadata={"error": error_msg}
        )
