"""
Response Formatter - Formatador de Respostas para o Chatbot

Este módulo implementa a formatação padronizada de respostas para o Frontend,
garantindo consistência na estrutura JSON retornada pelo endpoint /chat.

Responsabilidades:
- Formatar respostas de diferentes tipos (list, text, error, help)
- Gerar mensagens amigáveis para o usuário
- Criar sugestões contextuais
- Manter consistência no formato de resposta
"""

import logging
from typing import Dict, List, Optional, Any, Union

from .schemas import ChatResponse, ResponseType

logger = logging.getLogger(__name__)


class ResponseFormatter:
    """
    Formatador de respostas estruturadas para o Frontend.
    
    Garante que todas as respostas sigam o mesmo formato JSON,
    facilitando a renderização no Frontend.
    """
    
    @staticmethod
    def format_list(
        items: List[Dict], 
        message: Optional[str] = None,
        suggestions: Optional[List[str]] = None,
        metadata: Optional[Dict] = None
    ) -> ChatResponse:
        """
        Formata uma lista de itens (filmes, gêneros, etc.).
        
        Args:
            items: Lista de dicionários com os itens
            message: Mensagem opcional a incluir
            suggestions: Sugestões de queries relacionadas
            metadata: Metadados adicionais
            
        Returns:
            ChatResponse com type=LIST
            
        Examples:
            >>> formatter.format_list([{"titulo": "Inception"}, {"titulo": "Matrix"}])
            ChatResponse(type=LIST, content=[...])
        """
        content = items
        
        # Se tiver mensagem, inclui como parte do content
        if message:
            content = {
                "message": message,
                "items": items
            }
        
        return ChatResponse(
            type=ResponseType.LIST,
            content=content,
            suggestions=suggestions,
            metadata=metadata
        )
    
    @staticmethod
    def format_text(
        text: str,
        suggestions: Optional[List[str]] = None,
        metadata: Optional[Dict] = None
    ) -> ChatResponse:
        """
        Formata uma resposta de texto simples.
        
        Args:
            text: Texto da resposta
            suggestions: Sugestões de queries relacionadas
            metadata: Metadados adicionais
            
        Returns:
            ChatResponse com type=TEXT
        """
        return ChatResponse(
            type=ResponseType.TEXT,
            content=text,
            suggestions=suggestions,
            metadata=metadata
        )
    
    @staticmethod
    def format_error(
        error_message: str,
        suggestions: Optional[List[str]] = None,
        error_code: Optional[str] = None,
        metadata: Optional[Dict] = None
    ) -> ChatResponse:
        """
        Formata uma resposta de erro.
        
        Args:
            error_message: Mensagem de erro amigável
            suggestions: Sugestões para corrigir o erro
            error_code: Código do erro (para logging/debug)
            metadata: Metadados adicionais
            
        Returns:
            ChatResponse com type=ERROR
        """
        meta = metadata or {}
        if error_code:
            meta["error_code"] = error_code
        
        return ChatResponse(
            type=ResponseType.ERROR,
            content=error_message,
            suggestions=suggestions,
            metadata=meta if meta else None
        )
    
    @staticmethod
    def format_help(
        message: str,
        examples: Dict[str, List[str]],
        metadata: Optional[Dict] = None
    ) -> ChatResponse:
        """
        Formata uma resposta de ajuda com exemplos por categoria.
        
        Args:
            message: Mensagem principal de ajuda
            examples: Dicionário {categoria: [exemplos]}
            metadata: Metadados adicionais
            
        Returns:
            ChatResponse com type=HELP
        """
        content = {
            "message": message,
            "examples": examples
        }
        
        return ChatResponse(
            type=ResponseType.HELP,
            content=content,
            suggestions=None,
            metadata=metadata
        )
    
    @staticmethod
    def format_clarification(
        message: str,
        options: List[str],
        metadata: Optional[Dict] = None
    ) -> ChatResponse:
        """
        Formata uma resposta de esclarecimento (quando há ambiguidade).
        
        Args:
            message: Mensagem pedindo esclarecimento
            options: Opções para o usuário escolher
            metadata: Metadados adicionais
            
        Returns:
            ChatResponse com type=CLARIFICATION
        """
        content = {
            "message": message,
            "options": options
        }
        
        return ChatResponse(
            type=ResponseType.CLARIFICATION,
            content=content,
            suggestions=options,
            metadata=metadata
        )
    
    # =========================================================================
    # FORMATADORES ESPECÍFICOS DO DOMÍNIO
    # =========================================================================
    
    @staticmethod
    def format_filme_list(
        filmes: List[Dict],
        context: Optional[str] = None,
        suggestions: Optional[List[str]] = None
    ) -> ChatResponse:
        """
        Formata uma lista de filmes.
        
        Args:
            filmes: Lista de filmes com campo 'titulo'
            context: Contexto da busca (ex: "do ator Tom Hanks")
            suggestions: Sugestões relacionadas
            
        Returns:
            ChatResponse formatada para filmes
        """
        if not filmes:
            return ResponseFormatter.format_text(
                f"Não encontrei filmes{' ' + context if context else ''}.",
                suggestions=suggestions
            )
        
        count = len(filmes)
        message = f"Encontrei {count} filme{'s' if count > 1 else ''}"
        if context:
            message += f" {context}"
        message += ":"
        
        return ResponseFormatter.format_list(
            items=filmes,
            message=message,
            suggestions=suggestions
        )
    
    @staticmethod
    def format_genero_list(
        generos: List[Dict],
        filme_titulo: Optional[str] = None
    ) -> ChatResponse:
        """
        Formata uma lista de gêneros.
        
        Args:
            generos: Lista de gêneros com campo 'nome'
            filme_titulo: Título do filme (para contexto)
            
        Returns:
            ChatResponse formatada para gêneros
        """
        if not generos:
            return ResponseFormatter.format_text(
                f"Não encontrei gêneros{' para ' + filme_titulo if filme_titulo else ''}."
            )
        
        # Formata gêneros de forma amigável
        nomes = [g.get("nome", "").title() for g in generos]
        
        if len(nomes) == 1:
            texto_generos = nomes[0]
        elif len(nomes) == 2:
            texto_generos = f"{nomes[0]} e {nomes[1]}"
        else:
            texto_generos = f"{', '.join(nomes[:-1])} e {nomes[-1]}"
        
        if filme_titulo:
            message = f"O filme **{filme_titulo}** é de: {texto_generos}"
        else:
            message = f"Gêneros: {texto_generos}"
        
        return ResponseFormatter.format_text(message)
    
    @staticmethod
    def format_contagem(
        contagem: int,
        genero: str,
        ano: Optional[int] = None
    ) -> ChatResponse:
        """
        Formata resultado de contagem de filmes.
        
        Args:
            contagem: Número de filmes
            genero: Nome do gênero
            ano: Ano (opcional)
            
        Returns:
            ChatResponse formatada
        """
        if ano:
            message = f"Encontrei **{contagem}** filme{'s' if contagem != 1 else ''} de {genero} em {ano}."
        else:
            message = f"Encontrei **{contagem}** filme{'s' if contagem != 1 else ''} de {genero}."
        
        return ResponseFormatter.format_text(
            message,
            suggestions=[f"filmes de {genero}", f"filmes de drama"]
        )
    
    @staticmethod
    def format_filme_aleatorio(
        titulo: str,
        suggestions: Optional[List[str]] = None
    ) -> ChatResponse:
        """
        Formata sugestão de filme aleatório.
        
        Args:
            titulo: Título do filme sugerido
            suggestions: Sugestões adicionais
            
        Returns:
            ChatResponse formatada
        """
        message = f"🎬 Que tal assistir: **{titulo}**?"
        
        default_suggestions = [
            f"gênero de {titulo}",
            "outro filme aleatório",
            "filmes de ação"
        ]
        
        return ResponseFormatter.format_text(
            message,
            suggestions=suggestions or default_suggestions
        )
    
    # =========================================================================
    # FORMATADORES DE ERRO ESPECÍFICOS
    # =========================================================================
    
    @staticmethod
    def format_entity_not_found(
        entity_type: str,
        entity_value: str,
        suggestions: Optional[List[str]] = None
    ) -> ChatResponse:
        """
        Formata erro de entidade não encontrada.
        
        Args:
            entity_type: Tipo da entidade (ator, gênero, etc.)
            entity_value: Valor buscado
            suggestions: Sugestões de correção
            
        Returns:
            ChatResponse de erro
        """
        message = f"Não encontrei {entity_type} '{entity_value}' na base de dados."
        
        return ResponseFormatter.format_error(
            error_message=message,
            suggestions=suggestions,
            error_code="ENTITY_NOT_FOUND",
            metadata={"entity_type": entity_type, "query": entity_value}
        )
    
    @staticmethod
    def format_missing_entity(
        entity_type: str,
        suggestions: Optional[List[str]] = None
    ) -> ChatResponse:
        """
        Formata erro de entidade faltante.
        
        Args:
            entity_type: Tipo da entidade esperada
            suggestions: Exemplos de como formular a query
            
        Returns:
            ChatResponse de erro
        """
        message = f"Não consegui identificar {entity_type}. Pode reformular?"
        
        return ResponseFormatter.format_error(
            error_message=message,
            suggestions=suggestions,
            error_code="MISSING_ENTITY",
            metadata={"entity_type": entity_type}
        )
    
    @staticmethod
    def format_no_results(
        context: str,
        suggestions: Optional[List[str]] = None
    ) -> ChatResponse:
        """
        Formata mensagem de nenhum resultado encontrado.
        
        Args:
            context: Contexto da busca
            suggestions: Sugestões alternativas
            
        Returns:
            ChatResponse de texto
        """
        message = f"Não encontrei resultados {context}."
        
        return ResponseFormatter.format_text(
            message,
            suggestions=suggestions
        )
    
    # =========================================================================
    # HELP FORMATTERS
    # =========================================================================
    
    @staticmethod
    def format_default_help() -> ChatResponse:
        """
        Formata resposta de ajuda padrão.
        
        Returns:
            ChatResponse com exemplos por categoria
        """
        return ResponseFormatter.format_help(
            message="Não entendi sua pergunta. Aqui estão alguns exemplos do que posso fazer:",
            examples={
                "Por ator": ["filmes por Tom Hanks", "filmes do Brad Pitt"],
                "Por gênero": ["filmes de ação", "filmes de comédia"],
                "Por diretor": ["filmes do diretor Spielberg", "filmes do diretor Nolan"],
                "Gênero de filme": ["gênero de Inception", "gênero do Matrix"],
                "Contagem": ["quantos filmes de ação", "contar filmes de drama em 2020"],
                "Recomendação": ["filmes de ação com Tom Hanks", "filmes de drama e romance"],
                "Aleatório": ["filme aleatório", "recomendar filme"],
            }
        )
    
    @staticmethod
    def format_low_confidence_help(
        original_text: str,
        suggestions: List[str]
    ) -> ChatResponse:
        """
        Formata ajuda para casos de baixa confiança.
        
        Args:
            original_text: Texto original do usuário
            suggestions: Sugestões similares
            
        Returns:
            ChatResponse de ajuda
        """
        content = {
            "message": f"Não entendi '{original_text}'. Você quis dizer:",
            "suggestions": suggestions
        }
        
        return ChatResponse(
            type=ResponseType.HELP,
            content=content,
            suggestions=suggestions
        )


# =============================================================================
# SINGLETON
# =============================================================================

_formatter_instance: Optional[ResponseFormatter] = None


def get_response_formatter() -> ResponseFormatter:
    """Retorna a instância singleton do ResponseFormatter."""
    global _formatter_instance
    if _formatter_instance is None:
        _formatter_instance = ResponseFormatter()
    return _formatter_instance
