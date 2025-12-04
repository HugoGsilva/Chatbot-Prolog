"""
Modelos Pydantic para respostas da API.
Propósito: Definir o contrato JSON usado pelos endpoints do FastAPI.

Inclui modelos para:
- Respostas de endpoints específicos (Filme, Genero, ContagemGenero)
- Arquitetura Thin Client (ChatRequest, ChatResponse, NLUResult, ResponseType)
"""

from enum import Enum
from typing import Dict, List, Optional, Union
from pydantic import BaseModel, Field, field_validator


# =============================================================================
# MODELOS EXISTENTES (Endpoints específicos)
# =============================================================================

class Filme(BaseModel):
    """Representa um filme com o campo 'titulo'."""
    titulo: str = Field(..., description="Título do filme")


class Genero(BaseModel):
    """Representa um género de filme com o campo 'nome'."""
    nome: str = Field(..., description="Nome do gênero")


class ContagemGenero(BaseModel):
    """Representa a contagem de filmes por género e ano."""
    genero: str = Field(..., description="Nome do gênero")
    ano: int = Field(..., description="Ano de lançamento")
    contagem: int = Field(..., description="Número de filmes no ano/gênero")


# =============================================================================
# MODELOS THIN CLIENT ARCHITECTURE
# =============================================================================

class ResponseType(str, Enum):
    """Tipos de resposta do endpoint /chat.
    
    Cada tipo indica ao Frontend como renderizar o conteúdo:
    - TEXT: Mensagem de texto simples
    - LIST: Lista de itens (filmes, gêneros, etc.)
    - ERROR: Mensagem de erro com sugestões opcionais
    - CLARIFICATION: Pedido de esclarecimento com opções
    - HELP: Ajuda com exemplos de queries
    """
    TEXT = "text"
    LIST = "list"
    ERROR = "error"
    CLARIFICATION = "clarification"
    HELP = "help"


class NLUResult(BaseModel):
    """Resultado do processamento NLU de uma mensagem.
    
    Contém a intenção detectada, entidades extraídas e metadados
    sobre a confiança e correção ortográfica aplicada.
    """
    intent: str = Field(..., description="Intenção detectada (ex: filmes_por_ator)")
    entities: Dict[str, str] = Field(
        default_factory=dict, 
        description="Entidades extraídas {tipo: valor}"
    )
    confidence: float = Field(
        ..., 
        ge=0.0, 
        le=1.0, 
        description="Score de confiança da detecção (0.0 a 1.0)"
    )
    original_text: str = Field(..., description="Texto original do usuário")
    corrected_text: Optional[str] = Field(
        None, 
        description="Texto após correção ortográfica (None se não houve correção)"
    )
    
    @field_validator('confidence')
    @classmethod
    def validate_confidence(cls, v: float) -> float:
        """Garante que confidence está entre 0 e 1."""
        return max(0.0, min(1.0, v))


class ChatRequest(BaseModel):
    """Requisição para o endpoint unificado /chat.
    
    O Frontend envia apenas o texto bruto da mensagem e o session_id.
    Todo o processamento NLU é feito no Backend.
    """
    message: str = Field(
        ..., 
        min_length=1, 
        max_length=1000, 
        description="Mensagem do usuário (texto bruto)"
    )
    session_id: str = Field(
        ..., 
        min_length=1,
        max_length=100,
        description="Identificador da sessão"
    )
    
    @field_validator('message')
    @classmethod
    def validate_message(cls, v: str) -> str:
        """Remove espaços extras e valida conteúdo."""
        v = v.strip()
        if not v:
            raise ValueError("Mensagem não pode ser vazia")
        return v
    
    @field_validator('session_id')
    @classmethod
    def validate_session_id(cls, v: str) -> str:
        """Valida formato do session_id."""
        v = v.strip()
        if not v:
            raise ValueError("session_id não pode ser vazio")
        # Aceita formatos: sess_xxx, uuid, ou alfanumérico
        import re
        if not re.match(r'^[a-zA-Z0-9_-]+$', v):
            raise ValueError("session_id contém caracteres inválidos")
        return v


class ChatResponse(BaseModel):
    """Resposta estruturada do endpoint /chat.
    
    O Frontend usa o campo 'type' para decidir como renderizar o conteúdo.
    Cada tipo tem uma estrutura de content específica.
    """
    type: ResponseType = Field(..., description="Tipo de resposta para renderização")
    content: Union[str, List[Dict], Dict] = Field(
        ..., 
        description="Conteúdo da resposta (formato depende do type)"
    )
    suggestions: Optional[List[str]] = Field(
        None, 
        description="Sugestões de queries para o usuário"
    )
    metadata: Optional[Dict] = Field(
        None, 
        description="Metadados adicionais (intent, confidence, etc.)"
    )
    
    class Config:
        """Configuração do modelo com exemplos para documentação."""
        json_schema_extra = {
            "examples": [
                {
                    "type": "list",
                    "content": [{"titulo": "Forrest Gump"}, {"titulo": "Cast Away"}],
                    "suggestions": ["filmes de drama", "filmes por Steven Spielberg"],
                    "metadata": {"intent": "filmes_por_ator", "confidence": 0.92}
                },
                {
                    "type": "error",
                    "content": "Não encontrei o ator 'Tom Hank'. Você quis dizer 'Tom Hanks'?",
                    "suggestions": ["filmes por Tom Hanks"],
                    "metadata": {"error_code": "ENTITY_NOT_FOUND"}
                },
                {
                    "type": "help",
                    "content": {
                        "message": "Não entendi sua pergunta. Aqui estão alguns exemplos:",
                        "examples": {
                            "Por ator": ["filmes por Tom Hanks", "filmes do Brad Pitt"],
                            "Por gênero": ["filmes de ação", "filmes de comédia"]
                        }
                    },
                    "suggestions": None,
                    "metadata": None
                }
            ]
        }


# =============================================================================
# EXPORTS
# =============================================================================

__all__ = [
    # Modelos existentes
    "Filme", 
    "Genero", 
    "ContagemGenero",
    # Novos modelos Thin Client
    "ResponseType",
    "NLUResult",
    "ChatRequest",
    "ChatResponse",
]