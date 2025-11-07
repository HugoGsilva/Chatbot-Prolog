"""
Modelos Pydantic para respostas da API.
Propósito: Definir o contrato JSON usado pelos endpoints do FastAPI.
"""

from pydantic import BaseModel, Field


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


__all__ = ["Filme", "Genero", "ContagemGenero"]