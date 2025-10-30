"""
Schemas Pydantic para o contrato de resposta da API FastAPI
(Fase 3.1 — Middleware FastAPI).

Estes modelos representam as estruturas JSON retornadas pelos endpoints,
servindo como ponte para as regras do módulo Prolog `sakila_rules`.
"""

from pydantic import BaseModel, Field


class Filme(BaseModel):
    """Schema para representar um único filme."""

    titulo: str = Field(..., description="Título do filme")


class Genero(BaseModel):
    """Schema para representar um único gênero de filme."""

    nome: str = Field(..., description="Nome do gênero")


class ContagemGenero(BaseModel):
    """Schema para representar a contagem de filmes por gênero e ano."""

    genero: str = Field(..., description="Nome do gênero")
    ano: int = Field(..., description="Ano de lançamento")
    contagem: int = Field(..., description="Número de filmes no ano/gênero")


__all__ = ["Filme", "Genero", "ContagemGenero"]