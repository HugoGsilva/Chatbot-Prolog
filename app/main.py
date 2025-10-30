"""
Instancia a aplicação FastAPI e expõe um endpoint raiz de verificação
para confirmar que o servidor está rodando.
"""

from fastapi import FastAPI, HTTPException
from contextlib import asynccontextmanager
from .schemas import Filme
from .nlu import normalize_actor_name
from .prolog_service import prolog_service


@asynccontextmanager
async def lifespan(app: FastAPI):
    # Startup
    print("Iniciando API e carregando motor Prolog...")
    prolog_service.load_rules("prolog/rules/inferencia.pl")
    print("Motor Prolog carregado.")

    yield

    # Shutdown
    print("API desligando...")


app = FastAPI(
    title="Sakila-Prolog Chatbot API",
    description="API para consultar a base Sakila usando lógica Prolog.",
    version="0.1.0",
    lifespan=lifespan,
)


@app.get("/")
async def health_check():
    return {"status": "Sakila-Prolog API running"}


# Removido: startup obsoleto via on_event, substituído por lifespan


@app.get("/filmes-por-ator/{nome_ator}", response_model=list[Filme])
async def get_filmes_por_ator(nome_ator: str):
    """Retorna filmes para um ator indicado, consultando o Prolog.

    Normaliza o nome para mitigar case-sensitivity e retorna uma lista
    no formato do schema `Filme`.
    """
    normalized_name = normalize_actor_name(nome_ator)
    # Prefixo do módulo para chamadas a regras exportadas
    query_string = f"sakila_rules:filmes_por_ator('{normalized_name}', TituloFilme)"
    results = prolog_service.query(query_string)

    if not results:
        raise HTTPException(status_code=404, detail="Ator não encontrado")

    response_data = [{"titulo": r["TituloFilme"]} for r in results]
    return response_data


@app.get("/recomendar-por-ator/{nome_ator}", response_model=list[Filme])
async def recomendar_filmes_por_ator(nome_ator: str):
    """Recomenda filmes com base nos gêneros associados aos filmes de um ator.

    Normaliza o nome do ator e consulta a regra Prolog
    `sakila_rules:recomendar_por_ator/2`, retornando uma lista no formato
    do schema `Filme`.
    """
    normalized_name = normalize_actor_name(nome_ator)
    query_string = f"sakila_rules:recomendar_por_ator('{normalized_name}', FilmeRecomendado)"
    results = prolog_service.query(query_string)

    if not results:
        raise HTTPException(
            status_code=404,
            detail="Não foi possível gerar recomendações (ator não encontrado ou sem recomendações disponíveis)",
        )

    response_data = [{"titulo": r["FilmeRecomendado"]} for r in results]
    return response_data