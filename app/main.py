"""
Instancia a aplicação FastAPI e expõe um endpoint raiz de verificação
para confirmar que o servidor está rodando.
"""

from fastapi import FastAPI, HTTPException
from contextlib import asynccontextmanager
from .schemas import Filme, ContagemGenero
from .nlu import normalize_actor_name, normalize_genre_name
from .session_manager import session_service
from .prolog_service import prolog_service


@asynccontextmanager
async def lifespan(app: FastAPI):
    # Startup
    print("Iniciando API e carregando motor Prolog...")
    prolog_service.load_rules("prolog/rules/inferencia.pl")
    # Testa conexão com Redis (SessionManager)
    try:
        await session_service.test_connection()
        print("Conexão com Redis verificada.")
    except Exception as e:
        print(f"[WARN] Falha ao conectar no Redis: {e}")
    print("Motor Prolog carregado.")

    yield

    # Shutdown
    try:
        await session_service.client.aclose()
        print("Conexão Redis fechada.")
    except Exception as e:
        print(f"[WARN] Falha ao fechar conexão Redis: {e}")
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
async def get_filmes_por_ator(nome_ator: str, session_id: str):
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
    # Grava histórico de conversa no Redis (session manager)
    try:
        await session_service.add_to_history(session_id, f"User: {nome_ator}")
        await session_service.add_to_history(session_id, f"Bot: {response_data}")
    except Exception as e:
        # Não falha o endpoint por erro de histórico; apenas loga
        print(f"[WARN] Falha ao gravar histórico na sessão '{session_id}': {e}")
    return response_data


@app.get("/recomendar-por-ator/{nome_ator}", response_model=list[Filme])
async def recomendar_filmes_por_ator(nome_ator: str, session_id: str):
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
    # Grava histórico de conversa no Redis (session manager), usando nome_ator original
    try:
        await session_service.add_to_history(session_id, f"User: {nome_ator}")
        await session_service.add_to_history(session_id, f"Bot: {response_data}")
    except Exception as e:
        print(f"[WARN] Falha ao gravar histórico na sessão '{session_id}': {e}")
    return response_data


@app.get("/contar-filmes", response_model=ContagemGenero)
async def contar_filmes_por_genero_e_ano(genero: str, ano: int):
    """Conta filmes pelo gênero e ano de lançamento usando regra Prolog agregadora."""
    normalized_genero = normalize_genre_name(genero)
    query_string = f"sakila_rules:contar_filmes_por_genero_e_ano('{normalized_genero}', {ano}, Contagem)"
    results = prolog_service.query(query_string)

    if not results or "Contagem" not in results[0]:
        raise HTTPException(
            status_code=404,
            detail="Não foi possível gerar a contagem para os parâmetros fornecidos",
        )

    contagem_result = results[0]["Contagem"]
    return {"genero": normalized_genero, "ano": ano, "contagem": contagem_result}


@app.get("/history/{session_id}")
async def get_history(session_id: str, limit: int = 5):
    """Retorna o histórico de mensagens de uma sessão (para validação do Redis)."""
    history = await session_service.get_history(session_id, limit=limit)
    return {"session_id": session_id, "history": history}