"""
Instancia a aplicação FastAPI e expõe um endpoint raiz de verificação
para confirmar que o servidor está rodando.
"""

from fastapi import FastAPI, HTTPException
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse
from contextlib import asynccontextmanager
import json
import os
from .schemas import Filme, ContagemGenero, Genero
from .nlu import find_best_actor, find_best_genre, find_best_film
from .session_manager import session_service
from .prolog_service import prolog_service

# Cache global de nomes de atores para NLU (carregada no startup)
ACTOR_CACHE: list[str] = []
GENRE_CACHE: list[str] = []
FILM_CACHE: list[str] = []


@asynccontextmanager
async def lifespan(app: FastAPI):
    # Startup
    print("Iniciando API e carregando motor Prolog...")
    prolog_service.load_rules("prolog/rules/inferencia.pl")
    # --- INÍCIO DA NOVA LÓGICA ---
    print("[Cache] A carregar cache de nomes de atores...")
    global ACTOR_CACHE
    try:
        query_string = "sakila_rules:get_all_actors(ListaNomes)"
        results = prolog_service.query(query_string)
        if results and "ListaNomes" in results[0]:
            ACTOR_CACHE = results[0]["ListaNomes"]
            print(f"[Cache] Cache de Atores carregada com {len(ACTOR_CACHE)} nomes.")
        else:
            print("[Cache] [ERRO] Não foi possível carregar a cache de atores do Prolog.")
    except Exception as e:
        print(f"[Cache] [ERRO] Exceção ao carregar cache de atores: {e}")

    # --- Carregar Cache de Géneros ---
    print("[Cache] A carregar cache de nomes de géneros...")
    global GENRE_CACHE
    try:
        results_genre = prolog_service.query("sakila_rules:get_all_genres(ListaGeneros)")
        if results_genre and "ListaGeneros" in results_genre[0]:
            GENRE_CACHE = results_genre[0]["ListaGeneros"]
            print(f"[Cache] Cache de Géneros carregada com {len(GENRE_CACHE)} nomes.")
        else:
            print("[Cache] [ERRO] Não foi possível carregar a cache de géneros.")
    except Exception as e:
        print(f"[Cache] [ERRO] Exceção ao carregar cache de géneros: {e}")

    # --- Carregar Cache de Títulos de Filmes ---
    print("[Cache] A carregar cache de títulos de filmes...")
    global FILM_CACHE
    try:
        results_film = prolog_service.query("sakila_rules:get_all_films(ListaTitulos)")
        if results_film and "ListaTitulos" in results_film[0]:
            FILM_CACHE = results_film[0]["ListaTitulos"]
            print(f"[Cache] Cache de Filmes carregada com {len(FILM_CACHE)} nomes.")
        else:
            print("[Cache] [ERRO] Não foi possível carregar a cache de filmes.")
    except Exception as e:
        print(f"[Cache] [ERRO] Exceção ao carregar cache de filmes: {e}")
    # --- FIM DA NOVA LÓGICA ---
    # Testa conexão com Redis (SessionManager)
    try:
        await session_service.test_connection()
        print("Conexão com Redis verificada.")
    except Exception as e:
        print(f"[WARN] Falha ao conectar no Redis: {e}")
    print("Motor Prolog carregado.")

    # Tentar carregar caches NLU a partir do Redis (geradas pelo db-init)
    try:
        actors_json = await session_service.client.get("nlu_actors_cache")
        genres_json = await session_service.client.get("nlu_genres_cache")
        films_json = await session_service.client.get("nlu_films_cache")

        # Se existirem, substituem as caches carregadas do Prolog
        if actors_json:
            ACTOR_CACHE = json.loads(actors_json)
        if genres_json:
            GENRE_CACHE = json.loads(genres_json)
        if films_json:
            FILM_CACHE = json.loads(films_json)
        if actors_json or genres_json or films_json:
            print("[Cache] Caches NLU carregadas do Redis.")
    except Exception as e:
        print(f"[WARN] Falha ao carregar caches NLU do Redis: {e}")

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

STATIC_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "frontend"))
app.mount("/static", StaticFiles(directory=STATIC_DIR), name="static")

@app.get("/", response_class=FileResponse)
async def read_index():
    return os.path.join(STATIC_DIR, "index.html")

@app.get("/health")
async def health_check():
    return {"status": "Sakila-Prolog API running"}


# Removido: startup obsoleto via on_event, substituído por lifespan


@app.get("/filmes-por-ator/{nome_ator}", response_model=list[Filme])
async def get_filmes_por_ator(nome_ator: str, session_id: str):
    """Retorna filmes para um ator indicado, consultando o Prolog.

    Normaliza o nome para mitigar case-sensitivity e retorna uma lista
    no formato do schema `Filme`.
    """
    # Resolve melhor correspondência do ator usando fuzzy matching (thefuzz)
    best_match_name = find_best_actor(nome_ator)
    if not best_match_name:
        raise HTTPException(status_code=404, detail=f"Ator '{nome_ator}' não encontrado.")
    # Prefixo do módulo para chamadas a regras exportadas
    query_string = f"sakila_rules:filmes_por_ator('{best_match_name}', TituloFilme)"
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
    # Usa fuzzy matching para resolver o melhor nome do ator
    best_match_name = find_best_actor(nome_ator)
    if not best_match_name:
        raise HTTPException(
            status_code=404,
            detail="Não foi possível gerar recomendações (ator não encontrado ou sem recomendações disponíveis)",
        )

    query_string = f"sakila_rules:recomendar_por_ator('{best_match_name}', FilmeRecomendado)"
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


@app.get("/genero-do-filme/{titulo_filme}", response_model=Genero)
async def get_genero_do_filme(titulo_filme: str, session_id: str):
    """Retorna o gênero de um filme específico, consultando o Prolog.

    Normaliza o título para o formato esperado e retorna um objeto
    no formato do schema `Genero`.
    """
    best_match_film = find_best_film(titulo_filme)
    if not best_match_film:
        raise HTTPException(status_code=404, detail=f"Filme '{titulo_filme}' não encontrado.")
    query_string = f"sakila_rules:genero_do_filme('{best_match_film}', NomeGenero)"
    results = prolog_service.query(query_string)

    if not results or "NomeGenero" not in results[0]:
        raise HTTPException(status_code=404, detail="Filme ou gênero não encontrado")

    genero_result = results[0]["NomeGenero"]
    response_data = {"nome": genero_result}
    # Grava histórico de conversa no Redis (session manager)
    try:
        await session_service.add_to_history(session_id, f"User: {titulo_filme}")
        await session_service.add_to_history(session_id, f"Bot: {response_data}")
    except Exception as e:
        print(f"[WARN] Falha ao gravar histórico na sessão '{session_id}': {e}")
    return response_data


@app.get("/contar-filmes", response_model=ContagemGenero)
async def contar_filmes_por_genero_e_ano(genero: str, ano: int, session_id: str):
    """Conta filmes pelo gênero e ano de lançamento usando regra Prolog agregadora."""
    best_match_genre = find_best_genre(genero)
    if not best_match_genre:
        raise HTTPException(status_code=404, detail=f"Gênero '{genero}' não encontrado.")
    query_string = f"sakila_rules:contar_filmes_por_genero_e_ano('{best_match_genre}', {ano}, Contagem)"
    results = prolog_service.query(query_string)

    if not results or "Contagem" not in results[0]:
        raise HTTPException(
            status_code=404,
            detail="Não foi possível gerar a contagem para os parâmetros fornecidos",
        )

    contagem_result = results[0]["Contagem"]
    response_data = {"genero": best_match_genre, "ano": ano, "contagem": contagem_result}
    # Grava histórico de conversa no Redis (session manager)
    try:
        user_query = f"genero={genero}, ano={ano}"
        await session_service.add_to_history(session_id, f"User: {user_query}")
        await session_service.add_to_history(session_id, f"Bot: {response_data}")
    except Exception as e:
        print(f"[WARN] Falha ao gravar histórico na sessão '{session_id}': {e}")
    return response_data


@app.get("/filmes-por-genero/{genero}", response_model=list[Filme])
async def get_filmes_por_genero(genero: str, session_id: str):
    """Retorna filmes para um gênero específico, consultando o Prolog.

    Normaliza o gênero e retorna uma lista no formato do schema `Filme`.
    """
    best_match_genre = find_best_genre(genero)
    if not best_match_genre:
        raise HTTPException(status_code=404, detail=f"Gênero '{genero}' não encontrado.")
    query_string = f"sakila_rules:filmes_por_genero('{best_match_genre}', TituloFilme)"
    results = prolog_service.query(query_string)

    if not results:
        raise HTTPException(status_code=404, detail="Gênero não encontrado ou sem filmes associados")

    response_data = [{"titulo": r["TituloFilme"]} for r in results]
    # Grava histórico de conversa no Redis (session manager)
    try:
        await session_service.add_to_history(session_id, f"User: {genero}")
        await session_service.add_to_history(session_id, f"Bot: {response_data}")
    except Exception as e:
        print(f"[WARN] Falha ao gravar histórico na sessão '{session_id}': {e}")
    return response_data


@app.get("/history/{session_id}")
async def get_history(session_id: str, limit: int = 5):
    """Retorna o histórico de mensagens de uma sessão (para validação do Redis)."""
    history = await session_service.get_history(session_id, limit=limit)
    return {"session_id": session_id, "history": history}