import os
import json
from contextlib import asynccontextmanager
from fastapi import FastAPI, HTTPException
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse

# Nossas importações de serviço
from .prolog_service import prolog_service
from .session_manager import session_service

# [FIX] Importa as caches (listas vazias) do nlu.py
# Agora 'main' popula as listas que 'nlu' detém.
from .nlu import ACTOR_CACHE, GENRE_CACHE, FILM_CACHE

# Importa os Schemas Pydantic (necessários para os novos endpoints TDD)
from .schemas import Filme, Genero, ContagemGenero

# --- LÓGICA DE STARTUP (LIFESPAN V4.3 - LEVE) ---

@asynccontextmanager
async def lifespan(app: FastAPI):
    """
    Rotina de startup e shutdown "Leve".
    - NÃO executa o ETL.
    - Carrega as regras Prolog V2 (imdb_rules) do disco.
    - Carrega as caches NLU (V2) do REDIS.
    """
    print("[Startup] Iniciando API e carregando motor Prolog (V2)...")

    # 1. Carregar Regras Prolog (V2 - 'imdb_rules')
    # (O ficheiro imdb_kb.pl já foi criado pelo db-init)
    prolog_service.load_rules("prolog/rules/inferencia.pl")
    print("[Startup] Regras Prolog (imdb_rules) carregadas.")

    # 2. Carregar Caches NLU do Redis [Rec. 1]
    try:
        print("[Startup] A carregar Caches NLU do Redis...")

        # (Usamos o cliente async do session_service)
        actor_data = await session_service.client.get("nlu_actors_cache")
        genre_data = await session_service.client.get("nlu_genres_cache")
        film_data = await session_service.client.get("nlu_films_cache")

        if not actor_data or not genre_data or not film_data:
            print("[Startup] [ERRO CRÍTICO] Caches NLU não encontradas no Redis.")
            # (Numa app real, isto devia abortar o startup)
        else:
            # Popula as listas globais importadas do nlu.py
            ACTOR_CACHE.extend(json.loads(actor_data))
            GENRE_CACHE.extend(json.loads(genre_data))
            FILM_CACHE.extend(json.loads(film_data))
            print(f"[Startup] Caches NLU carregadas do Redis: {len(ACTOR_CACHE)} Atores, {len(GENRE_CACHE)} Géneros, {len(FILM_CACHE)} Filmes.")

    except Exception as e:
        print(f"[Startup] [ERRO CRÍTICO] Falha ao carregar caches NLU do Redis: {e}")

    # 3. Testar Conexão Redis (do session_service)
    await session_service.test_connection()
    print("[Startup] Conexão com Redis (Sessão) verificada.")

    print("\n--- SERVIÇO 'app' PRONTO ---")
    yield  # A API arranca aqui

    # --- LÓGICA DE SHUTDOWN ---
    print("[Shutdown] A fechar conexões...")
    await session_service.client.aclose()
    print("[Shutdown] Conexão Redis fechada.")

# --- INICIALIZAÇÃO DA APP ---

app = FastAPI(
    title="Chatbot Sakila-Prolog (Migração IMDB/Netflix)",
    description="API para consultar a base Netflix usando lógica Prolog.",
    version="2.0.0",
    lifespan=lifespan,  # (O novo startup leve)
)

# --- SERVIR O FRONTEND (Inalterado) ---

STATIC_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "frontend"))

@app.get("/", response_class=FileResponse)
async def read_index():
    return os.path.join(STATIC_DIR, "index.html")

@app.get("/health")
async def health_check():
    return {"status": "Chatbot API (V2 Netflix) running"}

app.mount("/static", StaticFiles(directory=STATIC_DIR), name="static")

# --- ENDPOINTS (V2) ---
# (Todos os endpoints antigos do Sakila foram removidos)
# (Vamos adicionar os novos endpoints V2 (TDD) aqui, na Fase 4)