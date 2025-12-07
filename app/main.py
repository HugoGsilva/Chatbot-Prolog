"""
Aplicação FastAPI que expõe endpoints para consultar o KB Prolog.
Carrega regras Prolog e caches NLU no arranque.
Frontend Angular servido separadamente via Nginx.
"""

import os
import json
import csv
import time
from contextlib import asynccontextmanager
from fastapi import FastAPI, HTTPException, Request
from fastapi.middleware.cors import CORSMiddleware

# Nossas importações de serviço
from .prolog_service import prolog_service
from .session_manager import session_service

# [FIX] Importa as caches (listas vazias) do nlu.py
# Agora 'main' popula as listas que 'nlu' detém.
from .nlu import ACTOR_CACHE, GENRE_CACHE, FILM_CACHE, DIRECTOR_CACHE

# --- Imports Thin Client (Fase 2) ---
from .schemas import ChatRequest, ChatResponse
from .spell_corrector import SpellCorrector
from .nlu_engine import NLUEngine
from .intent_router import IntentRouter
from .rate_limiter import RateLimiter, check_rate_limit, RateLimitExceeded

# --- Singletons Thin Client ---
spell_corrector: SpellCorrector = None
nlu_engine: NLUEngine = None
intent_router: IntentRouter = None
rate_limiter: RateLimiter = None

# --- Helper: fallback para carregar caches NLU do CSV ---
def load_nlu_caches_from_csv(csv_path: str) -> tuple[int, int, int, int]:
    """
    Fallback para carregar caches NLU a partir do CSV.
    Propósito: Popular ACTOR/GENRE/FILM/DIRECTOR caches caso Redis não contenha as caches.
    """
    actor_set: set[str] = set()
    genre_set: set[str] = set()
    film_set: set[str] = set()
    director_set: set[str] = set()

    try:
        with open(csv_path, "r", encoding="utf-8") as f:
            reader = csv.DictReader(f)
            for row in reader:
                title = (row.get("title") or "").strip()
                if title:
                    film_set.add(title.upper())

                cast = row.get("cast") or ""
                for name in [n.strip() for n in cast.split(",") if n.strip()]:
                    actor_set.add(name.upper())

                director = row.get("director") or ""
                for name in [n.strip() for n in director.split(",") if n.strip()]:
                    director_set.add(name.upper())

                listed = row.get("listed_in") or ""
                for g in [gr.strip().upper() for gr in listed.split(",") if gr.strip()]:
                    genre_set.add(g)

        ACTOR_CACHE.extend(sorted(actor_set))
        GENRE_CACHE.extend(sorted(genre_set))
        FILM_CACHE.extend(sorted(film_set))
        DIRECTOR_CACHE.extend(sorted(director_set))
        return len(ACTOR_CACHE), len(GENRE_CACHE), len(FILM_CACHE), len(DIRECTOR_CACHE)
    except Exception as e:
        print(f"[Startup] [ERRO] Falha ao ler CSV de caches NLU: {e}")
        return (0, 0, 0, 0)

# --- LÓGICA DE STARTUP (LIFESPAN V4.3 - LEVE) ---

@asynccontextmanager
async def lifespan(app: FastAPI):
    """
    Rotina de startup/shutdown da aplicação.
    Propósito: Inicializar motor Prolog, carregar caches NLU do Redis (ou fallback CSV)
    e verificar a ligação ao Redis para sessões.
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
        director_data = await session_service.client.get("nlu_directors_cache")

        if not actor_data or not genre_data or not film_data:
            print("[Startup] [ERRO CRÍTICO] Caches NLU não encontradas no Redis. Tentando fallback CSV...")
            csv_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "data_netflix", "netflix_titles.csv"))
            counts = load_nlu_caches_from_csv(csv_path)
            print(f"[Startup] Caches NLU carregadas do CSV: {counts[0]} Atores, {counts[1]} Géneros, {counts[2]} Filmes, {counts[3]} Diretores.")
        else:
            # Popula as listas globais importadas do nlu.py
            ACTOR_CACHE.extend(json.loads(actor_data))
            GENRE_CACHE.extend(json.loads(genre_data))
            FILM_CACHE.extend(json.loads(film_data))
            if director_data:
                DIRECTOR_CACHE.extend(json.loads(director_data))
            print(f"[Startup] Caches NLU carregadas do Redis: {len(ACTOR_CACHE)} Atores, {len(GENRE_CACHE)} Géneros, {len(FILM_CACHE)} Filmes, {len(DIRECTOR_CACHE)} Diretores.")

    except Exception as e:
        print(f"[Startup] [ERRO CRÍTICO] Falha ao carregar caches NLU do Redis: {e}")
        # --- Fallback CSV mesmo em erro de conexão ---
        csv_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "data_netflix", "netflix_titles.csv"))
        counts = load_nlu_caches_from_csv(csv_path)
        print(f"[Startup] [Fallback] Caches NLU carregadas do CSV: {counts[0]} Atores, {counts[1]} Géneros, {counts[2]} Filmes, {counts[3]} Diretores.")

    # 3. Testar Conexão Redis (do session_service)
    try:
        await session_service.test_connection()
        print("[Startup] Conexão com Redis (Sessão) verificada.")
    except Exception as e:
        print(f"[Startup] [AVISO] Redis indisponível, prosseguindo sem sessão: {e}")

    # 4. Inicializar Singletons Thin Client (Fase 2)
    global spell_corrector, nlu_engine, intent_router, rate_limiter
    
    print("[Startup] Inicializando componentes Thin Client...")
    
    # 4.1 SpellCorrector com vocabulário das caches
    spell_corrector = SpellCorrector(max_edit_distance=2)
    vocab_count = spell_corrector.load_vocabulary(
        actors=ACTOR_CACHE,
        genres=GENRE_CACHE,
        films=FILM_CACHE,
        directors=DIRECTOR_CACHE
    )
    print(f"[Startup] SpellCorrector inicializado com {vocab_count} termos no vocabulário.")
    
    # 4.2 NLUEngine com SpellCorrector integrado
    nlu_engine = NLUEngine(spell_corrector=spell_corrector)
    print(f"[Startup] NLUEngine inicializado (semantic={'ATIVADO' if nlu_engine.use_semantic else 'DESATIVADO'}).")
    
    # 4.2.1 Warm-up do semantic classifier se ativado
    if nlu_engine.use_semantic:
        print("[Startup] Aquecendo semantic classifier...")
        from .semantic_classifier import get_semantic_classifier
        classifier = get_semantic_classifier()
        # Faz uma classificação dummy para carregar modelo
        _ = classifier.classify("teste de inicialização", top_k=1)
        print("[Startup] ✅ Semantic classifier carregado e pronto.")
    
    # 4.3 IntentRouter
    intent_router = IntentRouter()
    print("[Startup] IntentRouter inicializado.")
    
    # 4.4 RateLimiter (20 req/min por IP, 10 req/min por sessão)
    rate_limiter = RateLimiter(session_service.client)
    print("[Startup] RateLimiter inicializado (20 req/min por IP, 10 req/min por sessão).")

    print("\n--- SERVIÇO 'app' PRONTO (Thin Client Habilitado) ---")
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

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# --- HEALTH CHECK ---

@app.get("/health")
async def health_check():
    return {"status": "Chatbot API (V2 Netflix) running"}

# --- ENDPOINT THIN CLIENT (Fase 2) ---

@app.post("/chat", response_model=ChatResponse)
async def chat_endpoint(chat_request: ChatRequest, request: Request):
    """
    Endpoint unificado para chat Thin Client.
    
    Processa mensagem do usuário através do pipeline:
    1. Rate limiting (20 req/min por IP, 10 req/min por sessão)
    2. Validação de sessão (recria se expirada)
    3. SpellCorrector -> Correção ortográfica
    4. NLUEngine -> Detecção de intenção e extração de entidades
    5. IntentRouter -> Roteamento para handler apropriado
    6. Persistência no histórico da sessão
    
    Args:
        chat_request: ChatRequest com message e session_id
        request: FastAPI Request para obter IP do cliente
        
    Returns:
        ChatResponse estruturada com type, content, suggestions, metadata
        
    Raises:
        HTTPException 429: Se rate limit excedido
    """
    global nlu_engine, intent_router, rate_limiter
    
    # Início do tracking de tempo para logging estruturado
    start_time = time.time()
    client_ip = request.client.host if request.client else "unknown"
    
    if not nlu_engine or not intent_router:
        print(f"[CHAT] ERROR | ip={client_ip} | session={chat_request.session_id} | error=startup_incomplete")
        return ChatResponse(
            type="error",
            content="Sistema não inicializado. Tente novamente em alguns segundos.",
            suggestions=None,
            metadata={"error": "startup_incomplete"}
        )
    
    try:
        # 0. Verificar rate limit (IP + sessão)
        if rate_limiter:
            try:
                await check_rate_limit(rate_limiter, client_ip, chat_request.session_id)
            except RateLimitExceeded as e:
                elapsed_ms = (time.time() - start_time) * 1000
                print(f"[CHAT] RATE_LIMITED | ip={client_ip} | session={chat_request.session_id} | time_ms={elapsed_ms:.2f}")
                raise HTTPException(
                    status_code=429,
                    detail=str(e),
                    headers={"Retry-After": "60"}
                )
        
        # 1. Validar/recriar sessão se necessário
        session_existed = await session_service.ensure_session_exists(chat_request.session_id)
        if not session_existed:
            print(f"[CHAT] SESSION_RECREATED | session={chat_request.session_id}")
        
        # 2. Processar NLU (já inclui correção ortográfica)
        nlu_start = time.time()
        nlu_result = nlu_engine.parse(chat_request.message)
        nlu_time_ms = (time.time() - nlu_start) * 1000
        
        # 3. Rotear para handler apropriado
        route_start = time.time()
        response = await intent_router.route(nlu_result, chat_request.session_id)
        route_time_ms = (time.time() - route_start) * 1000
        
        # 4. Salvar no histórico da sessão
        try:
            await session_service.add_to_history(
                chat_request.session_id, 
                f"User: {chat_request.message}"
            )
            await session_service.add_to_history(
                chat_request.session_id,
                f"Bot: {response.content}"
            )
        except Exception as e:
            print(f"[CHAT] HISTORY_ERROR | session={chat_request.session_id} | error={e}")
        
        # 5. Log estruturado de sucesso
        elapsed_ms = (time.time() - start_time) * 1000
        msg_preview = chat_request.message[:30] + "..." if len(chat_request.message) > 30 else chat_request.message
        print(f"[CHAT] OK | ip={client_ip} | session={chat_request.session_id[:8]}... | "
              f"intent={nlu_result.intent} | conf={nlu_result.confidence:.2f} | "
              f"type={response.type} | nlu_ms={nlu_time_ms:.1f} | route_ms={route_time_ms:.1f} | "
              f"total_ms={elapsed_ms:.1f} | msg=\"{msg_preview}\"")
        
        return response
        
    except HTTPException:
        # Re-raise HTTPExceptions (como rate limit 429)
        raise
    except Exception as e:
        elapsed_ms = (time.time() - start_time) * 1000
        print(f"[CHAT] ERROR | ip={client_ip} | session={chat_request.session_id} | "
              f"time_ms={elapsed_ms:.2f} | error={e}")
        return ChatResponse(
            type="error",
            content=f"Desculpe, ocorreu um erro ao processar sua mensagem: {str(e)}",
            suggestions=["Tente reformular sua pergunta", "Digite 'ajuda' para ver comandos disponíveis"],
            metadata={"error": str(e)}
        )


# --- ENDPOINTS DE SESSÃO (Fase 2 - Task 10) ---

@app.post("/session/create")
async def create_session():
    """
    Cria uma nova sessão e retorna o session_id.
    
    O Frontend deve chamar este endpoint para obter um session_id
    antes de iniciar uma conversa.
    
    Returns:
        Dict com session_id gerado
    """
    try:
        session_id = await session_service.create_session()
        return {
            "session_id": session_id,
            "ttl_seconds": 86400,  # 24 horas
            "message": "Sessão criada com sucesso"
        }
    except Exception as e:
        print(f"[ERROR] Erro ao criar sessão: {e}")
        raise HTTPException(status_code=500, detail="Erro ao criar sessão")


@app.delete("/session/{session_id}")
async def delete_session(session_id: str):
    """
    Remove uma sessão e seu histórico.
    
    Args:
        session_id: ID da sessão a remover
        
    Returns:
        Dict com status da operação
    """
    try:
        deleted = await session_service.delete_session(session_id)
        if deleted:
            return {"message": "Sessão removida com sucesso", "session_id": session_id}
        else:
            raise HTTPException(status_code=404, detail="Sessão não encontrada")
    except HTTPException:
        raise
    except Exception as e:
        print(f"[ERROR] Erro ao remover sessão: {e}")
        raise HTTPException(status_code=500, detail="Erro ao remover sessão")


@app.get("/session/{session_id}/history")
async def get_session_history(session_id: str, limit: int = 10):
    """
    Retorna o histórico de mensagens de uma sessão.
    
    Args:
        session_id: ID da sessão
        limit: Número máximo de mensagens a retornar (default: 10)
        
    Returns:
        Dict com histórico de mensagens
    """
    try:
        exists = await session_service.session_exists(session_id)
        if not exists:
            raise HTTPException(status_code=404, detail="Sessão não encontrada")
        
        history = await session_service.get_history(session_id, limit)
        ttl = await session_service.get_session_ttl(session_id)
        
        return {
            "session_id": session_id,
            "history": history,
            "count": len(history),
            "ttl_seconds": ttl
        }
    except HTTPException:
        raise
    except Exception as e:
        print(f"[ERROR] Erro ao buscar histórico: {e}")
        raise HTTPException(status_code=500, detail="Erro ao buscar histórico")


# =============================================================================
# NOTA: Endpoints legados foram removidos na Fase 5 (Thin Client Migration)
# 
# Endpoints removidos:
#   - GET /filmes-por-ator/{nome_ator}
#   - GET /filmes-por-genero/{genero}
#   - GET /filmes-por-diretor/{diretor}
#   - GET /genero-do-filme/{titulo_filme}
#   - GET /recomendar/ator-e-genero
#   - GET /recomendar/dois-generos
#   - GET /contar-filmes
#   - GET /recomendar/aleatorio
#
# Todas as funcionalidades estão agora disponíveis através do endpoint
# unificado POST /chat que processa linguagem natural.
# =============================================================================