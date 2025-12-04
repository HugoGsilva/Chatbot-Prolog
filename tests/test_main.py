"""
Testes de integração para o endpoint POST /chat (Thin Client Architecture).

Substituição dos testes de endpoints legados.
Testa o fluxo completo: mensagem → NLU → Prolog → resposta estruturada.
"""

import json
import pytest
import httpx
from asgi_lifespan import LifespanManager
from unittest.mock import AsyncMock, MagicMock

from app.main import app


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def mock_session_service(monkeypatch):
    """Mock do session service para testes isolados."""
    mock_service = AsyncMock()
    mock_service.add_to_history = AsyncMock()
    mock_service.test_connection = AsyncMock()
    mock_service.session_exists = AsyncMock(return_value=True)
    mock_service.create_session = AsyncMock(return_value="test-session-id")
    
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()
    
    # Simula caches NLU no Redis
    actores_cache = ["TOM HANKS", "ACTOR A", "ACTOR B"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION"]
    filmes_cache = ["SAMPLE FILM", "CATCH ME IF YOU CAN"]
    diretores_cache = ["DIRECTOR A", "STEVEN SPIELBERG"]
    
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(actores_cache),
        json.dumps(generos_cache),
        json.dumps(filmes_cache),
        json.dumps(diretores_cache),
    ])
    
    mock_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_service)
    
    return mock_service


@pytest.fixture
def mock_rate_limiter(monkeypatch):
    """Mock do rate limiter para evitar bloqueios nos testes."""
    # Permite todas as requisições nos testes
    async def mock_check_rate_limit(request, session_id, limiter):
        pass  # Não faz nada, permite todas as requisições
    
    monkeypatch.setattr("app.main.check_rate_limit", mock_check_rate_limit)


# =============================================================================
# TESTES DO ENDPOINT /chat
# =============================================================================

@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_chat_endpoint_basic(anyio_backend, mock_session_service, mock_rate_limiter):
    """
    Testa que o endpoint /chat aceita mensagens e retorna resposta estruturada.
    """
    async with LifespanManager(app):
        async with httpx.AsyncClient(app=app, base_url="http://test") as client:
            response = await client.post("/chat", json={
                "message": "ajuda",
                "session_id": "test-session"
            })
            
            assert response.status_code == 200
            data = response.json()
            
            # Verifica estrutura da resposta
            assert "type" in data
            assert "content" in data
            assert data["type"] in ["text", "list", "error", "help", "clarification"]


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_chat_help_intent(anyio_backend, mock_session_service, mock_rate_limiter):
    """
    Testa que pedido de ajuda retorna resposta do tipo 'help'.
    """
    async with LifespanManager(app):
        async with httpx.AsyncClient(app=app, base_url="http://test") as client:
            response = await client.post("/chat", json={
                "message": "ajuda",
                "session_id": "test-session"
            })
            
            assert response.status_code == 200
            data = response.json()
            
            assert data["type"] == "help"
            assert "examples" in data["content"]


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_chat_missing_session_id(anyio_backend, mock_session_service, mock_rate_limiter):
    """
    Testa que mensagem sem session_id retorna erro 422.
    """
    async with LifespanManager(app):
        async with httpx.AsyncClient(app=app, base_url="http://test") as client:
            response = await client.post("/chat", json={
                "message": "olá"
            })
            
            assert response.status_code == 422  # Validation error


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_chat_empty_message(anyio_backend, mock_session_service, mock_rate_limiter):
    """
    Testa que mensagem vazia retorna erro apropriado.
    """
    async with LifespanManager(app):
        async with httpx.AsyncClient(app=app, base_url="http://test") as client:
            response = await client.post("/chat", json={
                "message": "",
                "session_id": "test-session"
            })
            
            assert response.status_code == 422  # Validation error


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_chat_response_has_metadata(anyio_backend, mock_session_service, mock_rate_limiter):
    """
    Testa que a resposta inclui metadata com informações de NLU.
    """
    async with LifespanManager(app):
        async with httpx.AsyncClient(app=app, base_url="http://test") as client:
            response = await client.post("/chat", json={
                "message": "ajuda",
                "session_id": "test-session"
            })
            
            assert response.status_code == 200
            data = response.json()
            
            assert "metadata" in data
            assert "intent" in data["metadata"]
            assert "confidence" in data["metadata"]


# =============================================================================
# TESTES DE SESSÃO
# =============================================================================

@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_session_create(anyio_backend, mock_session_service, mock_rate_limiter):
    """
    Testa criação de sessão via endpoint dedicado.
    """
    async with LifespanManager(app):
        async with httpx.AsyncClient(app=app, base_url="http://test") as client:
            response = await client.post("/session/create")
            
            assert response.status_code == 200
            data = response.json()
            
            assert "session_id" in data
            assert "ttl_seconds" in data


# =============================================================================
# TESTES DE HEALTH CHECK
# =============================================================================

@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_health_endpoint(anyio_backend, mock_session_service, mock_rate_limiter):
    """
    Testa que o endpoint /health retorna status OK.
    """
    async with LifespanManager(app):
        async with httpx.AsyncClient(app=app, base_url="http://test") as client:
            response = await client.get("/health")
            
            assert response.status_code == 200
            data = response.json()
            
            assert "status" in data


# =============================================================================
# NOTA: Testes de endpoints legados foram removidos
# 
# Os seguintes endpoints foram removidos na Fase 5 (Thin Client Migration):
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
#
# Ver arquivo test_main_legacy.py.bak para referência dos testes antigos.
# =============================================================================
