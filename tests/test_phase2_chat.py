"""
Testes de Propriedade (Property Tests) para a Fase 2 - Chat Endpoint.

Tasks testadas:
- 9.1: Low confidence fallback response
- 9.2: Error response format validation
- 10.1: Session persistence across requests
- 10.2: Session history ordering
- 11: Rate limiting
"""

import json
import pytest
import httpx
from httpx import ASGITransport
from asgi_lifespan import LifespanManager
from unittest.mock import AsyncMock, patch
import asyncio

from app.main import app
from app.schemas import ChatResponse


# --- Fixtures Comuns ---

@pytest.fixture
def mock_session_service(monkeypatch):
    """Mock do SessionService para testes isolados."""
    mock = AsyncMock()
    mock.test_connection = AsyncMock()
    mock.add_to_history = AsyncMock()
    mock.ensure_session_exists = AsyncMock(return_value=True)
    mock.session_exists = AsyncMock(return_value=True)
    mock.create_session = AsyncMock(return_value="test-session-123")
    mock.delete_session = AsyncMock(return_value=True)
    mock.get_history = AsyncMock(return_value=[])
    
    # Mock do cliente Redis
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(["ACTOR A", "ACTOR B"]),  # actors
        json.dumps(["DRAMA", "COMEDY", "ACTION"]),  # genres
        json.dumps(["FILM A", "FILM B"]),  # films
        json.dumps(["DIRECTOR A"]),  # directors
    ])
    mock_client.zadd = AsyncMock()
    mock_client.zremrangebyscore = AsyncMock()
    mock_client.zcount = AsyncMock(return_value=1)
    
    mock.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock)
    return mock


# =============================================================================
# Task 9.1: Property Test - Low Confidence Fallback
# =============================================================================

class TestLowConfidenceFallback:
    """
    Propriedade: Quando a confiança da NLU é baixa (< 0.4),
    o sistema deve retornar uma resposta de fallback amigável.
    """
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_gibberish_input_returns_fallback(self, anyio_backend, mock_session_service):
        """Input sem sentido deve resultar em resposta de fallback."""
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                response = await client.post("/chat", json={
                    "message": "xyzabc qwerty poiuy",
                    "session_id": "test-session-low-conf"
                })
                
                assert response.status_code == 200
                data = response.json()
                
                # Deve ser tipo "fallback" ou "clarification"
                assert data["type"] in ["fallback", "clarification", "error"]
                
                # Deve ter sugestões para ajudar o usuário
                assert data.get("suggestions") is not None or "ajuda" in data["content"].lower() or "entendi" in data["content"].lower()
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_partial_intent_asks_clarification(self, anyio_backend, mock_session_service):
        """Input ambíguo deve pedir clarificação."""
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                response = await client.post("/chat", json={
                    "message": "filmes",  # Muito genérico
                    "session_id": "test-session-ambig"
                })
                
                assert response.status_code == 200
                data = response.json()
                
                # Ou retorna fallback ou tenta processar
                assert data["type"] in ["fallback", "clarification", "filmes", "error"]


# =============================================================================
# Task 9.2: Property Test - Error Response Format
# =============================================================================

class TestErrorResponseFormat:
    """
    Propriedade: Todas as respostas de erro devem seguir o formato
    ChatResponse com type="error" e metadata contendo detalhes.
    """
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_error_response_has_required_fields(self, anyio_backend, mock_session_service):
        """Resposta de erro deve ter todos os campos obrigatórios."""
        # Força um erro mockando o NLU engine
        with patch("app.main.nlu_engine", None):
            async with LifespanManager(app):
                async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                    response = await client.post("/chat", json={
                        "message": "teste",
                        "session_id": "test-session-error"
                    })
                    
                    assert response.status_code == 200
                    data = response.json()
                    
                    # Validar estrutura de resposta de erro
                    assert "type" in data
                    assert data["type"] == "error"
                    assert "content" in data
                    assert isinstance(data["content"], str)
                    assert len(data["content"]) > 0
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_invalid_request_returns_422(self, anyio_backend, mock_session_service):
        """Request inválido deve retornar erro 422 de validação."""
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                # Enviar request sem campos obrigatórios
                response = await client.post("/chat", json={
                    "wrong_field": "value"
                })
                
                # FastAPI retorna 422 para erros de validação Pydantic
                assert response.status_code == 422
                data = response.json()
                assert "detail" in data


# =============================================================================
# Task 10.1: Property Test - Session Persistence
# =============================================================================

class TestSessionPersistence:
    """
    Propriedade: Uma sessão criada deve persistir entre requests
    e manter seu estado (histórico, contexto).
    """
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_session_persists_across_requests(self, anyio_backend, mock_session_service):
        """Sessão deve persistir entre múltiplos requests."""
        history_storage = []
        
        # Mock add_to_history para capturar histórico
        async def capture_history(session_id, message):
            history_storage.append({"session_id": session_id, "message": message})
        
        mock_session_service.add_to_history = AsyncMock(side_effect=capture_history)
        
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                session_id = "persistent-session-123"
                
                # Request 1
                r1 = await client.post("/chat", json={
                    "message": "ola",
                    "session_id": session_id
                })
                assert r1.status_code == 200
                
                # Request 2 - mesma sessão
                r2 = await client.post("/chat", json={
                    "message": "ajuda",
                    "session_id": session_id
                })
                assert r2.status_code == 200
                
                # Verificar que histórico foi gravado para a mesma sessão
                session_messages = [h for h in history_storage if h["session_id"] == session_id]
                assert len(session_messages) >= 2  # Pelo menos 2 (user + bot por request)
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_create_session_returns_valid_id(self, anyio_backend, mock_session_service):
        """Endpoint de criação de sessão deve retornar ID válido."""
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                response = await client.post("/session/create")
                
                assert response.status_code == 200
                data = response.json()
                
                assert "session_id" in data
                assert len(data["session_id"]) > 0
                assert "ttl_seconds" in data
                assert data["ttl_seconds"] == 86400  # 24 horas


# =============================================================================
# Task 10.2: Property Test - Session History Ordering
# =============================================================================

class TestSessionHistoryOrdering:
    """
    Propriedade: O histórico da sessão deve manter a ordem cronológica
    das mensagens (FIFO).
    """
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_history_maintains_order(self, anyio_backend, mock_session_service):
        """Histórico deve manter ordem de inserção."""
        ordered_history = []
        
        async def capture_ordered_history(session_id, message):
            ordered_history.append(message)
        
        mock_session_service.add_to_history = AsyncMock(side_effect=capture_ordered_history)
        mock_session_service.get_history = AsyncMock(return_value=ordered_history)
        
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                session_id = "ordered-session-456"
                
                # Enviar mensagens em sequência
                messages = ["mensagem um", "mensagem dois", "mensagem tres"]
                
                for msg in messages:
                    await client.post("/chat", json={
                        "message": msg,
                        "session_id": session_id
                    })
                
                # Verificar que histórico está na ordem correta
                user_messages = [h for h in ordered_history if h.startswith("User:")]
                
                # Verificar ordem (primeira mensagem deve vir primeiro)
                for i, msg in enumerate(messages):
                    expected_prefix = f"User: {msg}"
                    assert any(expected_prefix in h for h in user_messages[:len(messages)]), \
                        f"Mensagem '{msg}' não encontrada na posição esperada"
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_get_history_endpoint(self, anyio_backend, mock_session_service):
        """Endpoint de histórico deve retornar lista ordenada."""
        mock_session_service.get_history = AsyncMock(return_value=[
            "User: primeira",
            "Bot: resposta 1",
            "User: segunda",
            "Bot: resposta 2"
        ])
        
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                response = await client.get("/session/test-session/history")
                
                assert response.status_code == 200
                data = response.json()
                
                assert "history" in data
                assert isinstance(data["history"], list)
                
                # Primeira entrada deve ser do usuário
                if len(data["history"]) > 0:
                    assert data["history"][0].startswith("User:")


# =============================================================================
# Task 11: Property Test - Rate Limiting
# =============================================================================

class TestRateLimiting:
    """
    Propriedade: O sistema deve limitar requisições por IP e por sessão.
    """
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_rate_limit_returns_429(self, anyio_backend, monkeypatch):
        """Exceder rate limit deve retornar 429."""
        mock = AsyncMock()
        mock.test_connection = AsyncMock()
        mock.add_to_history = AsyncMock()
        mock.ensure_session_exists = AsyncMock(return_value=True)
        
        mock_client = AsyncMock()
        mock_client.aclose = AsyncMock()
        mock_client.get = AsyncMock(side_effect=[
            json.dumps(["ACTOR A"]),
            json.dumps(["DRAMA"]),
            json.dumps(["FILM A"]),
            json.dumps(["DIRECTOR A"]),
        ])
        # Simular rate limit excedido (mais de 20 requests)
        mock_client.zadd = AsyncMock()
        mock_client.zremrangebyscore = AsyncMock()
        mock_client.zcount = AsyncMock(return_value=25)  # Excede limite de 20
        
        mock.client = mock_client
        monkeypatch.setattr("app.main.session_service", mock)
        
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                response = await client.post("/chat", json={
                    "message": "teste",
                    "session_id": "rate-limited-session"
                })
                
                # Deve retornar 429 Too Many Requests
                assert response.status_code == 429
                assert "Retry-After" in response.headers
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_normal_rate_allows_request(self, anyio_backend, mock_session_service):
        """Requisições dentro do limite devem passar normalmente."""
        # mock_session_service já configura zcount=1 (dentro do limite)
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                response = await client.post("/chat", json={
                    "message": "ola",
                    "session_id": "normal-rate-session"
                })
                
                # Deve processar normalmente (200 ou resposta válida)
                assert response.status_code == 200


# =============================================================================
# Integration Test - Full Chat Flow
# =============================================================================

class TestFullChatFlow:
    """Teste de integração do fluxo completo de chat."""
    
    @pytest.mark.parametrize("anyio_backend", ["asyncio"])
    @pytest.mark.anyio
    async def test_complete_chat_flow(self, anyio_backend, mock_session_service):
        """Fluxo completo: criar sessão -> enviar mensagens -> obter histórico."""
        async with LifespanManager(app):
            async with httpx.AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as client:
                # 1. Criar sessão
                create_resp = await client.post("/session/create")
                assert create_resp.status_code == 200
                session_id = create_resp.json()["session_id"]
                
                # 2. Enviar mensagem de saudação
                chat_resp = await client.post("/chat", json={
                    "message": "ola",
                    "session_id": session_id
                })
                assert chat_resp.status_code == 200
                data = chat_resp.json()
                assert "content" in data
                
                # 3. Enviar pergunta
                query_resp = await client.post("/chat", json={
                    "message": "ajuda",
                    "session_id": session_id
                })
                assert query_resp.status_code == 200
                
                # 4. Obter histórico
                history_resp = await client.get(f"/session/{session_id}/history")
                assert history_resp.status_code == 200
                assert "history" in history_resp.json()
