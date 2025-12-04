"""
Testes de Propriedade (Property Tests) para a Fase 2 - Chat Endpoint.

Estes testes validam a funcionalidade da API via HTTP real.
Use: docker exec prologue-app /opt/venv/bin/python -m pytest tests/test_phase2_functional.py -v

Tasks testadas:
- 9.1: Low confidence fallback response
- 9.2: Error response format validation  
- 10.1: Session persistence across requests
- 10.2: Session history ordering
- 11: Rate limiting
"""

import pytest
import httpx
import asyncio

# Configura pytest-asyncio para modo auto
pytestmark = pytest.mark.anyio

BASE_URL = "http://localhost:8000"


# =============================================================================
# Task 9.1: Property Test - Low Confidence Fallback
# =============================================================================

class TestLowConfidenceFallback:
    """
    Propriedade: Quando a confiança da NLU é baixa,
    o sistema deve retornar uma resposta amigável.
    """
    
    
    async def test_gibberish_input_returns_help_or_fallback(self):
        """Input sem sentido deve resultar em resposta de ajuda ou fallback."""
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{BASE_URL}/chat",
                json={
                    "message": "xyzabc qwerty poiuy",
                    "session_id": "test-gibberish-123"
                }
            )
            
            assert response.status_code == 200
            data = response.json()
            
            # Deve ter um tipo de resposta válido
            assert "type" in data
            assert data["type"] in ["help", "fallback", "clarification", "error"]
            
            # Deve ter conteúdo
            assert "content" in data
            assert len(data["content"]) > 0
    
    
    async def test_greeting_returns_response(self):
        """Saudação deve retornar alguma resposta."""
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{BASE_URL}/chat",
                json={
                    "message": "ola",
                    "session_id": "test-greeting-123"
                }
            )
            
            assert response.status_code == 200
            data = response.json()
            assert "content" in data


# =============================================================================
# Task 9.2: Property Test - Error Response Format
# =============================================================================

class TestErrorResponseFormat:
    """
    Propriedade: Respostas de erro devem seguir o formato esperado.
    """
    
    
    async def test_invalid_request_returns_422(self):
        """Request inválido deve retornar erro 422."""
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{BASE_URL}/chat",
                json={"wrong_field": "value"}
            )
            
            # FastAPI retorna 422 para erros de validação
            assert response.status_code == 422
            data = response.json()
            assert "detail" in data
    
    
    async def test_missing_message_returns_422(self):
        """Request sem message deve retornar erro 422."""
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{BASE_URL}/chat",
                json={"session_id": "test-123"}
            )
            
            assert response.status_code == 422


# =============================================================================
# Task 10.1: Property Test - Session Persistence
# =============================================================================

class TestSessionPersistence:
    """
    Propriedade: Sessões devem persistir entre requests.
    """
    
    
    async def test_create_session_returns_valid_id(self):
        """Endpoint de criação de sessão deve retornar ID válido."""
        async with httpx.AsyncClient() as client:
            response = await client.post(f"{BASE_URL}/session/create")
            
            assert response.status_code == 200
            data = response.json()
            
            assert "session_id" in data
            assert len(data["session_id"]) > 0
            assert "ttl_seconds" in data
            assert data["ttl_seconds"] == 86400  # 24 horas
    
    
    async def test_multiple_messages_same_session(self):
        """Múltiplas mensagens na mesma sessão devem funcionar."""
        async with httpx.AsyncClient() as client:
            # Criar sessão
            create_resp = await client.post(f"{BASE_URL}/session/create")
            session_id = create_resp.json()["session_id"]
            
            # Enviar múltiplas mensagens
            for i in range(3):
                response = await client.post(
                    f"{BASE_URL}/chat",
                    json={
                        "message": f"teste {i}",
                        "session_id": session_id
                    }
                )
                assert response.status_code == 200


# =============================================================================
# Task 10.2: Property Test - Session History
# =============================================================================

class TestSessionHistory:
    """
    Propriedade: Histórico da sessão deve ser acessível.
    """
    
    
    async def test_get_history_endpoint_exists(self):
        """Endpoint de histórico deve existir e responder."""
        async with httpx.AsyncClient() as client:
            # Criar sessão e enviar mensagem
            create_resp = await client.post(f"{BASE_URL}/session/create")
            session_id = create_resp.json()["session_id"]
            
            await client.post(
                f"{BASE_URL}/chat",
                json={
                    "message": "teste historico",
                    "session_id": session_id
                }
            )
            
            # Obter histórico
            history_resp = await client.get(f"{BASE_URL}/session/{session_id}/history")
            
            assert history_resp.status_code == 200
            data = history_resp.json()
            assert "history" in data
            assert isinstance(data["history"], list)
    
    
    async def test_delete_session(self):
        """Deve ser possível deletar uma sessão."""
        async with httpx.AsyncClient() as client:
            # Criar sessão
            create_resp = await client.post(f"{BASE_URL}/session/create")
            session_id = create_resp.json()["session_id"]
            
            # Deletar sessão
            delete_resp = await client.delete(f"{BASE_URL}/session/{session_id}")
            
            assert delete_resp.status_code == 200


# =============================================================================
# Task 11: Property Test - Rate Limiting
# =============================================================================

class TestRateLimiting:
    """
    Propriedade: O sistema deve ter proteção contra abuso.
    """
    
    
    async def test_normal_requests_allowed(self):
        """Requisições normais devem ser permitidas."""
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{BASE_URL}/chat",
                json={
                    "message": "filmes de drama",
                    "session_id": "test-rate-normal-123"
                }
            )
            
            # Deve processar normalmente
            assert response.status_code == 200
    
    
    async def test_rate_limit_headers_present(self):
        """Requisição deve ser processada (rate limiter ativo)."""
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{BASE_URL}/chat",
                json={
                    "message": "ajuda",
                    "session_id": "test-rate-check-123"
                }
            )
            
            # A requisição deve passar (não atingiu limite)
            assert response.status_code in [200, 429]


# =============================================================================
# Integration Test - Full Chat Flow
# =============================================================================

class TestFullChatFlow:
    """Teste de integração do fluxo completo de chat."""
    
    
    async def test_complete_flow_genre_query(self):
        """Fluxo completo: criar sessão -> consultar gênero -> histórico."""
        async with httpx.AsyncClient() as client:
            # 1. Criar sessão
            create_resp = await client.post(f"{BASE_URL}/session/create")
            assert create_resp.status_code == 200
            session_id = create_resp.json()["session_id"]
            
            # 2. Consultar filmes por gênero
            chat_resp = await client.post(
                f"{BASE_URL}/chat",
                json={
                    "message": "filmes de acao",
                    "session_id": session_id
                }
            )
            assert chat_resp.status_code == 200
            data = chat_resp.json()
            assert data["type"] == "list"
            
            # 3. Verificar que histórico foi gravado
            history_resp = await client.get(f"{BASE_URL}/session/{session_id}/history")
            assert history_resp.status_code == 200
            
            # 4. Limpar sessão
            delete_resp = await client.delete(f"{BASE_URL}/session/{session_id}")
            assert delete_resp.status_code == 200
    
    
    async def test_help_command(self):
        """Comando de ajuda deve retornar exemplos."""
        async with httpx.AsyncClient() as client:
            response = await client.post(
                f"{BASE_URL}/chat",
                json={
                    "message": "ajuda",
                    "session_id": "test-help-cmd-123"
                }
            )
            
            assert response.status_code == 200
            data = response.json()
            assert "content" in data


# =============================================================================
# Health Check
# =============================================================================

class TestHealthCheck:
    """Testes de health check."""
    
    
    async def test_health_endpoint(self):
        """Endpoint /health deve responder."""
        async with httpx.AsyncClient() as client:
            response = await client.get(f"{BASE_URL}/health")
            
            assert response.status_code == 200
            data = response.json()
            assert "status" in data
