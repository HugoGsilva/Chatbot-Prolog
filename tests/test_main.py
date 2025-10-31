import pytest
import httpx
from urllib.parse import quote
from asgi_lifespan import LifespanManager
from unittest.mock import AsyncMock
from app.main import app

@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_get_filmes_por_ator_endpoint(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """Verifica contrato esperado do endpoint de filmes por ator.

    Espera status 200 e uma lista de objetos com campo `titulo`.
    Deverá falhar (404) até o endpoint ser implementado.
    """

    # Mock do SessionManager usado dentro de app.main
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    # cliente com aclose para o shutdown do lifespan
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()
    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    async with LifespanManager(app):
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            ator = "PENELOPE GUINESS"
            ator_encoded = quote(ator)
            resp = await client.get(f"/filmes-por-ator/{ator_encoded}?session_id=sessao_de_teste")

        # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
        assert resp.status_code == 200

    # Verifica que o histórico foi salvo no Redis via serviço de sessão
    mock_session_service.add_to_history.assert_any_call("sessao_de_teste", "User: PENELOPE GUINESS")
    mock_session_service.add_to_history.assert_any_call("sessao_de_teste", f"Bot: {resp.json()}")

    data = resp.json()
    assert isinstance(data, list)
    assert len(data) > 0
    assert data[0]["titulo"] is not None


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_recomendar_filmes_endpoint(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """Verifica contrato esperado do endpoint de recomendação por ator.

    Espera status 200 e uma lista de objetos com campo `titulo`.
    Deverá falhar (404) até o endpoint ser implementado.
    """

    # Mock do SessionManager usado dentro de app.main
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()
    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    async with LifespanManager(app):
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            ator = "PENELOPE GUINESS"
            ator_encoded = quote(ator)
            resp = await client.get(f"/recomendar-por-ator/{ator_encoded}?session_id=sessao_de_teste_rec")

    # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
    assert resp.status_code == 200

    # Verifica que o histórico foi salvo no Redis via serviço de sessão
    mock_session_service.add_to_history.assert_any_call("sessao_de_teste_rec", f"User: {ator}")
    mock_session_service.add_to_history.assert_any_call("sessao_de_teste_rec", f"Bot: {resp.json()}")

    data = resp.json()
    assert isinstance(data, list)
    assert len(data) > 0
    assert data[0]["titulo"] is not None


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_contar_filmes_endpoint(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """Contrato do endpoint de contagem de filmes por gênero e ano.

    Espera status 200 e um objeto no formato do schema ContagemGenero.
    Deverá falhar (404) até o endpoint ser implementado.
    """
    # Mock do SessionManager usado dentro de app.main
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    # cliente com aclose para o shutdown do lifespan
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()
    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    async with LifespanManager(app):
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            params = {"genero": "Action", "ano": 2006}
            params["session_id"] = "sessao_de_teste_contagem"
            resp = await client.get("/contar-filmes", params=params)

    # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
    assert resp.status_code == 200

    # Verifica que o histórico foi salvo no Redis via serviço de sessão
    user_query = f"genero={params['genero']}, ano={params['ano']}"
    mock_session_service.add_to_history.assert_any_call("sessao_de_teste_contagem", f"User: {user_query}")
    mock_session_service.add_to_history.assert_any_call("sessao_de_teste_contagem", f"Bot: {resp.json()}")

    data = resp.json()
    assert isinstance(data, dict)
    assert data["genero"] == "Action"
    assert data["ano"] == 2006
    assert data["contagem"] >= 0


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_get_genero_do_filme_endpoint(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """Verifica contrato esperado do endpoint de gênero do filme.

    Espera status 200 e um objeto com campo `nome`.
    Deverá falhar (404) até o endpoint ser implementado.
    """

    # Mock do SessionManager usado dentro de app.main
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    # cliente com aclose para o shutdown do lifespan
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()
    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    async with LifespanManager(app):
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            resp = await client.get("/genero-do-filme/ACADEMY%20DINOSAUR?session_id=sessao_teste_genero")

    # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
    assert resp.status_code == 200

    # Verifica que o histórico foi salvo no Redis via serviço de sessão
    mock_session_service.add_to_history.assert_any_call("sessao_teste_genero", "User: ACADEMY DINOSAUR")
    mock_session_service.add_to_history.assert_any_call("sessao_teste_genero", f"Bot: {resp.json()}")

    data = resp.json()
    assert isinstance(data, dict)
    assert data["nome"] == "Documentary"


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_get_filmes_por_genero_endpoint(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """Verifica contrato esperado do endpoint de filmes por gênero.

    Espera status 200 e uma lista de objetos com campo `titulo`.
    Deverá falhar (404) até o endpoint ser implementado.
    """

    # Mock do SessionManager usado dentro de app.main
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    # cliente com aclose para o shutdown do lifespan
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()
    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    async with LifespanManager(app):
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            resp = await client.get("/filmes-por-genero/Action?session_id=sessao_teste_filmes_genero")

    # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
    assert resp.status_code == 200

    # Verifica que o histórico foi salvo no Redis via serviço de sessão
    mock_session_service.add_to_history.assert_any_call("sessao_teste_filmes_genero", "User: Action")
    mock_session_service.add_to_history.assert_any_call("sessao_teste_filmes_genero", f"Bot: {resp.json()}")

    data = resp.json()
    assert isinstance(data, list)
    assert len(data) > 0
    assert data[0]["titulo"] is not None