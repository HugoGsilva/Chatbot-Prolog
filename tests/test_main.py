import pytest
import httpx
from urllib.parse import quote
from asgi_lifespan import LifespanManager
from app.main import app

@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_get_filmes_por_ator_endpoint(anyio_backend):
    """Verifica contrato esperado do endpoint de filmes por ator.

    Espera status 200 e uma lista de objetos com campo `titulo`.
    Deverá falhar (404) até o endpoint ser implementado.
    """

    async with LifespanManager(app):
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            ator = "PENELOPE GUINESS"
            ator_encoded = quote(ator)
            resp = await client.get(f"/filmes-por-ator/{ator_encoded}")

        # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
        assert resp.status_code == 200

    data = resp.json()
    assert isinstance(data, list)
    assert len(data) > 0
    assert data[0]["titulo"] is not None


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_recomendar_filmes_endpoint(anyio_backend):
    """Verifica contrato esperado do endpoint de recomendação por ator.

    Espera status 200 e uma lista de objetos com campo `titulo`.
    Deverá falhar (404) até o endpoint ser implementado.
    """

    async with LifespanManager(app):
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            ator = "PENELOPE GUINESS"
            ator_encoded = quote(ator)
            resp = await client.get(f"/recomendar-por-ator/{ator_encoded}")

    # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
    assert resp.status_code == 200

    data = resp.json()
    assert isinstance(data, list)
    assert len(data) > 0
    assert data[0]["titulo"] is not None


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_contar_filmes_endpoint(anyio_backend):
    """Contrato do endpoint de contagem de filmes por gênero e ano.

    Espera status 200 e um objeto no formato do schema ContagemGenero.
    Deverá falhar (404) até o endpoint ser implementado.
    """

    async with LifespanManager(app):
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            params = {"genero": "Action", "ano": 2006}
            resp = await client.get("/contar-filmes", params=params)

    # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
    assert resp.status_code == 200

    data = resp.json()
    assert isinstance(data, dict)
    assert data["genero"] == "Action"
    assert data["ano"] == 2006
    assert data["contagem"] >= 0