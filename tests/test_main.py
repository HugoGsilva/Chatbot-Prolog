import pytest
import httpx
from urllib.parse import quote

from app.main import app


@pytest.mark.asyncio
async def test_get_filmes_por_ator_endpoint():
    """Verifica contrato esperado do endpoint de filmes por ator.

    Espera status 200 e uma lista de objetos com campo `titulo`.
    Deverá falhar (404) até o endpoint ser implementado.
    """

    # Garante execução de eventos de lifespan (startup) para carregar Prolog
    transport = httpx.ASGITransport(app=app, lifespan="on")

    async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
        ator = "PENELOPE GUINESS"  # ator existente no subset real de sakila.pl
        ator_encoded = quote(ator)
        resp = await client.get(f"/filmes-por-ator/{ator_encoded}")

        # TDD: status esperado 200 (vai falhar com 404 até implementarmos o endpoint)
        assert resp.status_code == 200

        data = resp.json()
        assert isinstance(data, list)
        assert len(data) > 0
        assert data[0]["titulo"] is not None