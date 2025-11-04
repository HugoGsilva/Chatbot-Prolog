import json
import pytest
import httpx
from urllib.parse import quote
from asgi_lifespan import LifespanManager
from unittest.mock import AsyncMock

# Importa a app V2
from app.main import app

# Importa Schemas V2 (para futura validação de payloads)
from app.schemas import Filme


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_get_filmes_por_ator_v2(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """
    Testa o endpoint V2 (Netflix) /filmes-por-ator.
    Usa uma query "fuzzy" (Nível 1 e 2).
    Deve falhar com 404 até o endpoint ser implementado.
    """

    # --- Dados de Teste (AJUSTE CONFORME O SEU CSV DE AMOSTRA) ---
    QUERY_FUZZY = "tom hanks"  # O input "sujo"
    NOME_ATOR_REAL = "TOM HANKS"  # O nome na cache NLU
    FILME_ESPERADO = "FORREST GUMP"  # Um filme que ele fez

    # --- Mocking do SessionService e Caches do Redis ---
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()

    # Prepara respostas simuladas para as caches NLU no Redis
    actores_cache = [NOME_ATOR_REAL, "ACTOR A", "ACTOR B", "ACTOR C", "ACTOR D"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION", "ROMANCE", "THRILLER"]
    filmes_cache = [FILME_ESPERADO, "MOVIE A", "MOVIE B"]
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(actores_cache),
        json.dumps(generos_cache),
        json.dumps(filmes_cache),
    ])

    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    # --- Execução ---
    async with LifespanManager(app):  # Inicia o startup "leve" (lê caches do Redis)
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            resp = await client.get(
                f"/filmes-por-ator/{quote(QUERY_FUZZY)}?session_id=sessao_v2_ator"
            )

    # --- Asserções (TDD "Red") ---
    assert resp.status_code == 200  # (Isto deve falhar com 404)

    # (Asserções que vão passar quando o 200 OK for corrigido)
    data = resp.json()
    assert isinstance(data, list)
    assert data[0]["titulo"] == FILME_ESPERADO

    # Validar histórico (NLU Nível 1)
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_ator", f"User: {QUERY_FUZZY}"
    )
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_ator", f"Bot: {data}"
    )