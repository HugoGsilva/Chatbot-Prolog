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
    QUERY_FUZZY = "actor a"  # O input "sujo" (existe no KB)
    NOME_ATOR_REAL = "ACTOR A"  # O nome na cache NLU (existe)
    FILME_ESPERADO = "Sample Film"  # Um filme desse ator (s1)

    # --- Mocking do SessionService e Caches do Redis ---
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()

    # Prepara respostas simuladas para as caches NLU no Redis
    actores_cache = [NOME_ATOR_REAL, "ACTOR B", "ACTOR C", "ACTOR D", "ACTOR E"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION", "ROMANCE", "THRILLER"]
    filmes_cache = [FILME_ESPERADO, "Another Title", "Romantic Story"]
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


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_get_filmes_por_genero_v2(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """
    Testa o endpoint V2 (Netflix) /filmes-por-genero.
    Usa uma query "fuzzy" (Nível 1 e 2), incluindo tradução PT-BR.
    Deve falhar com 404 até o endpoint ser implementado.
    """

    # --- Dados de Teste (AJUSTE CONFORME O SEU CSV DE AMOSTRA) ---
    QUERY_FUZZY = "drama"  # O input "sujo" (ex: "dram", "acao")
    GENERO_REAL_TRADUZIDO = "DRAMA"  # Nome na cache NLU (PT ou EN)
    FILME_ESPERADO = "Sample Film"  # Um filme desse género

    # --- Mocking do SessionService e Caches do Redis ---
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()

    # Prepara respostas simuladas para as caches (atores, géneros, filmes)
    actores_cache = ["ACTOR A", "ACTOR B", "ACTOR C", "ACTOR D", "ACTOR E"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION", "ROMANCE", "THRILLER"]
    filmes_cache = ["Sample Film", "Another Title", "Romantic Story"]
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(actores_cache),
        json.dumps(generos_cache),
        json.dumps(filmes_cache),
    ])

    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    # --- Execução ---
    async with LifespanManager(app):  # Inicia o startup "leve"
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            resp = await client.get(
                f"/filmes-por-genero/{quote(QUERY_FUZZY)}?session_id=sessao_v2_genero"
            )

    # --- Asserções (TDD "Red") ---
    assert resp.status_code == 200  # (Isto deve falhar com 404)

    # (Asserções que vão passar quando o 200 OK for corrigido)
    data = resp.json()
    assert isinstance(data, list)
    assert data[0]["titulo"] == FILME_ESPERADO

    # Validar histórico (NLU Nível 1)
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_genero", f"User: {QUERY_FUZZY}"
    )
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_genero", f"Bot: {data}"
    )


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_get_genero_do_filme_v2(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """
    Testa o endpoint V2 (Netflix) /genero-do-filme.
    Usa uma query "fuzzy" (Nível 1 e 2).
    Deve falhar com 404 até o endpoint ser implementado.
    """

    # --- Dados de Teste (AJUSTE CONFORME O SEU CSV DE AMOSTRA) ---
    QUERY_FUZZY = "sample film"  # O input "sujo"
    FILME_REAL = "Sample Film"    # O nome na cache NLU
    GENERO_ESPERADO = "DRAMA"     # O género desse filme

    # --- Mocking (igual aos outros) ---
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()

    # Prepara respostas simuladas para as caches (atores, géneros, filmes)
    actores_cache = ["ACTOR A", "ACTOR B", "ACTOR C", "ACTOR D", "ACTOR E"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION", "ROMANCE", "THRILLER"]
    filmes_cache = ["Sample Film", "Another Title", "Romantic Story"]
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(actores_cache),
        json.dumps(generos_cache),
        json.dumps(filmes_cache),
    ])

    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    # --- Execução ---
    async with LifespanManager(app):  # Inicia o startup "leve"
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            resp = await client.get(
                f"/genero-do-filme/{quote(QUERY_FUZZY)}?session_id=sessao_v2_filme"
            )

    # --- Asserções (TDD "Red") ---
    assert resp.status_code == 200  # (Isto deve falhar com 404)

    # (Asserções que vão passar quando o 200 OK for corrigido)
    # (Este endpoint retorna uma LISTA de géneros)
    data = resp.json()
    assert isinstance(data, list)
    assert len(data) > 0
    assert data[0]["nome"] == GENERO_ESPERADO  # Verifica se o primeiro género está correto

    # Validar histórico (NLU Nível 1)
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_filme", f"User: {QUERY_FUZZY}"
    )
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_filme", f"Bot: {data}"
    )


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_contar_filmes_endpoint_v2(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """
    Testa o endpoint V2 (Netflix) /contar-filmes.
    Usa uma query "fuzzy" (Nível 1 e 2).
    Deve falhar com 404 até o endpoint ser implementado.
    """

    # --- Dados de Teste (AJUSTE CONFORME O SEU CSV DE AMOSTRA) ---
    QUERY_FUZZY_GENERO = "drama"  # Input "sujo"
    QUERY_ANO = 2020
    GENERO_REAL = "DRAMA"
    CONTAGEM_ESPERADA = 1  # Na amostra: 1 filme DRAMA em 2020

    # --- Mocking (igual aos outros) ---
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()

    # Prepara respostas simuladas para as caches (atores, géneros, filmes)
    actores_cache = ["ACTOR A", "ACTOR B", "ACTOR C", "ACTOR D", "ACTOR E"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION", "ROMANCE", "THRILLER"]
    filmes_cache = ["Sample Film", "Another Title", "Romantic Story"]
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(actores_cache),
        json.dumps(generos_cache),
        json.dumps(filmes_cache),
    ])

    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    # --- Execução ---
    async with LifespanManager(app):  # Inicia o startup "leve"
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            params = {
                "genero": QUERY_FUZZY_GENERO,
                "ano": QUERY_ANO,
                "session_id": "sessao_v2_contagem",
            }
            resp = await client.get("/contar-filmes", params=params)

    # --- Asserções (TDD "Red") ---
    assert resp.status_code == 200  # (Isto deve falhar com 404)

    # (Asserções que vão passar quando o 200 OK for corrigido)
    data = resp.json()
    assert isinstance(data, dict)
    assert data["genero"] == GENERO_REAL
    assert data["ano"] == QUERY_ANO
    assert data["contagem"] == CONTAGEM_ESPERADA

    # Validar histórico (NLU Nível 1)
    user_query = f"genero={QUERY_FUZZY_GENERO}, ano={QUERY_ANO}"
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_contagem", f"User: {user_query}"
    )
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_contagem", f"Bot: {data}"
    )


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_recomendar_ator_e_genero_v2(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """
    Testa o novo endpoint V2 (Netflix) /recomendar/ator-e-genero.
    Usa queries "fuzzy" (Nível 1 e 2).
    Deve falhar com 404 até o endpoint ser implementado.
    """

    # --- Dados de Teste (AJUSTE CONFORME O SEU CSV DE AMOSTRA) ---
    QUERY_FUZZY_ATOR = "actor a"  # Input "sujo"
    QUERY_FUZZY_GENERO = "drama"   # Input "sujo"
    FILME_ESPERADO = "Sample Film"  # Um filme que satisfaz AMBOS

    # --- Mocking (igual aos outros) ---
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()

    # Prepara respostas simuladas para as caches (atores, géneros, filmes)
    actores_cache = ["ACTOR A", "ACTOR B", "ACTOR C", "ACTOR D", "ACTOR E"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION", "ROMANCE", "THRILLER"]
    filmes_cache = ["Sample Film", "Another Title", "Romantic Story"]
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(actores_cache),
        json.dumps(generos_cache),
        json.dumps(filmes_cache),
    ])

    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    # --- Execução ---
    async with LifespanManager(app):  # Inicia o startup "leve"
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            params = {
                "ator": QUERY_FUZZY_ATOR,
                "genero": QUERY_FUZZY_GENERO,
                "session_id": "sessao_v2_rec_composta",
            }
            resp = await client.get("/recomendar/ator-e-genero", params=params)

    # --- Asserções (TDD "Red") ---
    assert resp.status_code == 200  # (Isto deve falhar com 404)

    # (Asserções que vão passar quando o 200 OK for corrigido)
    data = resp.json()
    assert isinstance(data, list)
    assert len(data) > 0
    assert data[0]["titulo"] == FILME_ESPERADO

    # Validar histórico (NLU Nível 1)
    user_query = f"ator={QUERY_FUZZY_ATOR}, genero={QUERY_FUZZY_GENERO}"
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_rec_composta", f"User: {user_query}"
    )
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_rec_composta", f"Bot: {data}"
    )


@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_recomendar_dois_generos_v2(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """
    Testa o novo endpoint V2 (Netflix) /recomendar/dois-generos.
    Usa queries "fuzzy" (Nível 1 e 2).
    Deve falhar com 404 até o endpoint ser implementado.
    """

    # --- Dados de Teste (AJUSTE CONFORME O SEU CSV DE AMOSTRA) ---
    QUERY_FUZZY_GENERO1 = "drama"  # Input "sujo"
    QUERY_FUZZY_GENERO2 = "comdy"  # Input "sujo"
    FILME_ESPERADO = "Sample Film"  # Ajustado ao dataset de amostra atual (DRAMA & COMEDY)

    # --- Mocking (igual aos outros) ---
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()

    # Prepara respostas simuladas para as caches (atores, géneros, filmes)
    actores_cache = ["ACTOR A", "ACTOR B", "ACTOR C", "ACTOR D", "ACTOR E"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION", "ROMANCE", "THRILLER"]
    filmes_cache = ["Sample Film", "Another Title", "Romantic Story"]
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(actores_cache),
        json.dumps(generos_cache),
        json.dumps(filmes_cache),
    ])

    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    # --- Execução ---
    async with LifespanManager(app):  # Inicia o startup "leve"
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            params = {
                "genero1": QUERY_FUZZY_GENERO1,
                "genero2": QUERY_FUZZY_GENERO2,
                "session_id": "sessao_v2_rec_generos",
            }
            resp = await client.get("/recomendar/dois-generos", params=params)

    # --- Asserções (TDD "Red") ---
    assert resp.status_code == 200  # (Isto deve falhar com 404)

    # (Asserções que vão passar quando o 200 OK for corrigido)
    data = resp.json()
    assert isinstance(data, list)
    assert len(data) > 0
    assert data[0]["titulo"] == FILME_ESPERADO

    # Validar histórico (NLU Nível 1)
    user_query = f"genero1={QUERY_FUZZY_GENERO1}, genero2={QUERY_FUZZY_GENERO2}"
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_rec_generos", f"User: {user_query}"
    )
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_rec_generos", f"Bot: {data}"
    )
@pytest.mark.parametrize("anyio_backend", ["asyncio"])
@pytest.mark.anyio
async def test_get_filmes_por_diretor_v2(anyio_backend, monkeypatch: pytest.MonkeyPatch):
    """
    Testa o novo endpoint V2 /filmes-por-diretor.
    Usa uma query "fuzzy" (Nível 1 e 2).
    Deve falhar com 404 até o endpoint ser implementado.
    """

    # --- Dados de Teste ---
    QUERY_FUZZY = "mike flanagan"
    NOME_DIRETOR_REAL = "MIKE FLANAGAN"
    FILME_ESPERADO = "Midnight Mass"  # Ajustado conforme retorno do Prolog

    # --- Mocking do SessionService e Caches do Redis ---
    mock_session_service = AsyncMock()
    mock_session_service.add_to_history = AsyncMock()
    mock_session_service.test_connection = AsyncMock()
    mock_client = AsyncMock()
    mock_client.aclose = AsyncMock()

    # Prepara respostas simuladas para as caches NLU no Redis
    actores_cache = ["ACTOR A", "ACTOR B", "ACTOR C", "ACTOR D", "ACTOR E"]
    generos_cache = ["DRAMA", "COMEDY", "ACTION", "ROMANCE", "THRILLER"]
    filmes_cache = [FILME_ESPERADO, "Another Title", "Romantic Story"]
    diretores_cache = [NOME_DIRETOR_REAL, "DIRECTOR B", "DIRECTOR C"]
    mock_client.get = AsyncMock(side_effect=[
        json.dumps(actores_cache),
        json.dumps(generos_cache),
        json.dumps(filmes_cache),
        json.dumps(diretores_cache),
    ])

    mock_session_service.client = mock_client
    monkeypatch.setattr("app.main.session_service", mock_session_service)

    # --- Execução ---
    async with LifespanManager(app):  # Inicia o startup "leve" (lê caches do Redis)
        transport = httpx.ASGITransport(app=app)
        async with httpx.AsyncClient(transport=transport, base_url="http://test") as client:
            resp = await client.get(
                f"/filmes-por-diretor/{quote(QUERY_FUZZY)}?session_id=sessao_v2_diretor"
            )

    # --- Asserções (TDD "Red") ---
    assert resp.status_code == 200  # (Isto deve falhar com 404)

    data = resp.json()
    assert isinstance(data, list)
    assert data[0]["titulo"] == FILME_ESPERADO

    # Validar histórico (NLU Nível 1)
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_diretor", f"User: {QUERY_FUZZY}"
    )
    mock_session_service.add_to_history.assert_any_call(
        "sessao_v2_diretor", f"Bot: {data}"
    )