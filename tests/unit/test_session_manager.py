import pytest
from unittest.mock import AsyncMock

from app.session_manager import session_service


@pytest.mark.parametrize("anyio_backend", ["asyncio"]) 
@pytest.mark.anyio
async def test_add_to_history(anyio_backend, monkeypatch: pytest.MonkeyPatch) -> None:
    # Mocka o cliente Redis assíncrono
    mock_client = AsyncMock()
    monkeypatch.setattr(session_service, "client", mock_client)

    await session_service.add_to_history("sessao_teste", "ola")

    # Verifica chamadas corretas
    mock_client.lpush.assert_called_once_with("sessao_teste", "ola")
    mock_client.expire.assert_called_once()


@pytest.mark.parametrize("anyio_backend", ["asyncio"]) 
@pytest.mark.anyio
async def test_get_history(anyio_backend, monkeypatch: pytest.MonkeyPatch) -> None:
    # Mocka o cliente Redis e simula retorno do lrange
    mock_client = AsyncMock()
    mock_client.lrange.return_value = ["mundo", "ola"]
    monkeypatch.setattr(session_service, "client", mock_client)

    result = await session_service.get_history("sessao_teste", limit=5)

    # limit=5 -> lrange(0, 4)
    mock_client.lrange.assert_called_once_with("sessao_teste", 0, 4)
    assert result == ["mundo", "ola"]