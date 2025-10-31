import os
import redis.asyncio as redis


REDIS_URL = os.getenv("REDIS_URL", "redis://redis:6379/0")


class SessionManager:
    """Gerencia sessões e histórico de conversas usando Redis (async)."""

    def __init__(self) -> None:
        # decode_responses=True para strings em vez de bytes
        self.client: redis.Redis = redis.from_url(REDIS_URL, decode_responses=True)

    async def test_connection(self) -> None:
        """Ping simples para validar conexão na inicialização."""
        await self.client.ping()

    async def add_to_history(self, session_id: str, message: str, ttl_seconds: int = 3600) -> None:
        """Adiciona mensagem ao topo da lista e define TTL de expiração."""
        await self.client.lpush(session_id, message)
        await self.client.expire(session_id, ttl_seconds)

    async def get_history(self, session_id: str, limit: int = 5) -> list[str]:
        """Retorna os últimos `limit` itens do histórico (mais recentes primeiro)."""
        # LPUSH insere no início; LRANGE 0..limit-1 pega mais recentes
        return await self.client.lrange(session_id, 0, max(0, limit - 1))


# Instância global para uso pela aplicação
session_service = SessionManager()