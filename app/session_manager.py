import os
import uuid
import logging
import redis.asyncio as redis
from typing import Optional

logger = logging.getLogger(__name__)

REDIS_URL = os.getenv("REDIS_URL", "redis://redis:6379/0")

# TTL de 24 horas em segundos
SESSION_TTL_SECONDS = 24 * 60 * 60  # 86400 segundos


class SessionManager:
    """Gerencia sessões e histórico de conversas usando Redis (async)."""

    def __init__(self) -> None:
        # decode_responses=True para strings em vez de bytes
        self.client: redis.Redis = redis.from_url(REDIS_URL, decode_responses=True)

    async def test_connection(self) -> None:
        """Ping simples para validar conexão na inicialização."""
        await self.client.ping()
    
    async def session_exists(self, session_id: str) -> bool:
        """
        Verifica se uma sessão existe no Redis.
        
        Args:
            session_id: ID da sessão
            
        Returns:
            True se a sessão existe, False caso contrário
        """
        return await self.client.exists(session_id) > 0
    
    async def create_session(self) -> str:
        """
        Cria uma nova sessão com ID único.
        
        Returns:
            ID da sessão criada
        """
        session_id = str(uuid.uuid4())
        # Inicializa a sessão com uma entrada vazia para garantir que existe
        await self.client.lpush(session_id, "__session_created__")
        await self.client.expire(session_id, SESSION_TTL_SECONDS)
        logger.info(f"Nova sessão criada: {session_id}")
        return session_id
    
    async def ensure_session_exists(self, session_id: str) -> bool:
        """
        Garante que a sessão existe, criando silenciosamente se expirou.
        
        Args:
            session_id: ID da sessão a verificar
            
        Returns:
            True se a sessão já existia, False se foi recriada
        """
        if await self.session_exists(session_id):
            return True
        
        # Sessão expirou ou nunca existiu - recriar silenciosamente
        logger.info(f"Sessão '{session_id}' não encontrada - recriando...")
        await self.client.lpush(session_id, "__session_recreated__")
        await self.client.expire(session_id, SESSION_TTL_SECONDS)
        return False
    
    async def delete_session(self, session_id: str) -> bool:
        """
        Remove uma sessão do Redis.
        
        Args:
            session_id: ID da sessão a remover
            
        Returns:
            True se a sessão foi removida, False se não existia
        """
        result = await self.client.delete(session_id)
        if result > 0:
            logger.info(f"Sessão removida: {session_id}")
            return True
        return False

    async def add_to_history(self, session_id: str, message: str, ttl_seconds: int = SESSION_TTL_SECONDS) -> None:
        """Adiciona mensagem ao topo da lista e define TTL de expiração (24h default)."""
        await self.client.lpush(session_id, message)
        await self.client.expire(session_id, ttl_seconds)

    async def get_history(self, session_id: str, limit: int = 10) -> list[str]:
        """Retorna os últimos `limit` itens do histórico (mais recentes primeiro)."""
        # LPUSH insere no início; LRANGE 0..limit-1 pega mais recentes
        history = await self.client.lrange(session_id, 0, max(0, limit - 1))
        # Filtra mensagens de sistema
        return [msg for msg in history if not msg.startswith("__")]
    
    async def get_session_ttl(self, session_id: str) -> int:
        """
        Retorna o TTL restante da sessão em segundos.
        
        Returns:
            TTL em segundos, -2 se não existe, -1 se não tem expiração
        """
        return await self.client.ttl(session_id)


# Instância global para uso pela aplicação
session_service = SessionManager()