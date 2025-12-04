"""
Rate Limiter - Limitador de Taxa de Requisições

Implementa rate limiting baseado em IP e sessão usando Redis
para proteger o endpoint /chat contra abuso.

Limites:
- Por IP: 20 requisições por minuto
- Por Sessão: 10 requisições por minuto
"""

import logging
import time
from typing import Optional, Tuple
from fastapi import Request, HTTPException

logger = logging.getLogger(__name__)


class RateLimitExceeded(Exception):
    """Exceção levantada quando o rate limit é excedido."""
    
    def __init__(self, reason: str, message: str = None):
        self.reason = reason
        if message is None:
            if reason == "ip_limit":
                message = "Limite de requisições por IP excedido. Aguarde 1 minuto."
            else:
                message = "Limite de requisições por sessão excedido. Aguarde 1 minuto."
        self.message = message
        super().__init__(self.message)


class RateLimiter:
    """
    Limitador de taxa de requisições usando Redis.
    
    Implementa sliding window rate limiting para IP e sessão.
    """
    
    # Limites de taxa
    IP_LIMIT = 20  # requisições por minuto
    SESSION_LIMIT = 10  # requisições por minuto
    WINDOW_SECONDS = 60  # janela de 1 minuto
    
    def __init__(self, redis_client):
        """
        Inicializa o rate limiter.
        
        Args:
            redis_client: Cliente Redis async
        """
        self.redis = redis_client
    
    async def check_rate_limit(
        self, 
        request: Request, 
        session_id: Optional[str] = None
    ) -> Tuple[bool, Optional[str]]:
        """
        Verifica se a requisição está dentro dos limites de taxa.
        
        Args:
            request: Request FastAPI para extrair IP
            session_id: ID da sessão (opcional)
            
        Returns:
            Tuple (permitido, motivo_bloqueio)
            - (True, None) se permitido
            - (False, "ip_limit" ou "session_limit") se bloqueado
        """
        client_ip = self._get_client_ip(request)
        current_time = int(time.time())
        
        # Verifica limite por IP
        ip_key = f"rate_limit:ip:{client_ip}"
        ip_allowed = await self._check_limit(ip_key, self.IP_LIMIT, current_time)
        
        if not ip_allowed:
            logger.warning(f"Rate limit por IP excedido: {client_ip}")
            return False, "ip_limit"
        
        # Verifica limite por sessão (se fornecido)
        if session_id:
            session_key = f"rate_limit:session:{session_id}"
            session_allowed = await self._check_limit(session_key, self.SESSION_LIMIT, current_time)
            
            if not session_allowed:
                logger.warning(f"Rate limit por sessão excedido: {session_id}")
                return False, "session_limit"
        
        return True, None
    
    async def _check_limit(
        self, 
        key: str, 
        limit: int, 
        current_time: int
    ) -> bool:
        """
        Verifica e incrementa contador para uma chave específica.
        
        Usa sliding window log pattern.
        
        Args:
            key: Chave Redis para o contador
            limit: Número máximo de requisições
            current_time: Timestamp atual
            
        Returns:
            True se dentro do limite, False caso contrário
        """
        window_start = current_time - self.WINDOW_SECONDS
        
        # Remove entradas antigas da janela
        await self.redis.zremrangebyscore(key, 0, window_start)
        
        # Conta requisições na janela atual
        request_count = await self.redis.zcard(key)
        
        if request_count >= limit:
            return False
        
        # Adiciona nova requisição com timestamp como score
        await self.redis.zadd(key, {str(current_time): current_time})
        
        # Define expiração da chave (limpeza automática)
        await self.redis.expire(key, self.WINDOW_SECONDS + 10)
        
        return True
    
    def _get_client_ip(self, request: Request) -> str:
        """
        Extrai o IP do cliente da requisição.
        
        Verifica headers de proxy (X-Forwarded-For, X-Real-IP) primeiro.
        
        Args:
            request: Request FastAPI
            
        Returns:
            IP do cliente como string
        """
        # Verifica headers de proxy
        forwarded_for = request.headers.get("X-Forwarded-For")
        if forwarded_for:
            # Pega o primeiro IP (cliente original)
            return forwarded_for.split(",")[0].strip()
        
        real_ip = request.headers.get("X-Real-IP")
        if real_ip:
            return real_ip.strip()
        
        # Fallback para IP direto
        if request.client:
            return request.client.host
        
        return "unknown"
    
    async def get_remaining_requests(
        self, 
        request: Request, 
        session_id: Optional[str] = None
    ) -> dict:
        """
        Retorna informações sobre o limite de taxa atual.
        
        Args:
            request: Request FastAPI
            session_id: ID da sessão (opcional)
            
        Returns:
            Dict com informações de limite
        """
        client_ip = self._get_client_ip(request)
        current_time = int(time.time())
        window_start = current_time - self.WINDOW_SECONDS
        
        # Conta requisições por IP
        ip_key = f"rate_limit:ip:{client_ip}"
        await self.redis.zremrangebyscore(ip_key, 0, window_start)
        ip_count = await self.redis.zcard(ip_key)
        
        result = {
            "ip": client_ip,
            "ip_requests": ip_count,
            "ip_limit": self.IP_LIMIT,
            "ip_remaining": max(0, self.IP_LIMIT - ip_count),
            "window_seconds": self.WINDOW_SECONDS,
        }
        
        if session_id:
            session_key = f"rate_limit:session:{session_id}"
            await self.redis.zremrangebyscore(session_key, 0, window_start)
            session_count = await self.redis.zcard(session_key)
            
            result.update({
                "session_id": session_id,
                "session_requests": session_count,
                "session_limit": self.SESSION_LIMIT,
                "session_remaining": max(0, self.SESSION_LIMIT - session_count),
            })
        
        return result


# Instância global (será inicializada com o cliente Redis)
rate_limiter: Optional[RateLimiter] = None


def create_rate_limit_response(reason: str) -> dict:
    """
    Cria resposta de erro para rate limiting.
    
    Args:
        reason: "ip_limit" ou "session_limit"
        
    Returns:
        Dict com mensagem de erro
    """
    if reason == "ip_limit":
        message = "Limite de requisições por IP excedido. Aguarde 1 minuto."
    else:
        message = "Limite de requisições por sessão excedido. Aguarde 1 minuto."
    
    return {
        "type": "error",
        "content": message,
        "suggestions": ["Aguarde um momento antes de enviar outra mensagem"],
        "metadata": {"error_code": "RATE_LIMIT_EXCEEDED", "reason": reason}
    }


async def check_rate_limit(
    rate_limiter: RateLimiter, 
    client_ip: str, 
    session_id: Optional[str] = None
) -> None:
    """
    Função auxiliar para verificar rate limit sem objeto Request.
    
    Esta função é uma versão simplificada que aceita IP como string
    em vez de extrair do Request.
    
    Args:
        rate_limiter: Instância do RateLimiter
        client_ip: IP do cliente como string
        session_id: ID da sessão (opcional)
        
    Raises:
        RateLimitExceeded: Se o limite foi excedido
    """
    current_time = int(time.time())
    
    # Verifica limite por IP
    ip_key = f"rate_limit:ip:{client_ip}"
    ip_allowed = await rate_limiter._check_limit(ip_key, rate_limiter.IP_LIMIT, current_time)
    
    if not ip_allowed:
        logger.warning(f"Rate limit por IP excedido: {client_ip}")
        raise RateLimitExceeded("ip_limit")
    
    # Verifica limite por sessão (se fornecido)
    if session_id:
        session_key = f"rate_limit:session:{session_id}"
        session_allowed = await rate_limiter._check_limit(session_key, rate_limiter.SESSION_LIMIT, current_time)
        
        if not session_allowed:
            logger.warning(f"Rate limit por sessão excedido: {session_id}")
            raise RateLimitExceeded("session_limit")

