"""
Info Handlers - Handlers para intenções de informação e navegação

Handlers simples que fornecem ajuda, saudações, e informações sobre o bot.
"""

from typing import Dict

from .base_handler import BaseHandler
from ..schemas import ChatResponse, ResponseType


class InfoHandlers(BaseHandler):
    """Handlers para intenções informativas."""
    
    async def handle_ajuda(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """
        Handler para intenção 'ajuda'.
        
        Retorna informações sobre como usar o chatbot.
        """
        help_content = {
            "message": "👋 Olá! Sou o chatbot de filmes Netflix. Posso ajudar você a:",
            "examples": {
                "Buscar por ator": ["filmes com Adam Sandler", "filmes com Denzel Washington"],
                "Buscar por gênero": ["filmes de ação", "filmes de comédia"],
                "Buscar por diretor": ["filmes do Diretor Quentin Tarantino", "filmes do Diretor Steven Spielberg"],
                "Descobrir gênero": ["gênero de Jaws", "qual o tipo de Grown Ups"],
                "Recomendações": ["recomende um filme de terror", "sugira um drama"],
                "Filme aleatório": ["filme aleatório", "me surpreenda"]
            }
        }
        
        return ChatResponse(
            type=ResponseType.HELP,
            content=help_content,
            suggestions=[],
        )
    
    async def handle_saudacao(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """
        Handler para intenção 'saudacao'.
        
        Responde a saudações do usuário.
        """
        return ChatResponse(
            type=ResponseType.TEXT,
            content="Olá! 👋 Sou o chatbot de filmes Netflix. Como posso ajudar? Digite 'ajuda' para ver o que posso fazer.",
            suggestions=[],
        )
    
    async def handle_identidade(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """
        Handler para intenção 'identidade'.
        
        Responde a perguntas sobre quem é o bot.
        """
        return ChatResponse(
            type=ResponseType.TEXT,
            content="🤖 Olá! Eu sou o **Chatbot Netflix**, um assistente virtual especializado em filmes. "
                    "Fui criado para ajudar você a descobrir filmes por ator, gênero, diretor, "
                    "obter recomendações e muito mais! Digite 'ajuda' para ver tudo que posso fazer.",
            suggestions=[],
        )
    
    async def handle_despedida(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """
        Handler para intenção 'despedida'.
        
        Responde a despedidas do usuário.
        """
        return ChatResponse(
            type=ResponseType.TEXT,
            content="👋 Até logo! Foi um prazer ajudar. Volte sempre que precisar de recomendações de filmes!",
            suggestions=[],
        )
    
    async def handle_small_talk(
        self, 
        entities: Dict[str, str], 
        session_id: str
    ) -> ChatResponse:
        """
        Handler para intenção 'small_talk'.
        
        Responde a perguntas genéricas fora do escopo.
        """
        return ChatResponse(
            type=ResponseType.TEXT,
            content="🤔 Essa é uma pergunta interessante, mas sou especializado em filmes! "
                    "Posso te ajudar a encontrar um bom filme para assistir?",
            suggestions=["ajuda", "filme aleatório", "filmes de drama"],
        )
