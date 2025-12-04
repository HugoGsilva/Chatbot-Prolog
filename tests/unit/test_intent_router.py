"""
Testes para o IntentRouter.

Feature: thin-client-architecture
Property 7: Intent Routing Completeness
Validates: Requirements 4.2, 4.3
"""

import pytest
from unittest.mock import AsyncMock, MagicMock, patch

from app.intent_router import IntentRouter, get_intent_router
from app.schemas import NLUResult, ChatResponse, ResponseType


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def router() -> IntentRouter:
    """Retorna uma nova instância do IntentRouter."""
    return IntentRouter()


@pytest.fixture
def high_confidence_result() -> NLUResult:
    """NLUResult com alta confiança."""
    return NLUResult(
        intent="filmes_por_ator",
        entities={"ator": "tom hanks"},
        confidence=0.85,
        original_text="filmes por tom hanks",
    )


@pytest.fixture
def medium_confidence_result() -> NLUResult:
    """NLUResult com média confiança."""
    return NLUResult(
        intent="filmes_por_genero",
        entities={"genero": "ação"},
        confidence=0.55,
        original_text="filmes de ação",
    )


@pytest.fixture
def low_confidence_result() -> NLUResult:
    """NLUResult com baixa confiança."""
    return NLUResult(
        intent="unknown",
        entities={},
        confidence=0.25,
        original_text="xyz abc",
    )


# =============================================================================
# TESTES BÁSICOS
# =============================================================================

class TestIntentRouterBasic:
    """Testes básicos do IntentRouter."""
    
    def test_initialization(self, router):
        """Testa que o router inicializa corretamente."""
        assert router is not None
        assert len(router._handlers) > 0
    
    def test_singleton_pattern(self):
        """Testa padrão singleton."""
        router1 = get_intent_router()
        router2 = get_intent_router()
        assert router1 is router2
    
    def test_has_handler(self, router):
        """Testa verificação de handler."""
        assert router.has_handler("filmes_por_ator") is True
        assert router.has_handler("intent_inexistente") is False
    
    def test_get_supported_intents(self, router):
        """Testa obtenção de intenções suportadas."""
        intents = router.get_supported_intents()
        
        assert isinstance(intents, list)
        assert "filmes_por_ator" in intents
        assert "filmes_por_genero" in intents
        assert "filme_aleatorio" in intents


# =============================================================================
# PROPERTY 7: INTENT ROUTING COMPLETENESS
# =============================================================================

class TestProperty7IntentRoutingCompleteness:
    """
    Property 7: Intent Routing Completeness
    
    Para qualquer intenção detectada pelo NLUEngine que está definida em
    intent_patterns.json, o IntentRouter DEVE ter uma função handler
    correspondente que pode processar essa intenção.
    """
    
    REQUIRED_INTENTS = [
        "filmes_por_ator",
        "filmes_por_genero",
        "filmes_por_diretor",
        "genero_do_filme",
        "filme_aleatorio",
        "recomendar_ator_e_genero",
        "recomendar_dois_generos",
        "contar_filmes",
        "unknown",
    ]
    
    @pytest.mark.parametrize("intent", REQUIRED_INTENTS)
    def test_handler_exists_for_intent(self, router, intent):
        """Testa que cada intenção tem um handler."""
        assert router.has_handler(intent), \
            f"Handler não encontrado para intent '{intent}'"
    
    @pytest.mark.parametrize("intent", REQUIRED_INTENTS)
    def test_handler_is_callable(self, router, intent):
        """Testa que cada handler é chamável."""
        handler = router._handlers.get(intent)
        assert callable(handler), \
            f"Handler para '{intent}' não é chamável"


# =============================================================================
# TESTES DE CONFIDENCE THRESHOLDS
# =============================================================================

class TestConfidenceThresholds:
    """Testes para lógica de thresholds de confiança."""
    
    @pytest.mark.asyncio
    async def test_low_confidence_returns_help(self, router, low_confidence_result):
        """Testa que baixa confiança retorna resposta de ajuda."""
        with patch.object(router, '_create_help_response') as mock_help:
            mock_help.return_value = ChatResponse(
                type=ResponseType.HELP,
                content={"message": "Ajuda"}
            )
            
            response = await router.route(low_confidence_result, "session_123")
            
            mock_help.assert_called_once()
            assert response.type == ResponseType.HELP
    
    def test_threshold_values(self, router):
        """Testa que os thresholds estão configurados corretamente."""
        assert router.HIGH_CONFIDENCE == 0.7
        assert router.MEDIUM_CONFIDENCE == 0.4


# =============================================================================
# TESTES DE HANDLERS INDIVIDUAIS
# =============================================================================

class TestHandlers:
    """Testes para handlers individuais (com mocks)."""
    
    @pytest.mark.asyncio
    async def test_handle_filmes_por_ator_no_entity(self, router):
        """Testa handler de ator sem entidade."""
        response = await router.handle_filmes_por_ator({}, "session_123")
        
        assert response.type == ResponseType.ERROR
        assert "ator" in response.content.lower()
        assert response.suggestions is not None
    
    @pytest.mark.asyncio
    async def test_handle_filmes_por_genero_no_entity(self, router):
        """Testa handler de gênero sem entidade."""
        response = await router.handle_filmes_por_genero({}, "session_123")
        
        assert response.type == ResponseType.ERROR
        assert "gênero" in response.content.lower()
    
    @pytest.mark.asyncio
    async def test_handle_filmes_por_diretor_no_entity(self, router):
        """Testa handler de diretor sem entidade."""
        response = await router.handle_filmes_por_diretor({}, "session_123")
        
        assert response.type == ResponseType.ERROR
        assert "diretor" in response.content.lower()
    
    @pytest.mark.asyncio
    async def test_handle_genero_do_filme_no_entity(self, router):
        """Testa handler de gênero do filme sem entidade."""
        response = await router.handle_genero_do_filme({}, "session_123")
        
        assert response.type == ResponseType.ERROR
        assert "filme" in response.content.lower()
    
    @pytest.mark.asyncio
    async def test_handle_recomendar_ator_e_genero_missing_entities(self, router):
        """Testa handler de recomendação com entidades faltando."""
        # Sem ator
        response = await router.handle_recomendar_ator_e_genero(
            {"genero": "ação"}, "session_123"
        )
        assert response.type == ResponseType.ERROR
        
        # Sem gênero
        response = await router.handle_recomendar_ator_e_genero(
            {"ator": "tom hanks"}, "session_123"
        )
        assert response.type == ResponseType.ERROR
    
    @pytest.mark.asyncio
    async def test_handle_recomendar_dois_generos_missing_entities(self, router):
        """Testa handler de dois gêneros com entidades faltando."""
        response = await router.handle_recomendar_dois_generos(
            {"genero1": "ação"}, "session_123"
        )
        assert response.type == ResponseType.ERROR
    
    @pytest.mark.asyncio
    async def test_handle_contar_filmes_no_genre(self, router):
        """Testa handler de contagem sem gênero."""
        response = await router.handle_contar_filmes({}, "session_123")
        
        assert response.type == ResponseType.ERROR
    
    @pytest.mark.asyncio
    async def test_handle_unknown_returns_help(self, router):
        """Testa que handler unknown retorna ajuda."""
        response = await router.handle_unknown({}, "session_123")
        
        assert response.type == ResponseType.HELP


# =============================================================================
# TESTES DE RESPOSTA DE AJUDA
# =============================================================================

class TestHelpResponse:
    """Testes para resposta de ajuda."""
    
    def test_create_help_response_structure(self, router, low_confidence_result):
        """Testa estrutura da resposta de ajuda."""
        response = router._create_help_response(low_confidence_result)
        
        assert response.type == ResponseType.HELP
        assert isinstance(response.content, dict)
        assert "message" in response.content
        assert "examples" in response.content
    
    def test_help_response_has_examples(self, router, low_confidence_result):
        """Testa que resposta de ajuda tem exemplos."""
        response = router._create_help_response(low_confidence_result)
        
        examples = response.content["examples"]
        assert isinstance(examples, dict)
        assert len(examples) > 0


# =============================================================================
# TESTES DE RESPOSTA DE ERRO
# =============================================================================

class TestErrorResponse:
    """Testes para resposta de erro."""
    
    def test_create_error_response_structure(self, router, high_confidence_result):
        """Testa estrutura da resposta de erro."""
        response = router._create_error_response("Erro de teste", high_confidence_result)
        
        assert response.type == ResponseType.ERROR
        assert "Erro de teste" in response.content
        assert response.suggestions is not None
        assert response.metadata is not None


# =============================================================================
# TESTES DE INTEGRAÇÃO (COM MOCKS)
# =============================================================================

class TestIntegration:
    """Testes de integração com mocks."""
    
    @pytest.mark.asyncio
    async def test_route_adds_metadata(self, router):
        """Testa que route adiciona metadados à resposta."""
        nlu_result = NLUResult(
            intent="filme_aleatorio",
            entities={},
            confidence=0.9,
            original_text="filme aleatório",
        )
        
        with patch('app.intent_router.prolog_service') as mock_prolog:
            mock_prolog.query.return_value = [{"TituloFilme": "Test Movie"}]
            
            response = await router.route(nlu_result, "session_123")
            
            assert response.metadata is not None
            assert "intent" in response.metadata
            assert "confidence" in response.metadata
    
    @pytest.mark.asyncio
    async def test_route_handles_exceptions(self, router):
        """Testa que route trata exceções graciosamente."""
        nlu_result = NLUResult(
            intent="filmes_por_ator",
            entities={"ator": "test"},
            confidence=0.9,
            original_text="filmes por test",
        )
        
        with patch('app.intent_router.find_best_actor') as mock_find:
            mock_find.side_effect = Exception("Erro simulado")
            
            response = await router.route(nlu_result, "session_123")
            
            assert response.type == ResponseType.ERROR
