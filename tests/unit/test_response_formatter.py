"""
Testes para o ResponseFormatter.

Feature: thin-client-architecture
Property 1: Response Structure Consistency
Validates: Requirements 1.3, 4.4, 6.1, 6.2, 6.3, 6.4, 6.5
"""

import pytest
from typing import Dict, List

from app.response_formatter import ResponseFormatter, get_response_formatter
from app.schemas import ChatResponse, ResponseType


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def formatter() -> ResponseFormatter:
    """Retorna uma instância do ResponseFormatter."""
    return ResponseFormatter()


@pytest.fixture
def sample_filmes() -> List[Dict]:
    """Lista de filmes de exemplo."""
    return [
        {"titulo": "Forrest Gump"},
        {"titulo": "Cast Away"},
        {"titulo": "The Green Mile"},
    ]


@pytest.fixture
def sample_generos() -> List[Dict]:
    """Lista de gêneros de exemplo."""
    return [
        {"nome": "DRAMA"},
        {"nome": "ACTION"},
    ]


# =============================================================================
# TESTES BÁSICOS
# =============================================================================

class TestResponseFormatterBasic:
    """Testes básicos do ResponseFormatter."""
    
    def test_singleton(self):
        """Testa padrão singleton."""
        f1 = get_response_formatter()
        f2 = get_response_formatter()
        assert f1 is f2
    
    def test_format_text_returns_chat_response(self, formatter):
        """Testa que format_text retorna ChatResponse."""
        response = formatter.format_text("Teste")
        
        assert isinstance(response, ChatResponse)
        assert response.type == ResponseType.TEXT
        assert response.content == "Teste"
    
    def test_format_list_returns_chat_response(self, formatter, sample_filmes):
        """Testa que format_list retorna ChatResponse."""
        response = formatter.format_list(sample_filmes)
        
        assert isinstance(response, ChatResponse)
        assert response.type == ResponseType.LIST
        assert response.content == sample_filmes
    
    def test_format_error_returns_chat_response(self, formatter):
        """Testa que format_error retorna ChatResponse."""
        response = formatter.format_error("Erro de teste")
        
        assert isinstance(response, ChatResponse)
        assert response.type == ResponseType.ERROR
        assert "Erro de teste" in response.content
    
    def test_format_help_returns_chat_response(self, formatter):
        """Testa que format_help retorna ChatResponse."""
        response = formatter.format_help(
            message="Mensagem de ajuda",
            examples={"Categoria": ["exemplo1", "exemplo2"]}
        )
        
        assert isinstance(response, ChatResponse)
        assert response.type == ResponseType.HELP
        assert "message" in response.content
        assert "examples" in response.content


# =============================================================================
# PROPERTY 1: RESPONSE STRUCTURE CONSISTENCY
# =============================================================================

class TestProperty1ResponseStructure:
    """
    Property 1: Response Structure Consistency
    
    Para qualquer mensagem válida processada pelo endpoint /chat, a resposta
    DEVE incluir um campo "type" com um dos tipos válidos (text, list, error,
    clarification, help), e o campo "content" DEVE corresponder à estrutura
    esperada para esse tipo.
    """
    
    def test_text_response_has_type_field(self, formatter):
        """Testa que resposta TEXT tem campo type."""
        response = formatter.format_text("Teste")
        
        assert hasattr(response, 'type')
        assert response.type == ResponseType.TEXT
    
    def test_text_response_content_is_string(self, formatter):
        """Testa que content de TEXT é string."""
        response = formatter.format_text("Mensagem de teste")
        
        assert isinstance(response.content, str)
    
    def test_list_response_has_type_field(self, formatter, sample_filmes):
        """Testa que resposta LIST tem campo type."""
        response = formatter.format_list(sample_filmes)
        
        assert hasattr(response, 'type')
        assert response.type == ResponseType.LIST
    
    def test_list_response_content_is_list_or_dict(self, formatter, sample_filmes):
        """Testa que content de LIST é lista ou dict."""
        # Sem mensagem - retorna lista
        response = formatter.format_list(sample_filmes)
        assert isinstance(response.content, list)
        
        # Com mensagem - retorna dict
        response = formatter.format_list(sample_filmes, message="Encontrei filmes")
        assert isinstance(response.content, dict)
        assert "items" in response.content
    
    def test_error_response_has_type_field(self, formatter):
        """Testa que resposta ERROR tem campo type."""
        response = formatter.format_error("Erro")
        
        assert hasattr(response, 'type')
        assert response.type == ResponseType.ERROR
    
    def test_error_response_content_is_string(self, formatter):
        """Testa que content de ERROR é string."""
        response = formatter.format_error("Mensagem de erro")
        
        assert isinstance(response.content, str)
    
    def test_help_response_has_type_field(self, formatter):
        """Testa que resposta HELP tem campo type."""
        response = formatter.format_help(
            message="Ajuda",
            examples={"cat": ["ex1"]}
        )
        
        assert hasattr(response, 'type')
        assert response.type == ResponseType.HELP
    
    def test_help_response_content_is_dict(self, formatter):
        """Testa que content de HELP é dict com message e examples."""
        response = formatter.format_help(
            message="Ajuda",
            examples={"cat": ["ex1"]}
        )
        
        assert isinstance(response.content, dict)
        assert "message" in response.content
        assert "examples" in response.content
    
    def test_clarification_response_has_type_field(self, formatter):
        """Testa que resposta CLARIFICATION tem campo type."""
        response = formatter.format_clarification(
            message="Você quis dizer?",
            options=["opção 1", "opção 2"]
        )
        
        assert hasattr(response, 'type')
        assert response.type == ResponseType.CLARIFICATION
    
    def test_clarification_response_content_structure(self, formatter):
        """Testa estrutura de content de CLARIFICATION."""
        response = formatter.format_clarification(
            message="Você quis dizer?",
            options=["opção 1", "opção 2"]
        )
        
        assert isinstance(response.content, dict)
        assert "message" in response.content
        assert "options" in response.content
    
    def test_all_response_types_have_valid_type(self, formatter, sample_filmes):
        """Testa que todos os tipos de resposta são válidos."""
        valid_types = {ResponseType.TEXT, ResponseType.LIST, ResponseType.ERROR, 
                       ResponseType.CLARIFICATION, ResponseType.HELP}
        
        responses = [
            formatter.format_text("texto"),
            formatter.format_list(sample_filmes),
            formatter.format_error("erro"),
            formatter.format_help("ajuda", {}),
            formatter.format_clarification("clarificação", []),
        ]
        
        for response in responses:
            assert response.type in valid_types


# =============================================================================
# TESTES DE SUGESTÕES
# =============================================================================

class TestSuggestions:
    """Testes para sugestões nas respostas."""
    
    def test_text_with_suggestions(self, formatter):
        """Testa texto com sugestões."""
        suggestions = ["sugestão 1", "sugestão 2"]
        response = formatter.format_text("Teste", suggestions=suggestions)
        
        assert response.suggestions == suggestions
    
    def test_list_with_suggestions(self, formatter, sample_filmes):
        """Testa lista com sugestões."""
        suggestions = ["outra busca"]
        response = formatter.format_list(sample_filmes, suggestions=suggestions)
        
        assert response.suggestions == suggestions
    
    def test_error_with_suggestions(self, formatter):
        """Testa erro com sugestões."""
        suggestions = ["tente isso"]
        response = formatter.format_error("Erro", suggestions=suggestions)
        
        assert response.suggestions == suggestions
    
    def test_clarification_has_suggestions(self, formatter):
        """Testa que clarification tem sugestões (= opções)."""
        options = ["opção 1", "opção 2"]
        response = formatter.format_clarification("Mensagem", options)
        
        assert response.suggestions == options


# =============================================================================
# TESTES DE METADADOS
# =============================================================================

class TestMetadata:
    """Testes para metadados nas respostas."""
    
    def test_text_with_metadata(self, formatter):
        """Testa texto com metadados."""
        metadata = {"intent": "teste", "confidence": 0.9}
        response = formatter.format_text("Teste", metadata=metadata)
        
        assert response.metadata == metadata
    
    def test_error_with_error_code(self, formatter):
        """Testa erro com código de erro."""
        response = formatter.format_error(
            "Erro", 
            error_code="TEST_ERROR"
        )
        
        assert response.metadata is not None
        assert response.metadata.get("error_code") == "TEST_ERROR"


# =============================================================================
# TESTES DE FORMATADORES ESPECÍFICOS DO DOMÍNIO
# =============================================================================

class TestDomainFormatters:
    """Testes para formatadores específicos do domínio de filmes."""
    
    def test_format_filme_list_with_filmes(self, formatter, sample_filmes):
        """Testa formatação de lista de filmes."""
        response = formatter.format_filme_list(sample_filmes, context="do ator Tom Hanks")
        
        assert response.type == ResponseType.LIST
        # Content deve ser dict com message e items
        assert "message" in response.content
        assert "items" in response.content
        assert "3" in response.content["message"]  # Número de filmes
    
    def test_format_filme_list_empty(self, formatter):
        """Testa formatação quando não há filmes."""
        response = formatter.format_filme_list([], context="do ator Desconhecido")
        
        assert response.type == ResponseType.TEXT
        assert "Não encontrei" in response.content
    
    def test_format_genero_list(self, formatter, sample_generos):
        """Testa formatação de lista de gêneros."""
        response = formatter.format_genero_list(sample_generos, filme_titulo="Inception")
        
        assert response.type == ResponseType.TEXT
        assert "Inception" in response.content
    
    def test_format_genero_list_single(self, formatter):
        """Testa formatação de gênero único."""
        response = formatter.format_genero_list(
            [{"nome": "DRAMA"}],
            filme_titulo="Test Movie"
        )
        
        assert response.type == ResponseType.TEXT
        assert "Drama" in response.content
    
    def test_format_contagem(self, formatter):
        """Testa formatação de contagem."""
        response = formatter.format_contagem(42, "Action", ano=2020)
        
        assert response.type == ResponseType.TEXT
        assert "42" in response.content
        assert "2020" in response.content
    
    def test_format_contagem_without_year(self, formatter):
        """Testa formatação de contagem sem ano."""
        response = formatter.format_contagem(10, "Drama")
        
        assert response.type == ResponseType.TEXT
        assert "10" in response.content
        assert "Drama" in response.content
    
    def test_format_filme_aleatorio(self, formatter):
        """Testa formatação de filme aleatório."""
        response = formatter.format_filme_aleatorio("Inception")
        
        assert response.type == ResponseType.TEXT
        assert "Inception" in response.content
        assert response.suggestions is not None


# =============================================================================
# TESTES DE FORMATADORES DE ERRO ESPECÍFICOS
# =============================================================================

class TestErrorFormatters:
    """Testes para formatadores de erro específicos."""
    
    def test_format_entity_not_found(self, formatter):
        """Testa formatação de entidade não encontrada."""
        response = formatter.format_entity_not_found(
            entity_type="ator",
            entity_value="Tom Hank"
        )
        
        assert response.type == ResponseType.ERROR
        assert "Tom Hank" in response.content
        assert response.metadata["error_code"] == "ENTITY_NOT_FOUND"
    
    def test_format_missing_entity(self, formatter):
        """Testa formatação de entidade faltante."""
        response = formatter.format_missing_entity("o nome do ator")
        
        assert response.type == ResponseType.ERROR
        assert "ator" in response.content
        assert response.metadata["error_code"] == "MISSING_ENTITY"
    
    def test_format_no_results(self, formatter):
        """Testa formatação de nenhum resultado."""
        response = formatter.format_no_results("para essa busca")
        
        assert response.type == ResponseType.TEXT
        assert "Não encontrei" in response.content


# =============================================================================
# TESTES DE HELP
# =============================================================================

class TestHelpFormatters:
    """Testes para formatadores de ajuda."""
    
    def test_format_default_help(self, formatter):
        """Testa ajuda padrão."""
        response = formatter.format_default_help()
        
        assert response.type == ResponseType.HELP
        assert isinstance(response.content, dict)
        assert "examples" in response.content
        
        # Deve ter várias categorias
        examples = response.content["examples"]
        assert len(examples) > 3
    
    def test_format_low_confidence_help(self, formatter):
        """Testa ajuda para baixa confiança."""
        response = formatter.format_low_confidence_help(
            original_text="xyz abc",
            suggestions=["filmes por tom hanks", "filmes de ação"]
        )
        
        assert response.type == ResponseType.HELP
        assert "xyz abc" in response.content["message"]
        assert response.suggestions is not None
