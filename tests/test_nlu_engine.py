"""
Testes Unitários para o NLU Engine
"""

import pytest
from app.nlu_engine import NLUEngine, get_nlu_engine


@pytest.fixture
def nlu():
    """Fixture que retorna uma instância do NLU Engine."""
    return get_nlu_engine()


class TestNLUEngine:
    """Testes para a classe NLUEngine."""
    
    def test_singleton_pattern(self):
        """Testa que get_nlu_engine() retorna sempre a mesma instância."""
        nlu1 = get_nlu_engine()
        nlu2 = get_nlu_engine()
        assert nlu1 is nlu2
    
    def test_parse_simple_ator_query(self, nlu):
        """Testa detecção de intenção para query simples de ator."""
        result = nlu.parse("filmes por tom hanks")
        
        assert result["intent"] == "filmes_por_ator"
        assert "ator" in result["entities"]
        assert result["confidence"] > 0.5
        assert result["original_text"] == "filmes por tom hanks"
    
    def test_parse_genero_query(self, nlu):
        """Testa detecção de intenção para query de gênero."""
        result = nlu.parse("filmes de ação")
        
        assert result["intent"] == "filmes_por_genero"
        assert "genero" in result["entities"]
        assert result["entities"]["genero"].lower() in ["ação", "acao"]
    
    def test_parse_diretor_query(self, nlu):
        """Testa detecção de intenção para query de diretor."""
        result = nlu.parse("filmes por spielberg")
        
        assert result["intent"] == "filmes_por_diretor"
        assert "diretor" in result["entities"]
    
    def test_parse_genero_filme_query(self, nlu):
        """Testa detecção de intenção para consulta de gênero de um filme."""
        result = nlu.parse("gênero de inception")
        
        assert result["intent"] == "genero_do_filme"
        assert "filme" in result["entities"]
    
    def test_parse_aleatorio_query(self, nlu):
        """Testa detecção de intenção para filme aleatório."""
        result = nlu.parse("filme aleatório")
        
        assert result["intent"] == "filme_aleatorio"
        assert result["confidence"] > 0.5
    
    def test_parse_recomendar_query(self, nlu):
        """Testa detecção de intenção para recomendação."""
        result = nlu.parse("recomendar filmes de ação")
        
        assert result["intent"] == "recomendar_filme"
        assert result["confidence"] > 0.5
    
    def test_parse_contar_query(self, nlu):
        """Testa detecção de intenção para contagem de filmes."""
        result = nlu.parse("quantos filmes de drama")
        
        assert result["intent"] == "contar_filmes"
        assert result["confidence"] > 0.5
    
    def test_parse_complex_query_with_filters(self, nlu):
        """Testa query complexa com múltiplos filtros."""
        result = nlu.parse("filmes com tom hanks de drama")
        
        # Deve detectar intent de filtros múltiplos ou ator
        assert result["intent"] in ["filmes_com_filtros", "filmes_por_ator"]
        assert "ator" in result["entities"] or "genero" in result["entities"]
    
    def test_parse_with_typo(self, nlu):
        """Testa que o NLU ainda funciona com pequenos typos."""
        result = nlu.parse("flmes por tom hanks")
        
        # Deve ainda detectar a intenção correta
        assert result["intent"] == "filmes_por_ator"
    
    def test_parse_unknown_query(self, nlu):
        """Testa comportamento com query não reconhecida."""
        result = nlu.parse("xyz abc def")
        
        assert result["intent"] == "unknown"
        assert result["confidence"] < 0.5
    
    def test_get_similar_queries(self, nlu):
        """Testa sugestão de queries similares."""
        suggestions = nlu.get_similar_queries("filmes por", top_n=3)
        
        assert isinstance(suggestions, list)
        assert len(suggestions) <= 3
        assert all(isinstance(s, str) for s in suggestions)
    
    def test_entity_extraction_variations(self, nlu):
        """Testa extração de entidades com diferentes preposições."""
        queries = [
            "filmes do brad pitt",
            "filmes com brad pitt",
            "filmes por brad pitt"
        ]
        
        for query in queries:
            result = nlu.parse(query)
            assert result["intent"] == "filmes_por_ator"
            assert "ator" in result["entities"]
    
    def test_case_insensitivity(self, nlu):
        """Testa que o NLU funciona independente de maiúsculas/minúsculas."""
        results = [
            nlu.parse("FILMES POR TOM HANKS"),
            nlu.parse("filmes por tom hanks"),
            nlu.parse("Filmes Por Tom Hanks")
        ]
        
        # Todos devem ter a mesma intenção
        intents = [r["intent"] for r in results]
        assert len(set(intents)) == 1
        assert intents[0] == "filmes_por_ator"
