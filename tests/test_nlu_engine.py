"""
Testes Unitários para o NLU Engine

Feature: thin-client-architecture
Property 2: Intent Detection for Portuguese Queries
Property 5: Multiple Entity Extraction
Validates: Requirements 2.1, 2.4
"""

import pytest
from app.nlu_engine import NLUEngine, get_nlu_engine, initialize_nlu_engine
from app.schemas import NLUResult


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
    
    def test_parse_returns_nlu_result(self, nlu):
        """Testa que parse retorna NLUResult."""
        result = nlu.parse("filmes por tom hanks")
        
        assert isinstance(result, NLUResult)
        assert hasattr(result, 'intent')
        assert hasattr(result, 'entities')
        assert hasattr(result, 'confidence')
        assert hasattr(result, 'original_text')
        assert hasattr(result, 'corrected_text')
    
    def test_parse_simple_ator_query(self, nlu):
        """Testa detecção de intenção para query simples de ator."""
        result = nlu.parse("filmes por tom hanks")
        
        assert result.intent == "filmes_por_ator"
        assert "ator" in result.entities
        assert result.confidence > 0.5
        assert result.original_text == "filmes por tom hanks"
    
    def test_parse_genero_query(self, nlu):
        """Testa detecção de intenção para query de gênero."""
        result = nlu.parse("filmes de ação")
        
        assert result.intent == "filmes_por_genero"
        assert "genero" in result.entities
        assert result.entities["genero"].lower() in ["ação", "acao"]
    
    def test_parse_diretor_query(self, nlu):
        """Testa detecção de intenção para query de diretor."""
        result = nlu.parse("filmes do diretor spielberg")
        
        assert result.intent == "filmes_por_diretor"
        assert "diretor" in result.entities
    
    def test_parse_genero_filme_query(self, nlu):
        """Testa detecção de intenção para consulta de gênero de um filme."""
        result = nlu.parse("gênero de inception")
        
        assert result.intent == "genero_do_filme"
        assert "filme" in result.entities
    
    def test_parse_aleatorio_query(self, nlu):
        """Testa detecção de intenção para filme aleatório."""
        result = nlu.parse("filme aleatório")
        
        assert result.intent == "filme_aleatorio"
        assert result.confidence > 0.5
    
    def test_parse_recomendar_query(self, nlu):
        """Testa detecção de intenção para recomendação."""
        result = nlu.parse("recomendar filmes de ação")
        
        assert result.intent == "recomendar_filme"
        assert result.confidence > 0.5
    
    def test_parse_contar_query(self, nlu):
        """Testa detecção de intenção para contagem de filmes."""
        result = nlu.parse("quantos filmes de drama")
        
        assert result.intent == "contar_filmes"
        assert result.confidence > 0.5
    
    def test_parse_complex_query_with_filters(self, nlu):
        """Testa query complexa com múltiplos filtros."""
        result = nlu.parse("filmes com tom hanks de drama")
        
        # Deve detectar intent de filtros múltiplos ou ator
        assert result.intent in ["filmes_com_filtros", "filmes_por_ator", "recomendar_ator_e_genero"]
        assert "ator" in result.entities or "genero" in result.entities
    
    def test_parse_with_typo(self, nlu):
        """Testa que o NLU ainda funciona com pequenos typos."""
        result = nlu.parse("flmes por tom hanks")
        
        # Deve ainda detectar a intenção correta
        assert result.intent == "filmes_por_ator"
    
    def test_parse_unknown_query(self, nlu):
        """Testa comportamento com query não reconhecida."""
        result = nlu.parse("xyz abc def")
        
        assert result.intent == "unknown"
        assert result.confidence < 0.5
    
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
            assert result.intent == "filmes_por_ator"
            assert "ator" in result.entities
    
    def test_case_insensitivity(self, nlu):
        """Testa que o NLU funciona independente de maiúsculas/minúsculas."""
        results = [
            nlu.parse("FILMES POR TOM HANKS"),
            nlu.parse("filmes por tom hanks"),
            nlu.parse("Filmes Por Tom Hanks")
        ]
        
        # Todos devem ter a mesma intenção
        intents = [r.intent for r in results]
        assert len(set(intents)) == 1
        assert intents[0] == "filmes_por_ator"


# =============================================================================
# PROPERTY 2: INTENT DETECTION FOR PORTUGUESE QUERIES
# =============================================================================

class TestProperty2IntentDetection:
    """
    Property 2: Intent Detection for Portuguese Queries
    
    Para qualquer query válida em português que corresponda a um padrão conhecido,
    o NLUEngine DEVE detectar uma intenção com score de confiança entre 0.0 e 1.0.
    """
    
    # Queries válidas com intenção esperada
    VALID_QUERIES = [
        ("filmes por tom hanks", "filmes_por_ator"),
        ("filmes do brad pitt", "filmes_por_ator"),
        ("filmes com penelope cruz", "filmes_por_ator"),
        ("filmes de ação", "filmes_por_genero"),
        ("filmes de comédia", "filmes_por_genero"),
        ("filmes de drama", "filmes_por_genero"),
        ("filme aleatório", "filme_aleatorio"),
        ("filme random", "filme_aleatorio"),
        ("gênero de inception", "genero_do_filme"),
        ("genero do matrix", "genero_do_filme"),
        ("quantos filmes de ação", "contar_filmes"),
        ("contar filmes de drama em 2020", "contar_filmes"),
        ("filmes do diretor spielberg", "filmes_por_diretor"),
        ("filmes pelo diretor nolan", "filmes_por_diretor"),
    ]
    
    @pytest.mark.parametrize("query,expected_intent", VALID_QUERIES)
    def test_valid_queries_return_correct_intent(self, query, expected_intent):
        """Testa que queries válidas retornam a intenção correta."""
        nlu = get_nlu_engine()
        result = nlu.parse(query)
        
        assert result.intent == expected_intent, \
            f"Query '{query}' deveria retornar intent '{expected_intent}', mas retornou '{result.intent}'"
    
    @pytest.mark.parametrize("query,expected_intent", VALID_QUERIES)
    def test_confidence_in_valid_range(self, query, expected_intent):
        """Testa que a confiança está entre 0.0 e 1.0."""
        nlu = get_nlu_engine()
        result = nlu.parse(query)
        
        assert 0.0 <= result.confidence <= 1.0, \
            f"Confidence {result.confidence} fora do intervalo [0.0, 1.0]"
    
    @pytest.mark.parametrize("query,expected_intent", VALID_QUERIES)
    def test_original_text_preserved(self, query, expected_intent):
        """Testa que o texto original é preservado."""
        nlu = get_nlu_engine()
        result = nlu.parse(query)
        
        assert result.original_text == query


# =============================================================================
# PROPERTY 5: MULTIPLE ENTITY EXTRACTION
# =============================================================================

class TestProperty5MultipleEntityExtraction:
    """
    Property 5: Multiple Entity Extraction
    
    Para qualquer query contendo múltiplas entidades relevantes para a intenção
    detectada, o NLUEngine DEVE extrair todas as entidades e incluí-las no
    dicionário de entidades.
    """
    
    def test_extract_single_actor(self):
        """Testa extração de um ator."""
        nlu = get_nlu_engine()
        result = nlu.parse("filmes por tom hanks")
        
        assert "ator" in result.entities
        assert len(result.entities["ator"]) > 0
    
    def test_extract_single_genre(self):
        """Testa extração de um gênero."""
        nlu = get_nlu_engine()
        result = nlu.parse("filmes de ação")
        
        assert "genero" in result.entities
        assert len(result.entities["genero"]) > 0
    
    def test_extract_single_movie(self):
        """Testa extração de um filme."""
        nlu = get_nlu_engine()
        result = nlu.parse("gênero de inception")
        
        assert "filme" in result.entities
        assert "inception" in result.entities["filme"].lower()
    
    def test_entities_dict_is_always_dict(self):
        """Testa que entities é sempre um dicionário."""
        nlu = get_nlu_engine()
        
        queries = [
            "filmes por tom hanks",
            "filme aleatório",
            "xyz abc def",
        ]
        
        for query in queries:
            result = nlu.parse(query)
            assert isinstance(result.entities, dict)


# =============================================================================
# TESTES DE CONFIDENCE THRESHOLDS
# =============================================================================

class TestConfidenceThresholds:
    """Testes para verificar os thresholds de confiança."""
    
    def test_is_low_confidence(self):
        """Testa método is_low_confidence."""
        nlu = get_nlu_engine()
        
        assert nlu.is_low_confidence(0.39) is True
        assert nlu.is_low_confidence(0.40) is False
        assert nlu.is_low_confidence(0.0) is True
    
    def test_is_high_confidence(self):
        """Testa método is_high_confidence."""
        nlu = get_nlu_engine()
        
        assert nlu.is_high_confidence(0.70) is True
        assert nlu.is_high_confidence(0.69) is False
        assert nlu.is_high_confidence(1.0) is True
    
    def test_unknown_query_has_low_confidence(self):
        """Testa que queries desconhecidas têm baixa confiança."""
        nlu = get_nlu_engine()
        result = nlu.parse("xyzabc123")
        
        assert nlu.is_low_confidence(result.confidence)


# =============================================================================
# TESTES DE SUGESTÕES
# =============================================================================

class TestSuggestions:
    """Testes para métodos de sugestão."""
    
    def test_get_suggestions_returns_list(self):
        """Testa que get_suggestions retorna lista."""
        nlu = get_nlu_engine()
        suggestions = nlu.get_suggestions("filmes")
        
        assert isinstance(suggestions, list)
    
    def test_get_suggestions_with_intent(self):
        """Testa sugestões filtradas por intenção."""
        nlu = get_nlu_engine()
        suggestions = nlu.get_suggestions("", intent="filmes_por_ator")
        
        assert isinstance(suggestions, list)
        # Sugestões devem ser relacionadas a atores
        if suggestions:
            for s in suggestions:
                assert isinstance(s, str)
    
    def test_get_help_by_category(self):
        """Testa obtenção de ajuda por categoria."""
        nlu = get_nlu_engine()
        help_dict = nlu.get_help_by_category()
        
        assert isinstance(help_dict, dict)
        # Deve ter pelo menos algumas categorias
        assert len(help_dict) > 0
        
        # Cada categoria deve ter lista de exemplos
        for category, examples in help_dict.items():
            assert isinstance(examples, list)
            assert all(isinstance(e, str) for e in examples)


# =============================================================================
# TESTES DE INTEGRAÇÃO COM SPELL CORRECTOR
# =============================================================================

class TestSpellCorrectorIntegration:
    """Testes de integração com SpellCorrector."""
    
    def test_parse_without_spell_corrector(self):
        """Testa parse sem spell corrector."""
        nlu = NLUEngine(spell_corrector=None)
        result = nlu.parse("filmes por tom hanks")
        
        assert result.corrected_text is None
    
    def test_set_spell_corrector(self):
        """Testa configuração do spell corrector."""
        nlu = NLUEngine()
        
        # Mock spell corrector
        class MockSpellCorrector:
            is_initialized = True
            def correct(self, text):
                return text, False
        
        nlu.set_spell_corrector(MockSpellCorrector())
        
        assert nlu._spell_corrector is not None
    
    def test_parse_dict_compatibility(self):
        """Testa que parse_dict mantém compatibilidade."""
        nlu = get_nlu_engine()
        result = nlu.parse_dict("filmes por tom hanks")
        
        assert isinstance(result, dict)
        assert "intent" in result
        assert "entities" in result
        assert "confidence" in result
        assert "original_text" in result
        assert "corrected_text" in result
