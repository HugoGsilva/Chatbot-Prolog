"""
Testes para SemanticEntityExtractor (Step 4).

Testa:
1. Extração de candidatos com spaCy (NER, noun chunks, preposições)
2. Matching semântico com embeddings
3. Fallback para fuzzy matching
4. Matching híbrido (semantic + fuzzy)
5. Casos edge (queries curtas, nomes compostos, typos)
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from app.semantic_entity_extractor import SemanticEntityExtractor, get_semantic_entity_extractor


@pytest.fixture
def mock_nlp():
    """Mock spaCy NLP."""
    nlp = MagicMock()
    
    # Mock doc com entities e noun chunks
    def create_mock_doc(text):
        mock_doc = MagicMock()
        
        # Mock entities (NER)
        mock_ent = MagicMock()
        mock_ent.text = "Leonardo DiCaprio"
        mock_ent.label_ = "PER"
        mock_doc.ents = [mock_ent] if "leonardo" in text.lower() else []
        
        # Mock noun chunks
        mock_chunk = MagicMock()
        mock_chunk.text = "Leonardo DiCaprio"
        mock_doc.noun_chunks = [mock_chunk] if "leonardo" in text.lower() else []
        
        return mock_doc
    
    nlp.side_effect = create_mock_doc
    return nlp


@pytest.fixture
def extractor_no_semantic(mock_nlp):
    """Extractor sem semantic classifier (só fuzzy)."""
    return SemanticEntityExtractor(nlp=mock_nlp, semantic_classifier=None, threshold=0.70)


@pytest.fixture
def mock_semantic_classifier():
    """Mock do semantic classifier."""
    classifier = MagicMock()
    
    # Mock modelo com encode
    mock_model = MagicMock()
    mock_model.encode.return_value = MagicMock()  # Retorna tensor mock
    classifier.model = mock_model
    
    return classifier


@pytest.fixture
def extractor_with_semantic(mock_nlp, mock_semantic_classifier):
    """Extractor com semantic classifier."""
    return SemanticEntityExtractor(
        nlp=mock_nlp,
        semantic_classifier=mock_semantic_classifier,
        threshold=0.70
    )


class TestEntityCandidateExtraction:
    """Testes de extração de candidatos."""
    
    def test_extract_named_entity(self, extractor_no_semantic):
        """Extrai named entity do spaCy."""
        candidates = extractor_no_semantic._extract_entity_candidates(
            "filmes do Leonardo DiCaprio"
        )
        
        assert len(candidates) > 0
        assert any("Leonardo" in c for c in candidates)
    
    def test_extract_after_preposition(self, extractor_no_semantic, mock_nlp):
        """Extrai texto após preposição."""
        # Mock doc sem entities
        mock_doc = MagicMock()
        mock_doc.ents = []
        mock_doc.noun_chunks = []
        mock_nlp.return_value = mock_doc
        
        candidates = extractor_no_semantic._extract_entity_candidates(
            "filmes de ação"
        )
        
        assert len(candidates) > 0
        assert any("ação" in c.lower() for c in candidates)
    
    def test_extract_after_multiple_preps(self, extractor_no_semantic, mock_nlp):
        """Testa extração com múltiplas preposições."""
        mock_doc = MagicMock()
        mock_doc.ents = []
        mock_doc.noun_chunks = []
        mock_nlp.return_value = mock_doc
        
        candidates = extractor_no_semantic._extract_entity_candidates(
            "filmes por Christopher Nolan"
        )
        
        assert len(candidates) > 0
        assert any("Christopher" in c or "Nolan" in c for c in candidates)
    
    def test_extract_removes_stopwords(self, extractor_no_semantic, mock_nlp):
        """Remove stopwords do final do candidato."""
        mock_doc = MagicMock()
        mock_doc.ents = []
        mock_doc.noun_chunks = []
        mock_nlp.return_value = mock_doc
        
        candidates = extractor_no_semantic._extract_entity_candidates(
            "filmes de ação e aventura"
        )
        
        # Deve remover " e aventura"
        assert len(candidates) > 0
        # Verifica que não tem " e " no candidato
        assert all(" e " not in c.lower() for c in candidates)
    
    def test_extract_full_text_fallback(self, extractor_no_semantic, mock_nlp):
        """Se não acha nada, usa texto completo."""
        mock_doc = MagicMock()
        mock_doc.ents = []
        mock_doc.noun_chunks = []
        mock_nlp.return_value = mock_doc
        
        candidates = extractor_no_semantic._extract_entity_candidates(
            "ação"  # Query muito curta
        )
        
        assert len(candidates) > 0
        assert "ação" in candidates[0].lower()


class TestFuzzyMatching:
    """Testes de fuzzy matching."""
    
    def test_fuzzy_exact_match(self, extractor_no_semantic):
        """Match exato deve retornar score alto."""
        known_entities = ["LEONARDO DICAPRIO", "BRAD PITT", "TOM HANKS"]
        
        result = extractor_no_semantic.find_best_match_fuzzy(
            "Leonardo DiCaprio",
            known_entities,
            threshold=80
        )
        
        assert result is not None
        entity, confidence = result
        assert "LEONARDO DICAPRIO" == entity
        assert confidence > 0.90
    
    def test_fuzzy_with_typo(self, extractor_no_semantic):
        """Fuzzy deve tolerar typos."""
        known_entities = ["LEONARDO DICAPRIO", "BRAD PITT"]
        
        result = extractor_no_semantic.find_best_match_fuzzy(
            "Leanardo DiCaprio",  # Typo: "Leanardo"
            known_entities,
            threshold=75
        )
        
        assert result is not None
        entity, confidence = result
        assert "LEONARDO DICAPRIO" == entity
    
    def test_fuzzy_substring_bonus(self, extractor_no_semantic):
        """Substring match deve ter bonus."""
        known_entities = ["ACTION & ADVENTURE", "COMEDIES", "DRAMAS"]
        
        result = extractor_no_semantic.find_best_match_fuzzy(
            "action",
            known_entities,
            threshold=70
        )
        
        assert result is not None
        entity, confidence = result
        assert "ACTION" in entity
    
    def test_fuzzy_below_threshold(self, extractor_no_semantic):
        """Score baixo não deve retornar match."""
        known_entities = ["LEONARDO DICAPRIO", "BRAD PITT"]
        
        result = extractor_no_semantic.find_best_match_fuzzy(
            "totally different",
            known_entities,
            threshold=80
        )
        
        assert result is None


class TestSemanticMatching:
    """Testes de semantic matching."""
    
    @patch('app.semantic_entity_extractor.util')
    def test_semantic_high_similarity(self, mock_util, extractor_with_semantic):
        """Semantic match com alta similaridade."""
        # Mock cosine similarity retornando score alto
        mock_util.cos_sim.return_value = [[0.92, 0.45, 0.30]]
        
        known_entities = ["LEONARDO DICAPRIO", "BRAD PITT", "TOM HANKS"]
        
        result = extractor_with_semantic.find_best_match_semantic(
            "o cara do Titanic",
            known_entities,
            top_k=3
        )
        
        assert result is not None
        entity, confidence = result
        assert entity == "LEONARDO DICAPRIO"
        assert confidence > 0.70
    
    @patch('app.semantic_entity_extractor.util')
    def test_semantic_below_threshold(self, mock_util, extractor_with_semantic):
        """Score abaixo de threshold não retorna match."""
        mock_util.cos_sim.return_value = [[0.55, 0.40, 0.35]]
        
        known_entities = ["LEONARDO DICAPRIO", "BRAD PITT", "TOM HANKS"]
        
        result = extractor_with_semantic.find_best_match_semantic(
            "random text",
            known_entities,
            top_k=3
        )
        
        assert result is None
    
    def test_semantic_without_classifier(self, extractor_no_semantic):
        """Sem classifier, semantic retorna None."""
        result = extractor_no_semantic.find_best_match_semantic(
            "Leonardo DiCaprio",
            ["LEONARDO DICAPRIO"],
            top_k=1
        )
        
        assert result is None


class TestHybridMatching:
    """Testes de matching híbrido."""
    
    @patch('app.semantic_entity_extractor.util')
    def test_hybrid_semantic_success(self, mock_util, extractor_with_semantic):
        """Híbrido usa semantic quando disponível."""
        mock_util.cos_sim.return_value = [[0.88, 0.45]]
        
        known_entities = ["LEONARDO DICAPRIO", "BRAD PITT"]
        
        result = extractor_with_semantic.find_best_match_hybrid(
            "o cara do lobo de wall street",
            known_entities,
            semantic_threshold=0.70,
            fuzzy_threshold=80
        )
        
        assert result is not None
        entity, confidence, method = result
        assert method == 'semantic'
        assert entity == "LEONARDO DICAPRIO"
    
    @patch('app.semantic_entity_extractor.util')
    def test_hybrid_fallback_to_fuzzy(self, mock_util, extractor_with_semantic):
        """Híbrido faz fallback para fuzzy se semantic falhar."""
        # Semantic retorna score baixo
        mock_util.cos_sim.return_value = [[0.55, 0.40]]
        
        known_entities = ["LEONARDO DICAPRIO", "BRAD PITT"]
        
        result = extractor_with_semantic.find_best_match_hybrid(
            "Leonardo DiCaprio",  # Nome exato
            known_entities,
            semantic_threshold=0.70,
            fuzzy_threshold=80
        )
        
        assert result is not None
        entity, confidence, method = result
        assert method == 'fuzzy'
        assert "LEONARDO DICAPRIO" == entity
    
    def test_hybrid_no_match(self, extractor_no_semantic):
        """Híbrido retorna None se nenhum método encontrar match."""
        known_entities = ["LEONARDO DICAPRIO", "BRAD PITT"]
        
        result = extractor_no_semantic.find_best_match_hybrid(
            "completely different",
            known_entities,
            semantic_threshold=0.70,
            fuzzy_threshold=80
        )
        
        assert result is None


class TestEdgeCases:
    """Testes de casos edge."""
    
    def test_empty_query(self, extractor_no_semantic):
        """Query vazia não deve crashar."""
        candidates = extractor_no_semantic._extract_entity_candidates("")
        assert len(candidates) == 0
    
    def test_empty_cache(self, extractor_no_semantic):
        """Cache vazia não deve crashar."""
        result = extractor_no_semantic.find_best_match_fuzzy("test", [], threshold=80)
        assert result is None
    
    def test_very_short_query(self, extractor_no_semantic, mock_nlp):
        """Query de 1 palavra deve funcionar."""
        mock_doc = MagicMock()
        mock_doc.ents = []
        mock_doc.noun_chunks = []
        mock_nlp.return_value = mock_doc
        
        candidates = extractor_no_semantic._extract_entity_candidates("ação")
        assert len(candidates) > 0
    
    def test_query_with_punctuation(self, extractor_no_semantic, mock_nlp):
        """Remove pontuação corretamente."""
        mock_doc = MagicMock()
        mock_doc.ents = []
        mock_doc.noun_chunks = []
        mock_nlp.return_value = mock_doc
        
        candidates = extractor_no_semantic._extract_entity_candidates(
            "filmes de ação?"
        )
        
        assert len(candidates) > 0
        # Não deve ter ? no candidato
        assert all("?" not in c for c in candidates)
    
    def test_composite_name(self, extractor_no_semantic):
        """Nomes compostos devem ser mantidos juntos."""
        known_entities = ["LEONARDO DICAPRIO", "LEONARDO DA VINCI"]
        
        result = extractor_no_semantic.find_best_match_fuzzy(
            "Leonardo DiCaprio",
            known_entities,
            threshold=80
        )
        
        assert result is not None
        entity, confidence = result
        # Deve pegar o nome completo correto
        assert entity == "LEONARDO DICAPRIO"


class TestFactory:
    """Testes da função factory."""
    
    def test_get_semantic_entity_extractor(self, mock_nlp):
        """Factory cria extractor corretamente."""
        extractor = get_semantic_entity_extractor(
            nlp=mock_nlp,
            threshold=0.75
        )
        
        assert extractor is not None
        assert extractor.threshold == 0.75
        assert extractor.nlp == mock_nlp


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
