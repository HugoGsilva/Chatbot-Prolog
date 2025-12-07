"""
Unit Tests for Semantic Intent Classifier

Tests semantic intent classification functionality.
"""

import pytest
from app.semantic_classifier import SemanticIntentClassifier


@pytest.fixture
def mock_intent_patterns():
    """Mock intent patterns for testing."""
    return {
        "filmes_por_ator": {
            "keywords": ["filmes", "com"],
            "examples": [
                "filmes com tom hanks",
                "filmes do brad pitt",
                "obras de adam sandler"
            ]
        },
        "filmes_por_genero": {
            "keywords": ["filmes", "de"],
            "examples": [
                "filmes de ação",
                "filmes de comédia",
                "filmes do gênero drama"
            ]
        },
        "ajuda": {
            "keywords": ["ajuda", "help"],
            "examples": [
                "ajuda",
                "help",
                "como funciona"
            ]
        }
    }


@pytest.fixture
def classifier(mock_intent_patterns):
    """Initialize classifier with mock patterns."""
    clf = SemanticIntentClassifier()
    clf.load_intent_patterns(mock_intent_patterns)
    return clf


def test_classifier_initialization():
    """Test classifier can be initialized."""
    clf = SemanticIntentClassifier()
    assert clf is not None
    assert not clf.is_loaded()


def test_load_intent_patterns(classifier):
    """Test loading intent patterns."""
    assert classifier.is_loaded()
    assert len(classifier.get_intents()) == 3
    assert "filmes_por_ator" in classifier.get_intents()


def test_classify_exact_match(classifier):
    """Test classification with exact match to example."""
    results = classifier.classify("filmes de ação")
    
    assert len(results) > 0
    best_intent, confidence = results[0]
    
    # Should match filmes_por_genero with high confidence
    assert best_intent == "filmes_por_genero"
    assert confidence > 0.9  # Very high similarity for exact match


def test_classify_paraphrase(classifier):
    """Test classification with paraphrased query."""
    results = classifier.classify("obras do tom hanks")
    
    assert len(results) > 0
    best_intent, confidence = results[0]
    
    # Should match filmes_por_ator (semantic similarity)
    assert best_intent == "filmes_por_ator"
    assert confidence > 0.6  # Good similarity for paraphrase


def test_classify_top_k(classifier):
    """Test returning top K results."""
    results = classifier.classify("filmes", top_k=3)
    
    assert len(results) == 3
    
    # Check scores are descending
    scores = [score for _, score in results]
    assert scores == sorted(scores, reverse=True)


def test_classify_with_details(classifier):
    """Test detailed classification."""
    results = classifier.classify_with_details("filmes de comédia", top_k=2)
    
    assert len(results) == 2
    assert "intent" in results[0]
    assert "confidence" in results[0]
    assert "best_example" in results[0]
    
    # Best match should be filmes_por_genero
    assert results[0]["intent"] == "filmes_por_genero"


def test_classify_empty_query(classifier):
    """Test classification with empty query."""
    results = classifier.classify("")
    
    assert len(results) == 1
    assert results[0][0] == "unknown"
    assert results[0][1] == 0.0


def test_classify_unknown_intent(classifier):
    """Test classification with completely unrelated query."""
    results = classifier.classify("how to bake a cake")
    
    assert len(results) > 0
    # Should return something, but with lower confidence
    _, confidence = results[0]
    assert confidence < 0.7


def test_get_example_count(classifier):
    """Test getting example count for intents."""
    count = classifier.get_example_count("filmes_por_ator")
    assert count == 3
    
    count_missing = classifier.get_example_count("nonexistent_intent")
    assert count_missing == 0


def test_classifier_not_loaded_error():
    """Test error when classifying before loading patterns."""
    clf = SemanticIntentClassifier()
    
    with pytest.raises(RuntimeError, match="Classifier not loaded"):
        clf.classify("test query")


def test_multilingual_support(classifier):
    """Test that model handles Portuguese queries."""
    # Portuguese query
    results = classifier.classify("filmes com atores famosos")
    assert len(results) > 0
    
    # Should still work (multilingual model)
    best_intent, confidence = results[0]
    assert confidence > 0.3  # Some similarity expected


def test_singleton_instance():
    """Test singleton pattern for classifier."""
    from app.semantic_classifier import get_semantic_classifier, initialize_semantic_classifier
    
    # Initially None
    assert get_semantic_classifier() is None
    
    # Initialize
    clf1 = initialize_semantic_classifier()
    assert clf1 is not None
    
    # Get same instance
    clf2 = get_semantic_classifier()
    assert clf1 is clf2


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
