"""
Unit Tests for Intent Data Augmentation

Tests data augmentation strategies.
"""

import pytest
from scripts.augment_intent_data import IntentDataAugmenter, add_manual_paraphrases


@pytest.fixture
def augmenter():
    """Create augmenter instance."""
    return IntentDataAugmenter()


class TestSynonymReplacement:
    """Tests for synonym replacement."""
    
    def test_replace_with_synonym(self, augmenter):
        """Test basic synonym replacement."""
        text = "filmes de ação"
        
        # Try multiple times (probabilistic)
        variations = set()
        for _ in range(20):
            result = augmenter.replace_with_synonym(text, "filmes", probability=1.0)
            variations.add(result)
        
        # Should have at least one variation
        assert len(variations) > 1
        assert "filmes de ação" in variations  # Original sometimes kept
    
    def test_augment_with_synonyms(self, augmenter):
        """Test generating synonym variations."""
        text = "filmes de ação"
        variations = augmenter.augment_with_synonyms(text, n_variations=5)
        
        assert isinstance(variations, list)
        assert len(variations) <= 5
        
        # Should be different from original
        for var in variations:
            assert var != text


class TestTemplateGeneration:
    """Tests for template-based generation."""
    
    def test_generate_from_templates_filmes_por_ator(self, augmenter):
        """Test template generation for filmes_por_ator."""
        examples = augmenter.generate_from_templates("filmes_por_ator", n_samples=10)
        
        assert isinstance(examples, list)
        assert len(examples) > 0
        assert len(examples) <= 10
        
        # Check that placeholders are filled
        for ex in examples:
            assert "{" not in ex
            assert "}" not in ex
    
    def test_generate_from_templates_filmes_por_genero(self, augmenter):
        """Test template generation for filmes_por_genero."""
        examples = augmenter.generate_from_templates("filmes_por_genero", n_samples=10)
        
        assert isinstance(examples, list)
        assert len(examples) > 0
        
        # Should contain genre-related terms
        has_genre = any("ação" in ex.lower() or "comédia" in ex.lower() 
                       for ex in examples)
        assert has_genre
    
    def test_generate_from_templates_unknown_intent(self, augmenter):
        """Test template generation for unknown intent."""
        examples = augmenter.generate_from_templates("unknown_intent", n_samples=10)
        
        assert examples == []


class TestExampleAugmentation:
    """Tests for full example augmentation."""
    
    def test_augment_examples_basic(self, augmenter):
        """Test basic example augmentation."""
        original = ["filmes de ação", "filmes de comédia"]
        augmented = augmenter.augment_examples(original, target_count=20)
        
        assert isinstance(augmented, list)
        assert len(augmented) >= len(original)
        assert len(augmented) <= 20
        
        # Original examples should be included
        assert "filmes de ação" in augmented
        assert "filmes de comédia" in augmented
    
    def test_augment_examples_no_duplicates(self, augmenter):
        """Test that augmentation doesn't create duplicates."""
        original = ["filmes de ação"]
        augmented = augmenter.augment_examples(original, target_count=10)
        
        # Check for duplicates
        assert len(augmented) == len(set([ex.lower() for ex in augmented]))
    
    def test_augment_examples_reaches_target(self, augmenter):
        """Test that augmentation reaches target count."""
        original = ["filmes de ação", "filmes de comédia"]
        target = 15
        augmented = augmenter.augment_examples(original, target_count=target)
        
        assert len(augmented) <= target


class TestIntentPatternAugmentation:
    """Tests for full intent pattern augmentation."""
    
    def test_augment_intent_patterns_basic(self, augmenter):
        """Test augmenting intent patterns."""
        patterns = {
            "filmes_por_ator": {
                "examples": ["filmes com tom hanks", "filmes do brad pitt"]
            },
            "filmes_por_genero": {
                "examples": ["filmes de ação", "filmes de drama"]
            }
        }
        
        result = augmenter.augment_intent_patterns(patterns)
        
        assert "filmes_por_ator" in result
        assert "filmes_por_genero" in result
        
        # Check structure
        assert "original" in result["filmes_por_ator"]
        assert "augmented" in result["filmes_por_ator"]
        
        # Check augmentation happened
        orig_count = len(result["filmes_por_ator"]["original"])
        aug_count = len(result["filmes_por_ator"]["augmented"])
        assert aug_count > orig_count
    
    def test_augment_intent_patterns_empty_examples(self, augmenter):
        """Test handling of empty examples."""
        patterns = {
            "test_intent": {
                "examples": []
            }
        }
        
        result = augmenter.augment_intent_patterns(patterns)
        
        # Should skip intent with no examples
        assert "test_intent" not in result
    
    def test_augment_intent_patterns_with_templates(self, augmenter):
        """Test that template generation is used."""
        patterns = {
            "filmes_por_ator": {
                "examples": ["filmes com tom hanks"]
            }
        }
        
        result = augmenter.augment_intent_patterns(patterns)
        
        # Should have more examples due to templates
        aug_examples = result["filmes_por_ator"]["augmented"]
        assert len(aug_examples) > 5


class TestManualParaphrases:
    """Tests for manual paraphrases."""
    
    def test_add_manual_paraphrases_structure(self):
        """Test structure of manual paraphrases."""
        paraphrases = add_manual_paraphrases()
        
        assert isinstance(paraphrases, dict)
        assert len(paraphrases) > 0
        
        # Check some expected intents
        assert "filmes_por_genero" in paraphrases
        assert "filme_aleatorio" in paraphrases
    
    def test_add_manual_paraphrases_content(self):
        """Test content of manual paraphrases."""
        paraphrases = add_manual_paraphrases()
        
        # Check for creative paraphrases
        genero_paraphrases = paraphrases["filmes_por_genero"]
        assert "quero rir" in genero_paraphrases
        assert "algo assustador" in genero_paraphrases
        
        # All should be non-empty strings
        for intent, phrases in paraphrases.items():
            assert isinstance(phrases, list)
            assert all(isinstance(p, str) and len(p) > 0 for p in phrases)


class TestIntegration:
    """Integration tests."""
    
    def test_full_augmentation_pipeline(self, augmenter):
        """Test full augmentation pipeline."""
        # Minimal intent patterns
        patterns = {
            "filmes_por_ator": {
                "examples": ["filmes com tom hanks", "filmes do brad pitt"]
            }
        }
        
        # Augment
        result = augmenter.augment_intent_patterns(patterns)
        
        # Add manual paraphrases
        manual = add_manual_paraphrases()
        for intent, paraphrases in manual.items():
            if intent in result:
                result[intent]["augmented"].extend(paraphrases)
        
        # Verify
        assert "filmes_por_ator" in result
        aug_examples = result["filmes_por_ator"]["augmented"]
        assert len(aug_examples) > 2
        
        # No duplicates
        lower_examples = [ex.lower() for ex in aug_examples]
        assert len(lower_examples) == len(set(lower_examples))


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
