"""
Unit Tests for Prolog Normalizer

Tests entity normalization for Prolog queries.
"""

import pytest
from app.prolog_normalizer import PrologNormalizer


class TestNormalizeToProlog Atom:
    """Tests for normalize_to_prolog_atom method."""
    
    def test_simple_name(self):
        """Test normalization of simple names."""
        assert PrologNormalizer.normalize_to_prolog_atom("Tom Hanks") == "tom_hanks"
        assert PrologNormalizer.normalize_to_prolog_atom("Brad Pitt") == "brad_pitt"
    
    def test_name_with_accents(self):
        """Test removal of accents."""
        assert PrologNormalizer.normalize_to_prolog_atom("Ação") == "acao"
        assert PrologNormalizer.normalize_to_prolog_atom("José García") == "jose_garcia"
        assert PrologNormalizer.normalize_to_prolog_atom("François Truffaut") == "francois_truffaut"
    
    def test_name_with_ampersand(self):
        """Test replacement of & symbol."""
        assert PrologNormalizer.normalize_to_prolog_atom("Ação & Aventura") == "acao_aventura"
        assert PrologNormalizer.normalize_to_prolog_atom("Rock & Roll") == "rock_roll"
    
    def test_name_with_special_characters(self):
        """Test removal of special characters."""
        assert PrologNormalizer.normalize_to_prolog_atom("O'Connor") == "oconnor"
        assert PrologNormalizer.normalize_to_prolog_atom("Jean-Claude") == "jeanclaude"
        assert PrologNormalizer.normalize_to_prolog_atom("Dr. Who") == "dr_who"
    
    def test_multiple_spaces(self):
        """Test handling of multiple spaces."""
        assert PrologNormalizer.normalize_to_prolog_atom("The    Rock") == "the_rock"
        assert PrologNormalizer.normalize_to_prolog_atom("  Leading Space") == "leading_space"
        assert PrologNormalizer.normalize_to_prolog_atom("Trailing Space  ") == "trailing_space"
    
    def test_empty_string(self):
        """Test handling of empty string."""
        assert PrologNormalizer.normalize_to_prolog_atom("") == ""
        assert PrologNormalizer.normalize_to_prolog_atom("   ") == ""
    
    def test_numbers_preserved(self):
        """Test that numbers are preserved."""
        assert PrologNormalizer.normalize_to_prolog_atom("Spider-Man 2") == "spiderman_2"
        assert PrologNormalizer.normalize_to_prolog_atom("2001") == "2001"


class TestNormalizeEntityForQuery:
    """Tests for normalize_entity_for_query method."""
    
    def test_actor_normalization(self):
        """Test actor name normalization."""
        assert PrologNormalizer.normalize_entity_for_query("tom hanks", "actor") == "TOM HANKS"
        assert PrologNormalizer.normalize_entity_for_query("Leonardo DiCaprio", "actor") == "LEONARDO DICAPRIO"
    
    def test_director_normalization(self):
        """Test director name normalization."""
        assert PrologNormalizer.normalize_entity_for_query("steven spielberg", "director") == "STEVEN SPIELBERG"
        assert PrologNormalizer.normalize_entity_for_query("Christopher Nolan", "director") == "CHRISTOPHER NOLAN"
    
    def test_film_normalization(self):
        """Test film title normalization."""
        assert PrologNormalizer.normalize_entity_for_query("the matrix", "film") == "THE MATRIX"
        assert PrologNormalizer.normalize_entity_for_query("Inception", "film") == "INCEPTION"
    
    def test_genre_normalization(self):
        """Test genre normalization."""
        assert PrologNormalizer.normalize_entity_for_query("ação", "genre") == "AÇÃO"
        assert PrologNormalizer.normalize_entity_for_query("comedy", "genre") == "COMEDY"
    
    def test_empty_entity(self):
        """Test handling of empty entity."""
        assert PrologNormalizer.normalize_entity_for_query("", "actor") == ""
        assert PrologNormalizer.normalize_entity_for_query("", "genre") == ""


class TestEscapePrologString:
    """Tests for escape_prolog_string method."""
    
    def test_escape_single_quote(self):
        """Test escaping of single quotes."""
        assert PrologNormalizer.escape_prolog_string("O'Connor") == "O\\'Connor"
        assert PrologNormalizer.escape_prolog_string("It's") == "It\\'s"
    
    def test_no_special_chars(self):
        """Test strings without special characters."""
        assert PrologNormalizer.escape_prolog_string("Tom Hanks") == "Tom Hanks"
        assert PrologNormalizer.escape_prolog_string("The Matrix") == "The Matrix"
    
    def test_empty_string(self):
        """Test empty string."""
        assert PrologNormalizer.escape_prolog_string("") == ""


class TestConvenienceMethods:
    """Tests for convenience normalization methods."""
    
    def test_normalize_actor_name(self):
        """Test normalize_actor_name."""
        assert PrologNormalizer.normalize_actor_name("tom hanks") == "TOM HANKS"
        assert PrologNormalizer.normalize_actor_name("") == ""
    
    def test_normalize_director_name(self):
        """Test normalize_director_name."""
        assert PrologNormalizer.normalize_director_name("spielberg") == "SPIELBERG"
        assert PrologNormalizer.normalize_director_name("") == ""
    
    def test_normalize_film_title(self):
        """Test normalize_film_title."""
        assert PrologNormalizer.normalize_film_title("matrix") == "MATRIX"
        assert PrologNormalizer.normalize_film_title("") == ""
    
    def test_normalize_genre_name(self):
        """Test normalize_genre_name."""
        assert PrologNormalizer.normalize_genre_name("action") == "ACTION"
        assert PrologNormalizer.normalize_genre_name("") == ""
    
    def test_normalize_entity(self):
        """Test generic normalize_entity."""
        assert PrologNormalizer.normalize_entity("tom hanks", "actor") == "TOM HANKS"
        assert PrologNormalizer.normalize_entity("inception", "film") == "INCEPTION"
        assert PrologNormalizer.normalize_entity("ação", "genre") == "AÇÃO"


class TestIntegrationScenarios:
    """Integration tests for real-world scenarios."""
    
    def test_actor_pipeline(self):
        """Test full pipeline for actor query."""
        # User query: "o cara do lobo de wall street"
        # Semantic match: "Leonardo DiCaprio"
        matched = "Leonardo DiCaprio"
        
        # Normalize for query
        normalized = PrologNormalizer.normalize_entity_for_query(matched, "actor")
        assert normalized == "LEONARDO DICAPRIO"
        
        # Escape for Prolog
        escaped = PrologNormalizer.escape_prolog_string(normalized)
        assert escaped == "LEONARDO DICAPRIO"
    
    def test_genre_with_ampersand(self):
        """Test genre with ampersand."""
        # Semantic match: "Action & Adventure"
        matched = "Action & Adventure"
        
        # Normalize for query
        normalized = PrologNormalizer.normalize_entity_for_query(matched, "genre")
        assert normalized == "ACTION & ADVENTURE"
    
    def test_film_with_apostrophe(self):
        """Test film title with apostrophe."""
        # Semantic match: "Ocean's Eleven"
        matched = "Ocean's Eleven"
        
        # Normalize for query
        normalized = PrologNormalizer.normalize_entity_for_query(matched, "film")
        assert normalized == "OCEAN'S ELEVEN"
        
        # Escape for Prolog
        escaped = PrologNormalizer.escape_prolog_string(normalized)
        assert escaped == "OCEAN\\'S ELEVEN"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
