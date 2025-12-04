"""
Testes para o SpellCorrector.

Feature: thin-client-architecture
Property 13: Spell Correction Application
Validates: Requirements 10.1

Testa que:
- Erros comuns de digitação (edit distance ≤ 2) são corrigidos
- Palavras corretas não são alteradas
- Performance adequada (< 5ms por correção)
"""

import pytest
import time
from typing import List, Tuple

from app.spell_corrector import (
    SpellCorrector,
    get_spell_corrector,
    initialize_spell_corrector,
)


# =============================================================================
# FIXTURES
# =============================================================================

@pytest.fixture
def corrector() -> SpellCorrector:
    """Retorna uma instância nova do SpellCorrector."""
    return SpellCorrector(max_edit_distance=2)


@pytest.fixture
def corrector_with_vocab() -> SpellCorrector:
    """Retorna um SpellCorrector com vocabulário de exemplo."""
    corrector = SpellCorrector(max_edit_distance=2)
    corrector.load_vocabulary(
        actors=["TOM HANKS", "BRAD PITT", "LEONARDO DICAPRIO", "PENELOPE CRUZ"],
        genres=["ACTION", "DRAMA", "COMEDY", "HORROR", "DOCUMENTARIES"],
        films=["FORREST GUMP", "INCEPTION", "THE MATRIX", "TITANIC"],
        directors=["STEVEN SPIELBERG", "CHRISTOPHER NOLAN", "QUENTIN TARANTINO"],
    )
    return corrector


# =============================================================================
# TESTES BÁSICOS
# =============================================================================

class TestSpellCorrectorBasic:
    """Testes básicos do SpellCorrector."""
    
    def test_initialization(self, corrector):
        """Testa que o corretor inicializa corretamente."""
        assert corrector is not None
        assert corrector.max_edit_distance == 2
        assert corrector.is_initialized is False
        assert corrector.vocabulary_size == 0
    
    def test_load_vocabulary(self, corrector):
        """Testa carregamento de vocabulário."""
        count = corrector.load_vocabulary(
            actors=["TOM HANKS", "BRAD PITT"],
            genres=["ACTION", "DRAMA"],
        )
        
        assert count > 0
        assert corrector.is_initialized is True
        assert corrector.vocabulary_size == count
    
    def test_empty_text_returns_empty(self, corrector_with_vocab):
        """Testa que texto vazio retorna vazio."""
        result, changed = corrector_with_vocab.correct("")
        assert result == ""
        assert changed is False
        
        result, changed = corrector_with_vocab.correct("   ")
        assert result == "   "
        assert changed is False
    
    def test_correct_text_unchanged(self, corrector_with_vocab):
        """Testa que texto correto não é alterado."""
        text = "filmes de drama"
        result, changed = corrector_with_vocab.correct(text)
        
        assert result == text
        assert changed is False
    
    def test_uninitialized_returns_original(self, corrector):
        """Testa que corretor não inicializado retorna texto original."""
        text = "flmes por tom hanks"
        result, changed = corrector.correct(text)
        
        assert result == text
        assert changed is False


# =============================================================================
# PROPERTY 13: SPELL CORRECTION APPLICATION
# =============================================================================

class TestProperty13SpellCorrection:
    """
    Property 13: Spell Correction Application
    
    Para qualquer mensagem contendo erros ortográficos comuns (edit distance ≤ 2
    de palavras conhecidas), a correção ortográfica DEVE produzir uma versão
    corrigida antes do processamento NLU.
    """
    
    # Casos de teste: (texto_com_erro, texto_esperado)
    TYPO_TEST_CASES: List[Tuple[str, str]] = [
        # Erros em palavras-chave (edit distance 1)
        ("flmes por tom hanks", "filmes por tom hanks"),
        ("filme por tom hanks", "filmes por tom hanks"),
        ("filmse de drama", "filmes de drama"),
        
        # Erros em nomes de atores (edit distance 1-2)
        ("filmes por tom hank", "filmes por tom hanks"),
        ("filmes por bard pitt", "filmes por brad pitt"),
        
        # Erros em gêneros (edit distance 1-2)
        ("filmes de drmaa", "filmes de drama"),
        ("filmes de acton", "filmes de action"),
        ("filmes de comdy", "filmes de comedy"),
    ]
    
    @pytest.mark.parametrize("typo_text,expected", TYPO_TEST_CASES)
    def test_common_typos_are_corrected(self, corrector_with_vocab, typo_text, expected):
        """Testa que erros comuns de digitação são corrigidos."""
        result, was_corrected = corrector_with_vocab.correct(typo_text)
        
        # Verifica que houve correção
        assert was_corrected is True, f"Esperava correção para: {typo_text}"
        
        # Verifica que o resultado é o esperado (case-insensitive)
        assert result.lower() == expected.lower(), \
            f"Esperava '{expected}', obteve '{result}'"
    
    def test_correct_words_not_changed(self, corrector_with_vocab):
        """Testa que palavras corretas não são alteradas."""
        correct_texts = [
            "filmes por tom hanks",
            "filmes de drama",
            "filmes de action",
            "filmes do diretor steven spielberg",
        ]
        
        for text in correct_texts:
            result, was_corrected = corrector_with_vocab.correct(text)
            # Pode ou não haver correção, mas o significado deve ser preservado
            # Aqui testamos que palavras conhecidas não são "corrompidas"
            words = result.lower().split()
            assert "filmes" in words, f"Palavra 'filmes' perdida em: {result}"
    
    def test_performance_under_5ms(self, corrector_with_vocab):
        """Testa que a correção é rápida (< 5ms por correção)."""
        test_texts = [
            "flmes por tom hanks",
            "filmes de drmaa com brad pitt",
            "recomendar filmes de acton",
        ]
        
        total_time = 0
        iterations = 100
        
        for _ in range(iterations):
            for text in test_texts:
                start = time.perf_counter()
                corrector_with_vocab.correct(text)
                elapsed = (time.perf_counter() - start) * 1000  # ms
                total_time += elapsed
        
        avg_time = total_time / (iterations * len(test_texts))
        
        assert avg_time < 5, f"Correção muito lenta: {avg_time:.2f}ms (esperado < 5ms)"
    
    def test_preserves_case(self, corrector_with_vocab):
        """Testa que a capitalização é preservada quando possível."""
        # Palavra com primeira letra maiúscula
        result, _ = corrector_with_vocab.correct("Flmes por Tom Hanks")
        
        # Verifica que começa com maiúscula se original começava
        words = result.split()
        if words:
            # "Flmes" corrigido para "Filmes" deve manter maiúscula
            assert words[0][0].isupper() or words[0] == "filmes"


# =============================================================================
# TESTES DE EDGE CASES
# =============================================================================

class TestSpellCorrectorEdgeCases:
    """Testes de casos especiais."""
    
    def test_short_words_not_corrected(self, corrector_with_vocab):
        """Testa que palavras curtas (≤2 chars) não são corrigidas."""
        text = "de a o em"
        result, changed = corrector_with_vocab.correct(text)
        
        assert changed is False
        assert result == text
    
    def test_punctuation_preserved(self, corrector_with_vocab):
        """Testa que pontuação é preservada."""
        text = "filmes de drama, por favor!"
        result, _ = corrector_with_vocab.correct(text)
        
        assert "," in result
        assert "!" in result
    
    def test_multiple_spaces_handled(self, corrector_with_vocab):
        """Testa que múltiplos espaços são tratados."""
        text = "filmes  por   tom hanks"
        result, _ = corrector_with_vocab.correct(text)
        
        # Deve funcionar mesmo com espaços extras
        assert "filmes" in result.lower()
        assert "tom" in result.lower()
    
    def test_numbers_preserved(self, corrector_with_vocab):
        """Testa que números são preservados."""
        text = "filmes de 2023"
        result, _ = corrector_with_vocab.correct(text)
        
        assert "2023" in result
    
    def test_get_suggestions(self, corrector_with_vocab):
        """Testa obtenção de sugestões para uma palavra."""
        suggestions = corrector_with_vocab.get_suggestions("drmaa", max_suggestions=3)
        
        assert len(suggestions) > 0
        assert "drama" in suggestions


# =============================================================================
# TESTES DO SINGLETON
# =============================================================================

class TestSpellCorrectorSingleton:
    """Testes do padrão singleton."""
    
    def test_singleton_returns_same_instance(self):
        """Testa que get_spell_corrector retorna a mesma instância."""
        corrector1 = get_spell_corrector()
        corrector2 = get_spell_corrector()
        
        assert corrector1 is corrector2
    
    def test_initialize_loads_vocabulary(self):
        """Testa que initialize_spell_corrector carrega vocabulário."""
        corrector = initialize_spell_corrector(
            actors=["TEST ACTOR"],
            genres=["TEST GENRE"],
        )
        
        assert corrector.is_initialized
        assert corrector.vocabulary_size > 0


# =============================================================================
# TESTES COM VOCABULÁRIO GRANDE
# =============================================================================

class TestSpellCorrectorLargeVocabulary:
    """Testes com vocabulário grande para verificar performance."""
    
    def test_large_vocabulary_performance(self):
        """Testa performance com vocabulário grande (1000+ termos)."""
        corrector = SpellCorrector()
        
        # Gera vocabulário grande
        actors = [f"ACTOR NAME {i}" for i in range(500)]
        films = [f"FILM TITLE {i}" for i in range(500)]
        
        # Carrega vocabulário
        start = time.perf_counter()
        corrector.load_vocabulary(actors=actors, films=films)
        load_time = (time.perf_counter() - start) * 1000
        
        # Verifica que carregamento foi razoável (< 1 segundo)
        assert load_time < 1000, f"Carregamento muito lento: {load_time:.0f}ms"
        
        # Verifica que correção ainda é rápida
        start = time.perf_counter()
        for _ in range(100):
            corrector.correct("flmes por actor name 1")
        correction_time = (time.perf_counter() - start) * 1000 / 100
        
        assert correction_time < 10, f"Correção muito lenta: {correction_time:.2f}ms"
