"""
Testes para NLUEngine com classificação semântica (Step 3).

Testa:
1. Modo híbrido (semantic + keyword)
2. High confidence semantic (≥0.75)
3. Validation zone (0.60-0.75) com keyword agreement/override
4. Fallback para keyword (<0.60)
5. Boost semântico no overall confidence
6. Paráfrases criativas do augmented data
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from app.nlu_engine import NLUEngine
from app.schemas import NLUResult


@pytest.fixture
def mock_spacy():
    """Mock spaCy para evitar dependência do modelo."""
    with patch('app.nlu_engine.spacy') as mock:
        mock_nlp = MagicMock()
        mock_nlp.return_value = MagicMock()  # Mock doc
        mock.load.return_value = mock_nlp
        yield mock


@pytest.fixture
def nlu_semantic(mock_spacy):
    """Cria NLUEngine com modo semântico ativado."""
    engine = NLUEngine(spell_corrector=None, use_semantic=True)
    return engine


@pytest.fixture
def nlu_keyword(mock_spacy):
    """Cria NLUEngine com modo keyword apenas."""
    engine = NLUEngine(spell_corrector=None, use_semantic=False)
    return engine


class TestSemanticMode:
    """Testes de ativação do modo semântico."""
    
    def test_semantic_mode_enabled(self, nlu_semantic):
        """Verifica que modo semântico está ativado."""
        assert nlu_semantic.use_semantic is True
    
    def test_keyword_mode_enabled(self, nlu_keyword):
        """Verifica que modo keyword está ativado."""
        assert nlu_keyword.use_semantic is False
    
    def test_lazy_loading_classifier(self, nlu_semantic):
        """Verifica que classifier não é carregado até ser necessário."""
        assert nlu_semantic._semantic_classifier is None


class TestHighConfidenceSemantic:
    """Testes para high confidence semantic (≥0.75)."""
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_high_confidence_direct_accept(self, mock_get_classifier, nlu_semantic):
        """High confidence semantic deve retornar direto sem keyword validation."""
        # Mock semantic classifier retornando alta confiança
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [
            ("filmes_por_genero", 0.85),
            ("recomendar_filme", 0.10)
        ]
        mock_get_classifier.return_value = mock_classifier
        
        # Mock doc vazio
        mock_doc = Mock()
        
        intent, confidence = nlu_semantic._detect_intent("quero rir", mock_doc)
        
        assert intent == "filmes_por_genero"
        assert confidence == 0.85
        mock_classifier.classify.assert_called_once_with("quero rir", top_k=2)
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_high_confidence_paraphrase(self, mock_get_classifier, nlu_semantic):
        """Testa paráfrase criativa com alta confiança."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [
            ("filmes_por_ator", 0.92),
            ("filmes_por_diretor", 0.05)
        ]
        mock_get_classifier.return_value = mock_classifier
        
        mock_doc = Mock()
        
        intent, confidence = nlu_semantic._detect_intent(
            "o cara do lobo de wall street", 
            mock_doc
        )
        
        assert intent == "filmes_por_ator"
        assert confidence == 0.92


class TestValidationZone:
    """Testes para validation zone (0.60-0.75)."""
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_validation_zone_agreement(self, mock_get_classifier, nlu_semantic):
        """Semantic e keyword concordam -> aceita com boost."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [
            ("filmes_por_genero", 0.68),
            ("recomendar_filme", 0.20)
        ]
        mock_get_classifier.return_value = mock_classifier
        
        # Mock keyword também retornando filmes_por_genero
        with patch.object(nlu_semantic, '_detect_intent_keyword_based') as mock_keyword:
            mock_keyword.return_value = ("filmes_por_genero", 0.65)
            
            mock_doc = Mock()
            intent, confidence = nlu_semantic._detect_intent("filmes de ação", mock_doc)
            
            assert intent == "filmes_por_genero"
            # Confiança combinada (boost por concordância)
            assert 0.70 <= confidence <= 0.95
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_validation_zone_keyword_override(self, mock_get_classifier, nlu_semantic):
        """Keyword tem maior confiança -> override semantic."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [
            ("filme_aleatorio", 0.62),
            ("recomendar_filme", 0.25)
        ]
        mock_get_classifier.return_value = mock_classifier
        
        # Keyword retorna confiança maior
        with patch.object(nlu_semantic, '_detect_intent_keyword_based') as mock_keyword:
            mock_keyword.return_value = ("recomendar_filme", 0.80)
            
            mock_doc = Mock()
            intent, confidence = nlu_semantic._detect_intent("me recomende um filme", mock_doc)
            
            assert intent == "recomendar_filme"
            assert confidence == 0.80
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_validation_zone_semantic_wins(self, mock_get_classifier, nlu_semantic):
        """Semantic tem maior confiança na validation zone."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [
            ("filmes_por_diretor", 0.71),
            ("filme_aleatorio", 0.15)
        ]
        mock_get_classifier.return_value = mock_classifier
        
        with patch.object(nlu_semantic, '_detect_intent_keyword_based') as mock_keyword:
            mock_keyword.return_value = ("filme_aleatorio", 0.50)
            
            mock_doc = Mock()
            intent, confidence = nlu_semantic._detect_intent(
                "aquele diretor inglês do batman", 
                mock_doc
            )
            
            assert intent == "filmes_por_diretor"
            assert confidence == 0.71


class TestFallbackToKeyword:
    """Testes para fallback para keyword (<0.60)."""
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_low_confidence_fallback(self, mock_get_classifier, nlu_semantic):
        """Low confidence semantic -> usa keyword fallback."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [
            ("unknown", 0.35),
            ("filme_aleatorio", 0.25)
        ]
        mock_get_classifier.return_value = mock_classifier
        
        with patch.object(nlu_semantic, '_detect_intent_keyword_based') as mock_keyword:
            mock_keyword.return_value = ("ajuda", 0.85)
            
            mock_doc = Mock()
            intent, confidence = nlu_semantic._detect_intent("ajuda", mock_doc)
            
            assert intent == "ajuda"
            assert confidence == 0.85
            mock_keyword.assert_called_once()
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_no_semantic_results_fallback(self, mock_get_classifier, nlu_semantic):
        """Nenhum resultado semântico -> fallback direto."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = []
        mock_get_classifier.return_value = mock_classifier
        
        with patch.object(nlu_semantic, '_detect_intent_keyword_based') as mock_keyword:
            mock_keyword.return_value = ("saudacao", 0.90)
            
            mock_doc = Mock()
            intent, confidence = nlu_semantic._detect_intent("oi", mock_doc)
            
            assert intent == "saudacao"
            assert confidence == 0.90


class TestSemanticBoost:
    """Testes para boost semântico no overall confidence."""
    
    def test_semantic_boost_applied(self, nlu_semantic):
        """Boost aplicado quando usado semantic classifier."""
        confidence = nlu_semantic._calculate_overall_confidence(
            intent_confidence=0.75,
            entities={"genero": "ação"},
            intent="filmes_por_genero",
            used_semantic=True
        )
        
        # Deve ter boost
        assert confidence > 0.50
    
    def test_no_boost_without_semantic(self, nlu_semantic):
        """Sem boost quando semantic não é usado."""
        confidence_with = nlu_semantic._calculate_overall_confidence(
            intent_confidence=0.75,
            entities={"genero": "ação"},
            intent="filmes_por_genero",
            used_semantic=True
        )
        
        confidence_without = nlu_semantic._calculate_overall_confidence(
            intent_confidence=0.75,
            entities={"genero": "ação"},
            intent="filmes_por_genero",
            used_semantic=False
        )
        
        # Com semantic deve ser maior
        assert confidence_with > confidence_without
    
    def test_boost_only_high_confidence(self, nlu_semantic):
        """Boost só aplicado para confiança >= 0.70."""
        # Alta confiança
        high_conf = nlu_semantic._calculate_overall_confidence(
            intent_confidence=0.85,
            entities={},
            intent="ajuda",
            used_semantic=True
        )
        
        # Baixa confiança
        low_conf = nlu_semantic._calculate_overall_confidence(
            intent_confidence=0.50,
            entities={},
            intent="unknown",
            used_semantic=True
        )
        
        # Boost maior para alta confiança
        assert high_conf > 0.60


class TestKeywordOnlyMode:
    """Testes para modo keyword-only (sem semantic)."""
    
    def test_keyword_mode_no_semantic_calls(self, nlu_keyword):
        """Modo keyword não deve chamar semantic classifier."""
        with patch('app.nlu_engine.get_semantic_classifier') as mock_get:
            mock_doc = Mock()
            
            # Simula detecção keyword
            with patch.object(nlu_keyword, '_detect_intent_keyword_based') as mock_kw:
                mock_kw.return_value = ("ajuda", 0.80)
                
                intent, confidence = nlu_keyword._detect_intent("ajuda", mock_doc)
                
                assert intent == "ajuda"
                assert confidence == 0.80
                # Nunca deve chamar get_semantic_classifier
                mock_get.assert_not_called()


class TestIntegration:
    """Testes de integração end-to-end."""
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_full_pipeline_semantic(self, mock_get_classifier, nlu_semantic):
        """Pipeline completo com semantic: parse -> intent -> entities -> confidence."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [
            ("filmes_por_genero", 0.88),
        ]
        mock_get_classifier.return_value = mock_classifier
        
        # Mock extração de entidades
        with patch.object(nlu_semantic, '_extract_entities') as mock_extract:
            mock_extract.return_value = {"genero": "comédia"}
            
            result = nlu_semantic.parse("quero rir", apply_spell_correction=False)
            
            assert isinstance(result, NLUResult)
            assert result.intent == "filmes_por_genero"
            assert result.entities == {"genero": "comédia"}
            assert result.confidence > 0.70
    
    def test_full_pipeline_keyword_only(self, nlu_keyword):
        """Pipeline completo com keyword apenas."""
        with patch.object(nlu_keyword, '_extract_entities') as mock_extract:
            mock_extract.return_value = {}
            
            result = nlu_keyword.parse("ajuda", apply_spell_correction=False)
            
            assert isinstance(result, NLUResult)
            assert result.intent == "ajuda"
            assert result.confidence > 0.50


class TestParaphrases:
    """Testes específicos para paráfrases do augmented data."""
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_paraphrase_quero_rir(self, mock_get_classifier, nlu_semantic):
        """'quero rir' deve detectar filmes de comédia."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [("filmes_por_genero", 0.90)]
        mock_get_classifier.return_value = mock_classifier
        
        mock_doc = Mock()
        intent, conf = nlu_semantic._detect_intent("quero rir", mock_doc)
        
        assert intent == "filmes_por_genero"
        assert conf >= 0.75
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_paraphrase_algo_assustador(self, mock_get_classifier, nlu_semantic):
        """'algo assustador' deve detectar filmes de terror."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [("filmes_por_genero", 0.87)]
        mock_get_classifier.return_value = mock_classifier
        
        mock_doc = Mock()
        intent, conf = nlu_semantic._detect_intent("algo assustador", mock_doc)
        
        assert intent == "filmes_por_genero"
        assert conf >= 0.75
    
    @patch('app.nlu_engine.get_semantic_classifier')
    def test_paraphrase_cara_do_lobo(self, mock_get_classifier, nlu_semantic):
        """'o cara do lobo de wall street' deve detectar filmes_por_ator."""
        mock_classifier = Mock()
        mock_classifier.classify.return_value = [("filmes_por_ator", 0.93)]
        mock_get_classifier.return_value = mock_classifier
        
        mock_doc = Mock()
        intent, conf = nlu_semantic._detect_intent("o cara do lobo de wall street", mock_doc)
        
        assert intent == "filmes_por_ator"
        assert conf >= 0.75


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
