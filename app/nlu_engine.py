"""
NLU Engine - Natural Language Understanding para o Chatbot

Este módulo implementa o processamento de linguagem natural usando spaCy,
permitindo detecção de intenções e extração de entidades de queries do usuário.
"""

import spacy
import json
import logging
from pathlib import Path
from typing import Dict, List, Optional, Tuple
from thefuzz import fuzz

logger = logging.getLogger(__name__)


class NLUEngine:
    """
    Motor de NLU que processa texto em português e extrai intenções e entidades.
    """
    
    def __init__(self):
        """Inicializa o motor NLU carregando o modelo spaCy e padrões de intenção."""
        try:
            self.nlp = spacy.load("pt_core_news_sm")
            logger.info("✅ Modelo spaCy 'pt_core_news_sm' carregado com sucesso")
        except OSError:
            logger.error("❌ Modelo spaCy não encontrado. Execute: python -m spacy download pt_core_news_sm")
            raise
        
        self.intent_patterns = self._load_patterns()
        logger.info(f"✅ {len(self.intent_patterns)} padrões de intenção carregados")
    
    def _load_patterns(self) -> Dict:
        """Carrega os padrões de intenção do arquivo JSON."""
        patterns_path = Path(__file__).parent / "intent_patterns.json"
        
        try:
            with open(patterns_path, "r", encoding="utf-8") as f:
                return json.load(f)
        except FileNotFoundError:
            logger.warning(f"⚠️ Arquivo de padrões não encontrado: {patterns_path}")
            return {}
    
    def parse(self, text: str) -> Dict:
        """
        Analisa o texto e retorna a intenção detectada com entidades extraídas.
        
        Args:
            text: Texto do usuário para análise
            
        Returns:
            Dict com:
                - intent: Intenção detectada (ex: "filmes_por_ator")
                - entities: Dicionário de entidades extraídas
                - confidence: Score de confiança (0.0 a 1.0)
                - original_text: Texto original
                
        Examples:
            >>> nlu = NLUEngine()
            >>> nlu.parse("filmes por tom hanks")
            {
                'intent': 'filmes_por_ator',
                'entities': {'ator': 'Tom Hanks'},
                'confidence': 0.92,
                'original_text': 'filmes por tom hanks'
            }
        """
        text_lower = text.lower().strip()
        doc = self.nlp(text)
        
        # 1. Detectar intenção
        intent, intent_confidence = self._detect_intent(text_lower, doc)
        
        # 2. Extrair entidades baseado no tipo de intenção
        entities = self._extract_entities(text, doc, intent)
        
        # 3. Calcular confiança geral
        entity_confidence = 1.0 if entities else 0.5
        overall_confidence = (intent_confidence + entity_confidence) / 2
        
        result = {
            "intent": intent,
            "entities": entities,
            "confidence": overall_confidence,
            "original_text": text
        }
        
        logger.debug(f"NLU Parse: {result}")
        return result
    
    def _detect_intent(self, text_lower: str, doc) -> Tuple[str, float]:
        """
        Detecta a intenção principal do texto.
        
        Returns:
            Tuple com (intent_name, confidence_score)
        """
        best_intent = "unknown"
        best_score = 0.0
        
        for intent_name, pattern in self.intent_patterns.items():
            score = 0.0
            matches = 0
            
            # Verifica presença de keywords
            for keyword in pattern["keywords"]:
                if keyword in text_lower:
                    matches += 1
                    score += 0.4
            
            # Verifica presença de preposições (se houver)
            if pattern["prepositions"]:
                for prep in pattern["prepositions"]:
                    if prep in text_lower:
                        score += 0.2
                        break
            
            # Bônus se encontrou pelo menos uma keyword
            if matches > 0:
                score += 0.2
            
            # Normaliza score
            score = min(score, 1.0)
            
            if score > best_score:
                best_score = score
                best_intent = intent_name
        
        return best_intent, best_score
    
    def _extract_entities(self, text: str, doc, intent: str) -> Dict[str, str]:
        """
        Extrai entidades do texto baseado no tipo de intenção.
        
        Returns:
            Dicionário de entidades {tipo: valor}
        """
        entities = {}
        
        if intent not in self.intent_patterns:
            return entities
        
        pattern = self.intent_patterns[intent]
        entity_type = pattern.get("entity_type", "NONE")
        
        if entity_type == "NONE":
            return entities
        
        # Para PERSON (ator/diretor): extrai nome próprio
        if entity_type == "PERSON":
            # Tenta extrair entidades PERSON do spaCy
            persons = [ent.text for ent in doc.ents if ent.label_ == "PER"]
            
            if persons:
                if "ator" in intent or "diretor" in intent:
                    key = "ator" if "ator" in intent else "diretor"
                    entities[key] = persons[0]
            else:
                # Fallback: extrai texto após preposição
                entities = self._extract_after_preposition(text, pattern["prepositions"], "ator")
        
        # Para GENRE: extrai gênero
        elif entity_type == "GENRE":
            entities = self._extract_after_preposition(text, pattern["prepositions"], "genero")
        
        # Para MOVIE: extrai nome do filme
        elif entity_type == "MOVIE":
            entities = self._extract_after_preposition(text, pattern["prepositions"], "filme")
        
        # Para MIXED: pode ter múltiplas entidades
        elif entity_type == "MIXED":
            # Tenta extrair ator
            ator_entities = self._extract_after_preposition(text, ["por", "com", "do"], "ator")
            entities.update(ator_entities)
            
            # Tenta extrair gênero
            genero_entities = self._extract_after_preposition(text.lower(), ["de"], "genero")
            entities.update(genero_entities)
        
        return entities
    
    def _extract_after_preposition(self, text: str, prepositions: List[str], entity_key: str) -> Dict[str, str]:
        """
        Extrai texto que vem após uma preposição.
        
        Returns:
            Dict com {entity_key: extracted_text}
        """
        text_lower = text.lower()
        
        for prep in prepositions:
            pattern = f" {prep} "
            if pattern in text_lower:
                # Encontra posição da preposição
                idx = text_lower.find(pattern)
                # Extrai tudo após a preposição
                after_prep = text[idx + len(pattern):].strip()
                
                # Remove palavras comuns no final
                stop_words = [" em ", " e ", " ou ", " que ", " de "]
                for stop in stop_words:
                    if stop in after_prep.lower():
                        after_prep = after_prep[:after_prep.lower().find(stop)].strip()
                
                if after_prep:
                    return {entity_key: after_prep}
        
        return {}
    
    def get_similar_queries(self, text: str, top_n: int = 3) -> List[str]:
        """
        Retorna queries de exemplo similares ao texto fornecido.
        
        Args:
            text: Texto do usuário
            top_n: Número de sugestões a retornar
            
        Returns:
            Lista de queries de exemplo
        """
        all_examples = []
        
        # Coleta todos os exemplos de todos os intents
        for intent_name, pattern in self.intent_patterns.items():
            examples = pattern.get("examples", [])
            for example in examples:
                similarity = fuzz.ratio(text.lower(), example.lower())
                all_examples.append((example, similarity))
        
        # Ordena por similaridade e retorna top N
        all_examples.sort(key=lambda x: x[1], reverse=True)
        return [ex[0] for ex in all_examples[:top_n]]


# Singleton instance (lazy loaded)
_nlu_instance: Optional[NLUEngine] = None


def get_nlu_engine() -> NLUEngine:
    """
    Retorna a instância singleton do NLU Engine.
    
    Returns:
        Instância do NLUEngine
    """
    global _nlu_instance
    if _nlu_instance is None:
        _nlu_instance = NLUEngine()
    return _nlu_instance
