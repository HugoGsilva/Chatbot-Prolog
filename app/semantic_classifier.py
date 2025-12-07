"""
Semantic Intent Classifier

Uses sentence-transformers to classify user intents based on semantic similarity
to example queries, providing more robust intent detection than keyword matching.
"""

import logging
from typing import Dict, List, Tuple, Optional
from sentence_transformers import SentenceTransformer, util
import torch

logger = logging.getLogger(__name__)


class SemanticIntentClassifier:
    """
    Semantic intent classifier using sentence embeddings.
    
    Computes cosine similarity between user query and example queries
    for each intent, providing confidence scores for intent classification.
    """
    
    def __init__(self, model_name: str = "paraphrase-multilingual-MiniLM-L12-v2"):
        """
        Initialize semantic classifier with sentence-transformer model.
        
        Args:
            model_name: HuggingFace model name for sentence embeddings
        """
        self.model_name = model_name
        self._model: Optional[SentenceTransformer] = None
        self.intent_embeddings: Dict[str, torch.Tensor] = {}
        self.intent_examples: Dict[str, List[str]] = {}
        self._is_loaded = False
        
        logger.info(f"SemanticIntentClassifier initialized with model: {model_name}")
    
    @property
    def model(self) -> SentenceTransformer:
        """
        Lazy loading of sentence-transformer model.
        
        Returns:
            Loaded SentenceTransformer model
        """
        if self._model is None:
            logger.info(f"Loading sentence-transformer model: {self.model_name}...")
            self._model = SentenceTransformer(self.model_name)
            logger.info(f"✅ Model loaded successfully")
        return self._model
    
    def load_intent_patterns(self, patterns: dict) -> None:
        """
        Load intent patterns and compute embeddings for example queries.
        
        Args:
            patterns: Intent patterns dict from intent_patterns.json
        """
        logger.info("Computing embeddings for intent examples...")
        
        total_examples = 0
        for intent, pattern in patterns.items():
            examples = pattern.get("examples", [])
            
            if not examples:
                logger.warning(f"Intent '{intent}' has no examples, skipping")
                continue
            
            # Encode examples to embeddings
            embeddings = self.model.encode(
                examples, 
                convert_to_tensor=True,
                show_progress_bar=False
            )
            
            self.intent_embeddings[intent] = embeddings
            self.intent_examples[intent] = examples
            total_examples += len(examples)
            
            logger.debug(f"Intent '{intent}': {len(examples)} examples encoded")
        
        self._is_loaded = True
        logger.info(f"✅ Loaded {len(self.intent_embeddings)} intents with {total_examples} total examples")
    
    def classify(self, query: str, top_k: int = 3) -> List[Tuple[str, float]]:
        """
        Classify user query to intents using semantic similarity.
        
        Args:
            query: User query text
            top_k: Number of top intents to return
            
        Returns:
            List of (intent, confidence) tuples, sorted by confidence descending
            
        Example:
            >>> classifier.classify("filmes engraçados")
            [('filmes_por_genero', 0.82), ('recomendar_filme', 0.65), ...]
        """
        if not self._is_loaded:
            raise RuntimeError("Classifier not loaded. Call load_intent_patterns() first.")
        
        if not query or not query.strip():
            return [("unknown", 0.0)]
        
        # Encode query
        query_embedding = self.model.encode(
            query, 
            convert_to_tensor=True,
            show_progress_bar=False
        )
        
        # Compute similarities for each intent
        scores = []
        for intent, example_embeddings in self.intent_embeddings.items():
            # Compute cosine similarity with all examples of this intent
            similarities = util.cos_sim(query_embedding, example_embeddings)
            
            # Use maximum similarity across all examples
            max_similarity = similarities.max().item()
            scores.append((intent, max_similarity))
        
        # Sort by confidence descending
        scores.sort(key=lambda x: x[1], reverse=True)
        
        return scores[:top_k]
    
    def classify_with_details(self, query: str, top_k: int = 3) -> List[dict]:
        """
        Classify query with detailed information about matches.
        
        Args:
            query: User query text
            top_k: Number of top intents to return
            
        Returns:
            List of dicts with intent, confidence, and best matching example
        """
        if not self._is_loaded:
            raise RuntimeError("Classifier not loaded. Call load_intent_patterns() first.")
        
        query_embedding = self.model.encode(
            query, 
            convert_to_tensor=True,
            show_progress_bar=False
        )
        
        results = []
        for intent, example_embeddings in self.intent_embeddings.items():
            similarities = util.cos_sim(query_embedding, example_embeddings)
            
            # Find best matching example
            max_idx = similarities.argmax().item()
            max_similarity = similarities[0, max_idx].item()
            best_example = self.intent_examples[intent][max_idx]
            
            results.append({
                "intent": intent,
                "confidence": max_similarity,
                "best_example": best_example,
                "similarity_to_example": max_similarity
            })
        
        # Sort by confidence
        results.sort(key=lambda x: x["confidence"], reverse=True)
        
        return results[:top_k]
    
    def is_loaded(self) -> bool:
        """Check if classifier has been loaded with patterns."""
        return self._is_loaded
    
    def get_intents(self) -> List[str]:
        """Get list of all loaded intents."""
        return list(self.intent_embeddings.keys())
    
    def get_example_count(self, intent: str) -> int:
        """Get number of examples for a specific intent."""
        return len(self.intent_examples.get(intent, []))


# Singleton instance (lazy loaded)
_classifier_instance: Optional[SemanticIntentClassifier] = None


def get_semantic_classifier() -> Optional[SemanticIntentClassifier]:
    """
    Get singleton instance of SemanticIntentClassifier.
    
    Returns:
        Classifier instance or None if not initialized
    """
    return _classifier_instance


def initialize_semantic_classifier(
    model_name: str = "paraphrase-multilingual-MiniLM-L12-v2",
    intent_patterns: dict = None
) -> SemanticIntentClassifier:
    """
    Initialize and return singleton SemanticIntentClassifier.
    
    Args:
        model_name: HuggingFace model name
        intent_patterns: Intent patterns dict (optional, can load later)
        
    Returns:
        Initialized classifier instance
    """
    global _classifier_instance
    
    if _classifier_instance is None:
        _classifier_instance = SemanticIntentClassifier(model_name=model_name)
        
        if intent_patterns:
            _classifier_instance.load_intent_patterns(intent_patterns)
    
    return _classifier_instance
