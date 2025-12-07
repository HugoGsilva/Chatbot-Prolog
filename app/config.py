"""
Configuration Module - Semantic NLU Settings

Manages configuration for semantic parsing features with environment variable support.
"""

import os
from typing import Optional


class Settings:
    """
    Configuration settings for Semantic NLU.
    
    All settings can be overridden via environment variables.
    """
    
    # Semantic NLU Feature Flags
    USE_SEMANTIC_NLU: bool = os.getenv("USE_SEMANTIC_NLU", "True").lower() == "true"
    
    # Model Configuration
    SEMANTIC_MODEL_NAME: str = os.getenv(
        "SEMANTIC_MODEL_NAME", 
        "paraphrase-multilingual-MiniLM-L12-v2"  # Fast model (118M params)
    )
    
    # Confidence Thresholds
    SEMANTIC_INTENT_THRESHOLD: float = float(os.getenv("SEMANTIC_INTENT_THRESHOLD", "0.75"))
    SEMANTIC_ENTITY_THRESHOLD: float = float(os.getenv("SEMANTIC_ENTITY_THRESHOLD", "0.70"))
    
    # Hybrid Mode Settings
    SEMANTIC_MEDIUM_CONFIDENCE: float = 0.60  # Threshold for keyword validation
    SEMANTIC_WEIGHT_INTENT: float = 0.6  # Weight for semantic score in hybrid mode
    SEMANTIC_WEIGHT_KEYWORD: float = 0.4  # Weight for keyword score in hybrid mode
    
    # Entity Extraction Settings
    SEMANTIC_ENTITY_TOP_K: int = 5  # Number of alternative entities to return
    SEMANTIC_ENTITY_MIN_CANDIDATES: int = 1  # Minimum candidates to extract with spaCy
    
    # Performance Settings
    SEMANTIC_CACHE_TTL: int = 3600  # Redis cache TTL for embeddings (1 hour)
    SEMANTIC_BATCH_SIZE: int = 32  # Batch size for encoding multiple queries
    
    # A/B Testing
    SEMANTIC_ROLLOUT_PERCENTAGE: int = int(os.getenv("SEMANTIC_ROLLOUT_PERCENTAGE", "100"))
    
    @classmethod
    def is_semantic_enabled(cls) -> bool:
        """Check if semantic NLU is enabled."""
        return cls.USE_SEMANTIC_NLU
    
    @classmethod
    def should_use_semantic_for_session(cls, session_id: str) -> bool:
        """
        Determine if semantic NLU should be used for this session (A/B testing).
        
        Args:
            session_id: User session ID
            
        Returns:
            True if session should use semantic NLU
        """
        if not cls.USE_SEMANTIC_NLU:
            return False
        
        if cls.SEMANTIC_ROLLOUT_PERCENTAGE >= 100:
            return True
        
        # Hash-based routing for consistent A/B testing
        import hashlib
        hash_val = int(hashlib.md5(session_id.encode()).hexdigest(), 16)
        return (hash_val % 100) < cls.SEMANTIC_ROLLOUT_PERCENTAGE
    
    @classmethod
    def get_config_summary(cls) -> dict:
        """Return current configuration as dict for logging."""
        return {
            "USE_SEMANTIC_NLU": cls.USE_SEMANTIC_NLU,
            "SEMANTIC_MODEL_NAME": cls.SEMANTIC_MODEL_NAME,
            "SEMANTIC_INTENT_THRESHOLD": cls.SEMANTIC_INTENT_THRESHOLD,
            "SEMANTIC_ENTITY_THRESHOLD": cls.SEMANTIC_ENTITY_THRESHOLD,
            "SEMANTIC_ROLLOUT_PERCENTAGE": cls.SEMANTIC_ROLLOUT_PERCENTAGE,
        }


# Singleton instance
config = Settings()
