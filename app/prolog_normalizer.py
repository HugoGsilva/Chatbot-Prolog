"""
Prolog Normalizer

Converts entities detected by NLU into Prolog-compatible format.
Ensures proper formatting for queries to the Prolog knowledge base.
"""

import re
import unicodedata
import logging
from typing import Optional

logger = logging.getLogger(__name__)


class PrologNormalizer:
    """
    Normalizes entities for Prolog queries.
    
    Handles conversion between Python strings and Prolog atoms,
    ensuring compatibility with the knowledge base format.
    """
    
    @staticmethod
    def normalize_to_prolog_atom(entity: str) -> str:
        """
        Convert entity to Prolog atom format (snake_case, lowercase).
        
        Rules:
        1. Lowercase
        2. Remove accents
        3. Replace spaces with underscores
        4. Replace & with underscores
        5. Remove special characters (keep only alphanumeric + underscore)
        6. Remove duplicate/leading/trailing underscores
        
        Args:
            entity: Entity name (e.g., "Leonardo DiCaprio", "Ação & Aventura")
            
        Returns:
            Prolog-compatible atom (e.g., "leonardo_dicaprio", "acao_aventura")
            
        Examples:
            >>> PrologNormalizer.normalize_to_prolog_atom("Leonardo DiCaprio")
            'leonardo_dicaprio'
            >>> PrologNormalizer.normalize_to_prolog_atom("Ação & Aventura")
            'acao_aventura'
            >>> PrologNormalizer.normalize_to_prolog_atom("The Rock")
            'the_rock'
        """
        if not entity:
            return ""
        
        # 1. Lowercase
        normalized = entity.lower()
        
        # 2. Remove accents (NFD decomposition)
        nfkd = unicodedata.normalize('NFKD', normalized)
        normalized = ''.join(c for c in nfkd if not unicodedata.combining(c))
        
        # 3. Replace spaces and & with underscore
        normalized = normalized.replace(' ', '_').replace('&', '_')
        
        # 4. Remove special characters (keep only a-z, 0-9, _)
        normalized = re.sub(r'[^a-z0-9_]', '', normalized)
        
        # 5. Remove duplicate underscores
        normalized = re.sub(r'_+', '_', normalized)
        
        # 6. Remove leading/trailing underscores
        normalized = normalized.strip('_')
        
        return normalized
    
    @staticmethod
    def normalize_entity_for_query(entity: str, entity_type: str) -> str:
        """
        Normalize entity based on type for KB query.
        
        Different entity types require different normalization:
        - actor/director/film: UPPERCASE (KB format)
        - genre: UPPERCASE (KB format)
        
        Args:
            entity: Entity name
            entity_type: Type of entity ('actor', 'director', 'film', 'genre')
            
        Returns:
            Normalized entity for Prolog query
            
        Examples:
            >>> PrologNormalizer.normalize_entity_for_query("tom hanks", "actor")
            'TOM HANKS'
            >>> PrologNormalizer.normalize_entity_for_query("ação", "genre")
            'AÇÃO'
        """
        if not entity:
            return ""
        
        # For all entity types, KB uses UPPERCASE
        if entity_type in ['actor', 'director', 'film', 'genre']:
            return entity.upper()
        
        # Default: return as-is
        logger.warning(f"Unknown entity_type '{entity_type}', returning uppercase")
        return entity.upper()
    
    @staticmethod
    def escape_prolog_string(text: str) -> str:
        """
        Escape string for Prolog query (single quotes).
        
        Args:
            text: String to escape
            
        Returns:
            Escaped string safe for Prolog query
            
        Examples:
            >>> PrologNormalizer.escape_prolog_string("O'Connor")
            "O\\'Connor"
        """
        if not text:
            return ""
        
        # Escape single quotes by doubling them (Prolog convention)
        # or using backslash (depends on Prolog implementation)
        escaped = text.replace("'", "\\'")
        
        return escaped
    
    @staticmethod
    def normalize_actor_name(name: str) -> str:
        """
        Normalize actor name for KB matching.
        
        Args:
            name: Actor name
            
        Returns:
            Normalized name (UPPERCASE)
        """
        return name.upper() if name else ""
    
    @staticmethod
    def normalize_director_name(name: str) -> str:
        """
        Normalize director name for KB matching.
        
        Args:
            name: Director name
            
        Returns:
            Normalized name (UPPERCASE)
        """
        return name.upper() if name else ""
    
    @staticmethod
    def normalize_film_title(title: str) -> str:
        """
        Normalize film title for KB matching.
        
        Args:
            title: Film title
            
        Returns:
            Normalized title (UPPERCASE)
        """
        return title.upper() if title else ""
    
    @staticmethod
    def normalize_genre_name(genre: str) -> str:
        """
        Normalize genre name for KB matching.
        
        Args:
            genre: Genre name
            
        Returns:
            Normalized genre (UPPERCASE)
        """
        return genre.upper() if genre else ""
    
    @classmethod
    def normalize_entity(cls, entity: str, entity_type: str) -> str:
        """
        Convenience method to normalize any entity.
        
        Args:
            entity: Entity value
            entity_type: Type ('actor', 'director', 'film', 'genre')
            
        Returns:
            Normalized entity
        """
        if entity_type == 'actor':
            return cls.normalize_actor_name(entity)
        elif entity_type == 'director':
            return cls.normalize_director_name(entity)
        elif entity_type == 'film':
            return cls.normalize_film_title(entity)
        elif entity_type == 'genre':
            return cls.normalize_genre_name(entity)
        else:
            return cls.normalize_entity_for_query(entity, entity_type)
