"""
Intent Data Augmentation Script

Generates augmented training examples for semantic intent classification.
Uses multiple strategies: back-translation, paraphrasing, templates, and synonyms.

Usage:
    python scripts/augment_intent_data.py
"""

import json
import logging
from pathlib import Path
from typing import List, Dict
import random

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)


class IntentDataAugmenter:
    """
    Augments intent training data with multiple strategies.
    """
    
    def __init__(self):
        """Initialize augmenter with synonym dictionaries."""
        # Synonym mappings for common words in queries
        self.synonyms = {
            # Verbs
            "filmes": ["obras", "produções", "títulos", "longas"],
            "mostre": ["exiba", "apresente", "liste", "traga"],
            "quero": ["gostaria", "desejo", "preciso"],
            "ver": ["assistir", "conferir", "visualizar"],
            "recomendar": ["sugerir", "indicar", "recomendar"],
            
            # Actors/People
            "ator": ["artista", "intérprete", "protagonista"],
            "atriz": ["artista", "intérprete", "protagonista"],
            "diretor": ["cineasta", "realizador"],
            "diretora": ["cineasta", "realizadora"],
            
            # Film terms
            "filme": ["longa", "produção", "obra", "título"],
            "elenco": ["atores", "cast", "protagonistas"],
            "gênero": ["tipo", "categoria", "estilo"],
            
            # Prepositions
            "de": ["do", "da"],
            "por": ["com", "de"],
            "com": ["por", "estrelando"],
        }
        
        # Template patterns for generation
        self.templates = {
            "filmes_por_ator": [
                "filmes {prep} {ator}",
                "{ator} {verb} em quais filmes",
                "obras {prep} {ator}",
                "filmografia {prep} {ator}",
                "{verb} filmes {prep} {ator}",
                "quais filmes tem {ator}",
                "{ator} atuou onde",
                "títulos {prep} {ator}",
            ],
            "filmes_por_genero": [
                "filmes de {genero}",
                "{verb} filmes de {genero}",
                "quero {verb} {genero}",
                "algo {genero}",
                "títulos de {genero}",
                "me mostre {genero}",
                "obras do gênero {genero}",
            ],
            "filmes_por_diretor": [
                "filmes {prep} diretor {diretor}",
                "filmes dirigidos por {diretor}",
                "obras de {diretor}",
                "{diretor} dirigiu o que",
                "filmografia de {diretor}",
                "direção de {diretor}",
            ],
            "genero_do_filme": [
                "qual o gênero de {filme}",
                "{filme} é de que gênero",
                "tipo de {filme}",
                "categoria de {filme}",
                "qual estilo de {filme}",
            ],
            "diretor_do_filme": [
                "quem dirigiu {filme}",
                "diretor de {filme}",
                "quem fez {filme}",
                "{filme} foi dirigido por quem",
                "quem é o diretor de {filme}",
            ],
            "atores_do_filme": [
                "quem atuou em {filme}",
                "elenco de {filme}",
                "atores de {filme}",
                "quem está em {filme}",
                "protagonistas de {filme}",
            ],
            "recomendar_filme": [
                "{verb} um filme",
                "sugestão de filme",
                "recomende algo",
                "indique um filme",
                "sugira um título",
            ],
            "filme_aleatorio": [
                "filme aleatório",
                "qualquer filme",
                "me surpreenda",
                "sorteia um filme",
                "escolha um filme",
            ],
        }
        
        # Sample entities for template filling
        self.sample_entities = {
            "ator": ["Tom Hanks", "Brad Pitt", "Adam Sandler", "Denzel Washington", "Leonardo DiCaprio"],
            "genero": ["ação", "comédia", "drama", "terror", "romance", "ficção científica"],
            "diretor": ["Steven Spielberg", "Christopher Nolan", "Quentin Tarantino", "Martin Scorsese"],
            "filme": ["Matrix", "Inception", "Titanic", "Jaws", "Forrest Gump"],
            "prep": ["de", "do", "com", "por"],
            "verb": ["mostre", "quero ver", "me mostre", "lista", "traga"],
        }
    
    def replace_with_synonym(self, text: str, word: str, probability: float = 0.5) -> str:
        """
        Replace word with synonym in text.
        
        Args:
            text: Original text
            word: Word to replace
            probability: Probability of replacement
            
        Returns:
            Text with possible synonym replacement
        """
        if word not in self.synonyms or random.random() > probability:
            return text
        
        synonyms = self.synonyms[word]
        synonym = random.choice(synonyms)
        
        # Replace all occurrences (case-insensitive)
        import re
        pattern = re.compile(re.escape(word), re.IGNORECASE)
        return pattern.sub(synonym, text, count=1)
    
    def augment_with_synonyms(self, text: str, n_variations: int = 3) -> List[str]:
        """
        Generate variations by replacing words with synonyms.
        
        Args:
            text: Original text
            n_variations: Number of variations to generate
            
        Returns:
            List of augmented texts
        """
        variations = []
        
        for _ in range(n_variations):
            augmented = text
            
            # Try to replace 1-2 words
            words_to_try = [w for w in self.synonyms.keys() if w in text.lower()]
            if words_to_try:
                n_replacements = min(2, len(words_to_try))
                words = random.sample(words_to_try, n_replacements)
                
                for word in words:
                    augmented = self.replace_with_synonym(augmented, word, probability=0.7)
            
            if augmented != text and augmented not in variations:
                variations.append(augmented)
        
        return variations
    
    def generate_from_templates(self, intent: str, n_samples: int = 10) -> List[str]:
        """
        Generate examples from templates.
        
        Args:
            intent: Intent name
            n_samples: Number of samples to generate
            
        Returns:
            List of generated examples
        """
        if intent not in self.templates:
            return []
        
        templates = self.templates[intent]
        generated = []
        
        for _ in range(n_samples):
            template = random.choice(templates)
            
            # Fill template with sample entities
            example = template
            for entity_type, entities in self.sample_entities.items():
                placeholder = f"{{{entity_type}}}"
                if placeholder in template:
                    entity = random.choice(entities)
                    example = example.replace(placeholder, entity)
            
            if example not in generated and "{" not in example:  # Valid if no unfilled placeholders
                generated.append(example)
        
        return generated
    
    def augment_examples(self, examples: List[str], target_count: int = 20) -> List[str]:
        """
        Augment list of examples to reach target count.
        
        Args:
            examples: Original examples
            target_count: Target number of total examples
            
        Returns:
            Augmented list of examples
        """
        augmented = examples.copy()
        
        # Strategy 1: Synonym replacement
        for example in examples:
            variations = self.augment_with_synonyms(example, n_variations=3)
            augmented.extend(variations)
        
        # Remove duplicates
        augmented = list(dict.fromkeys(augmented))
        
        return augmented[:target_count]
    
    def augment_intent_patterns(self, patterns: dict) -> dict:
        """
        Augment all intent patterns.
        
        Args:
            patterns: Original intent patterns
            
        Returns:
            Augmented patterns with additional examples
        """
        augmented_data = {}
        
        for intent, pattern in patterns.items():
            original_examples = pattern.get("examples", [])
            
            if not original_examples:
                logger.warning(f"Intent '{intent}' has no examples, skipping")
                continue
            
            logger.info(f"Augmenting '{intent}' ({len(original_examples)} original examples)...")
            
            # Combine strategies
            augmented_examples = []
            
            # 1. Keep originals
            augmented_examples.extend(original_examples)
            
            # 2. Template generation
            if intent in self.templates:
                template_examples = self.generate_from_templates(intent, n_samples=10)
                augmented_examples.extend(template_examples)
                logger.debug(f"  + {len(template_examples)} from templates")
            
            # 3. Synonym replacement
            synonym_examples = []
            for example in original_examples:
                variations = self.augment_with_synonyms(example, n_variations=5)
                synonym_examples.extend(variations)
            augmented_examples.extend(synonym_examples)
            logger.debug(f"  + {len(synonym_examples)} from synonyms")
            
            # Remove duplicates while preserving order
            seen = set()
            unique_examples = []
            for ex in augmented_examples:
                ex_lower = ex.lower().strip()
                if ex_lower not in seen:
                    seen.add(ex_lower)
                    unique_examples.append(ex)
            
            augmented_data[intent] = {
                "original": original_examples,
                "augmented": unique_examples
            }
            
            logger.info(f"  ✅ '{intent}': {len(original_examples)} → {len(unique_examples)} examples")
        
        return augmented_data


def add_manual_paraphrases() -> dict:
    """
    Add manually curated paraphrases for challenging queries.
    
    Returns:
        Dict of intent -> additional paraphrases
    """
    return {
        "filmes_por_genero": [
            "quero rir",  # → comédia
            "algo engraçado",  # → comédia
            "quero chorar",  # → drama
            "algo assustador",  # → terror
            "me dá medo",  # → terror
            "quero me assustar",  # → terror
            "algo romântico",  # → romance
            "filme de amor",  # → romance
            "adrenalina",  # → ação
            "explosões e tiros",  # → ação
            "futurista",  # → ficção científica
            "espacial",  # → ficção científica
        ],
        "filmes_por_diretor": [
            "aquele diretor inglês do batman",  # → Christopher Nolan
            "o cara do Inception",  # → Christopher Nolan
            "diretor de tubarão",  # → Spielberg
            "quem fez Star Wars",  # → George Lucas
        ],
        "filmes_por_ator": [
            "o cara do lobo de wall street",  # → Leonardo DiCaprio
            "aquele que fala devagar",  # → Morgan Freeman
            "o cara do Forrest Gump",  # → Tom Hanks
        ],
        "filme_aleatorio": [
            "surpreenda-me",
            "escolha por mim",
            "algo diferente",
            "não sei o que ver",
        ],
        "recomendar_filme": [
            "tô sem ideia do que assistir",
            "me ajuda a escolher",
            "qual você recomenda",
            "o que vale a pena",
        ],
    }


def main():
    """Main augmentation pipeline."""
    logger.info("="*60)
    logger.info("INTENT DATA AUGMENTATION")
    logger.info("="*60)
    
    # Load original patterns
    patterns_path = Path(__file__).parent.parent / "app" / "intent_patterns.json"
    logger.info(f"\n📖 Loading patterns from: {patterns_path}")
    
    with open(patterns_path, "r", encoding="utf-8") as f:
        original_patterns = json.load(f)
    
    logger.info(f"✅ Loaded {len(original_patterns)} intents")
    
    # Augment
    logger.info("\n🔄 Starting augmentation...")
    augmenter = IntentDataAugmenter()
    augmented_data = augmenter.augment_intent_patterns(original_patterns)
    
    # Add manual paraphrases
    logger.info("\n✍️  Adding manual paraphrases...")
    manual_paraphrases = add_manual_paraphrases()
    for intent, paraphrases in manual_paraphrases.items():
        if intent in augmented_data:
            augmented_data[intent]["augmented"].extend(paraphrases)
            logger.info(f"  + {len(paraphrases)} paraphrases for '{intent}'")
    
    # Save augmented data (flat structure: intent -> list of examples)
    output_path = Path(__file__).parent.parent / "app" / "intent_examples_augmented.json"
    logger.info(f"\n💾 Saving to: {output_path}")
    
    # Convert to flat structure
    flat_data = {}
    for intent, data in augmented_data.items():
        # Combine and deduplicate
        all_examples = list(set(data["original"] + data["augmented"]))
        flat_data[intent] = all_examples
    
    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(flat_data, f, ensure_ascii=False, indent=2)
    
    # Statistics
    logger.info("\n" + "="*60)
    logger.info("STATISTICS")
    logger.info("="*60)
    
    total_original = sum(len(data["original"]) for data in augmented_data.values())
    total_augmented = sum(len(data["augmented"]) for data in augmented_data.values())
    
    logger.info(f"\nTotal intents: {len(augmented_data)}")
    logger.info(f"Original examples: {total_original}")
    logger.info(f"Augmented examples: {total_augmented}")
    logger.info(f"Augmentation ratio: {total_augmented / total_original:.1f}x")
    logger.info(f"Average per intent: {total_augmented / len(augmented_data):.1f} examples")
    
    # Per-intent breakdown
    logger.info("\n📊 Per-Intent Breakdown:")
    for intent, data in sorted(augmented_data.items()):
        orig = len(data["original"])
        aug = len(data["augmented"])
        logger.info(f"  {intent:30s}: {orig:2d} → {aug:3d} ({aug/orig:.1f}x)")
    
    logger.info("\n✅ Augmentation complete!")
    logger.info(f"📁 Output: {output_path}")
    
    return 0


if __name__ == "__main__":
    import sys
    sys.exit(main())
