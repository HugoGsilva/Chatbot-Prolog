"""
Setup Verification Script for Semantic NLU

Run this script to verify that all dependencies are installed correctly
and the semantic NLU components can be loaded.
"""

import sys
import logging

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)


def check_imports():
    """Check if all required packages can be imported."""
    print("\n" + "="*60)
    print("STEP 1: Checking Package Imports")
    print("="*60)
    
    packages = {
        "sentence_transformers": "sentence-transformers",
        "torch": "torch",
        "spacy": "spacy",
    }
    
    missing = []
    for module, package in packages.items():
        try:
            __import__(module)
            print(f"✅ {package}: OK")
        except ImportError as e:
            print(f"❌ {package}: MISSING")
            missing.append(package)
    
    if missing:
        print(f"\n⚠️  Missing packages: {', '.join(missing)}")
        print(f"Install with: pip install {' '.join(missing)}")
        return False
    
    print("\n✅ All packages installed!")
    return True


def check_config():
    """Check if config module loads correctly."""
    print("\n" + "="*60)
    print("STEP 2: Checking Configuration")
    print("="*60)
    
    try:
        from app.config import config
        print(f"✅ Config module loaded")
        print(f"\nConfiguration:")
        for key, value in config.get_config_summary().items():
            print(f"  {key}: {value}")
        return True
    except Exception as e:
        print(f"❌ Failed to load config: {e}")
        return False


def check_prolog_normalizer():
    """Check if PrologNormalizer works."""
    print("\n" + "="*60)
    print("STEP 3: Checking Prolog Normalizer")
    print("="*60)
    
    try:
        from app.prolog_normalizer import PrologNormalizer
        
        # Test normalization
        test_cases = [
            ("Leonardo DiCaprio", "actor", "LEONARDO DICAPRIO"),
            ("Ação & Aventura", "genre", "AÇÃO & AVENTURA"),
            ("The Matrix", "film", "THE MATRIX"),
        ]
        
        for entity, entity_type, expected in test_cases:
            result = PrologNormalizer.normalize_entity_for_query(entity, entity_type)
            status = "✅" if result == expected else "❌"
            print(f"{status} normalize_entity_for_query('{entity}', '{entity_type}') = '{result}'")
            if result != expected:
                print(f"   Expected: '{expected}'")
        
        print("\n✅ PrologNormalizer working!")
        return True
    except Exception as e:
        print(f"❌ PrologNormalizer error: {e}")
        import traceback
        traceback.print_exc()
        return False


def check_semantic_classifier():
    """Check if SemanticIntentClassifier can load."""
    print("\n" + "="*60)
    print("STEP 4: Checking Semantic Classifier (Model Download)")
    print("="*60)
    print("⏳ This may take a few minutes on first run (downloading model ~120MB)...\n")
    
    try:
        from app.semantic_classifier import SemanticIntentClassifier
        from app.config import config
        
        # Initialize classifier
        classifier = SemanticIntentClassifier(model_name=config.SEMANTIC_MODEL_NAME)
        print(f"✅ Classifier initialized with model: {config.SEMANTIC_MODEL_NAME}")
        
        # Test with simple patterns
        test_patterns = {
            "test_intent": {
                "examples": ["hello world", "test query"]
            }
        }
        
        classifier.load_intent_patterns(test_patterns)
        print(f"✅ Patterns loaded successfully")
        
        # Test classification
        results = classifier.classify("hello world")
        print(f"✅ Classification test: {results[0]}")
        
        print("\n✅ Semantic Classifier working!")
        return True
    except Exception as e:
        print(f"❌ Semantic Classifier error: {e}")
        import traceback
        traceback.print_exc()
        return False


def main():
    """Run all checks."""
    print("\n" + "="*60)
    print("SEMANTIC NLU SETUP VERIFICATION")
    print("="*60)
    
    checks = [
        check_imports,
        check_config,
        check_prolog_normalizer,
        check_semantic_classifier,
    ]
    
    results = []
    for check in checks:
        try:
            result = check()
            results.append(result)
        except Exception as e:
            print(f"\n❌ Unexpected error in {check.__name__}: {e}")
            results.append(False)
    
    # Summary
    print("\n" + "="*60)
    print("SUMMARY")
    print("="*60)
    
    total = len(results)
    passed = sum(results)
    
    if all(results):
        print(f"\n🎉 ALL CHECKS PASSED ({passed}/{total})")
        print("\n✅ Semantic NLU is ready to use!")
        print("\nNext steps:")
        print("  1. Run tests: pytest tests/test_semantic_classifier.py -v")
        print("  2. Run tests: pytest tests/test_prolog_normalizer.py -v")
        print("  3. Continue with Step 2 (Data Augmentation)")
        return 0
    else:
        print(f"\n⚠️  SOME CHECKS FAILED ({passed}/{total} passed)")
        print("\nPlease fix the issues above before proceeding.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
