import sys
import os
import asyncio
import logging

# Adiciona o diretório raiz ao path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from app.nlu import is_valid_genre, GENRE_CACHE, GENRE_TRANSLATION_MAP

# Configura logging
logging.basicConfig(level=logging.DEBUG)

# Popula cache com alguns exemplos
GENRE_CACHE.extend(["ACTION & ADVENTURE", "COMEDIES", "DRAMAS", "MOVIES"])

def test_valid_genre():
    print("--- DEBUG IS_VALID_GENRE ---")
    
    cases = [
        "filme inception",
        "filme avengers",
        "ação",
        "comédia",
        "filme"
    ]
    
    for text in cases:
        print(f"\nTesting '{text}':")
        is_valid = is_valid_genre(text)
        print(f"  -> Valid? {is_valid}")

if __name__ == "__main__":
    test_valid_genre()
