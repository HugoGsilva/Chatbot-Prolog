import sys
import os
import asyncio
import logging

# Adiciona o diretório raiz ao path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from app.nlu_engine import NLUEngine
from app.nlu import find_best_film, FILM_CACHE

# Configura logging
logging.basicConfig(level=logging.DEBUG)

# Popula cache de filmes com alguns exemplos para teste
FILM_CACHE.extend(["MATRIX", "INCEPTION", "TITANIC", "AVENGERS", "O PODEROSO CHEFAO"])

async def test_genre_intent():
    print("--- INICIANDO DEBUG DE GENERO DO FILME ---")
    
    nlu = NLUEngine()
    
    test_cases = [
        "qual o gênero de Matrix",
        "genero do filme Inception",
        "qual a categoria de Titanic",
        "tipo do filme Avengers"
    ]
    
    for text in test_cases:
        print(f"\nInput: '{text}'")
        result = nlu.parse(text)
        print(f"  Intent: {result.intent}")
        print(f"  Confidence: {result.confidence:.2f}")
        print(f"  Entities: {result.entities}")
        
        if "filme" in result.entities:
            filme_raw = result.entities["filme"]
            best = find_best_film(filme_raw)
            print(f"  -> Fuzzy Film '{filme_raw}' => '{best}'")
        else:
            print("  -> NENHUM FILME EXTRAÍDO!")

if __name__ == "__main__":
    asyncio.run(test_genre_intent())
