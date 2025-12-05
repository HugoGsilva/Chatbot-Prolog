import asyncio
import sys
import os

# Adiciona o diretório raiz ao path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from app.intent_router import IntentRouter
from app.schemas import NLUResult, ChatResponse

async def test_help_intent():
    print("Iniciando teste de Intent 'ajuda'...")
    
    router = IntentRouter()
    
    # Simula resultado do NLU para "ajuda"
    nlu_result = NLUResult(
        intent="ajuda",
        entities={},
        confidence=0.95,
        original_text="ajuda",
        corrected_text=None
    )
    
    session_id = "test_session_123"
    
    try:
        print("Chamando router.route()...")
        response = await router.route(nlu_result, session_id)
        
        print("\n--- Resposta Recebida ---")
        print(f"Type: {response.type}")
        print(f"Content Type: {type(response.content)}")
        print(f"Content: {response.content}")
        print(f"Suggestions: {response.suggestions}")
        
        # Tenta validar com Pydantic (já deve ter sido validado na criação, mas reforçando)
        print("\nValidando modelo Pydantic...")
        json_output = response.model_dump_json()
        print("Serialização JSON OK.")
        print(json_output)
        
    except Exception as e:
        print(f"\n[ERRO] Falha durante o teste: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(test_help_intent())
