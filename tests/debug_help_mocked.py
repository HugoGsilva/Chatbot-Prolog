import sys
import os
from unittest.mock import MagicMock

# Mock pyswip antes de importar qualquer coisa do app
sys.modules["pyswip"] = MagicMock()
sys.modules["pyswip.Prolog"] = MagicMock()

# Mock prolog_service
prolog_service_mock = MagicMock()
sys.modules["app.prolog_service"] = MagicMock()
sys.modules["app.prolog_service"].prolog_service = prolog_service_mock
sys.modules["app.prolog_service"].PrologTimeoutError = Exception

# Adiciona o diretório raiz ao path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

import asyncio
from app.intent_router import IntentRouter
from app.schemas import NLUResult, ChatResponse, ResponseType

async def test_help_intent():
    print("Iniciando teste de Intent 'ajuda' (MOCKED)...")
    
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
        
        # Verifica se o tipo é HELP
        if response.type == ResponseType.HELP:
            print("✅ Tipo de resposta correto: HELP")
        else:
            print(f"❌ Tipo de resposta incorreto: {response.type}")
            
        # Verifica estrutura do content
        content = response.content
        if isinstance(content, dict) and "message" in content and "examples" in content:
            print("✅ Estrutura do content correta (dict com message e examples)")
        else:
            print("❌ Estrutura do content incorreta")
            
        # Tenta validar com Pydantic
        print("\nValidando modelo Pydantic...")
        json_output = response.model_dump_json()
        print("✅ Serialização JSON OK.")
        print(json_output)
        
    except Exception as e:
        print(f"\n[ERRO] Falha durante o teste: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(test_help_intent())
