"""
Instancia a aplicação FastAPI e expõe um endpoint raiz de verificação
para confirmar que o servidor está rodando.
"""

from fastapi import FastAPI
from .prolog_service import prolog_service


app = FastAPI(
    title="Sakila-Prolog Chatbot API",
    description="API para consultar a base Sakila usando lógica Prolog.",
    version="0.1.0",
)


@app.get("/")
async def health_check():
    return {"status": "Sakila-Prolog API running"}


@app.on_event("startup")
async def on_startup():
    # Carrega as regras Prolog no startup (carregamento único)
    prolog_service.load_rules("prolog/rules/inferencia.pl")