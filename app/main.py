"""
Instancia a aplicação FastAPI e expõe um endpoint raiz de verificação
para confirmar que o servidor está rodando.
"""

from fastapi import FastAPI


app = FastAPI(
    title="Sakila-Prolog Chatbot API",
    description="API para consultar a base Sakila usando lógica Prolog.",
    version="0.1.0",
)


@app.get("/")
async def health_check():
    return {"status": "Sakila-Prolog API running"}