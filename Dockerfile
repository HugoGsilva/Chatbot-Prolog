# syntax=docker/dockerfile:1
FROM python:3.11-slim

# Evita criação de .pyc e melhora logs
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

WORKDIR /app

# Dependências do sistema necessárias para Prolog (pyswip)
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      swi-prolog \
    && rm -rf /var/lib/apt/lists/*

# Instala dependências Python
COPY requirements.txt ./
RUN pip install --upgrade pip && \
    pip install -r requirements.txt && \
    pip install mysql-connector-python pytest

# Copia o código da aplicação
COPY . .

# Comando padrão: executar testes (pode ser sobrescrito pelo docker-compose)
CMD ["pytest", "-q"]