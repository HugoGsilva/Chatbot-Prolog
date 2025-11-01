# --- Fase 1: Builder (Instalação de Dependências) ---
FROM python:3.11-slim AS builder

WORKDIR /app

# Instala o SWI-Prolog (dependência do sistema)
RUN apt-get update && \
    apt-get install -y --no-install-recommends swi-prolog && \
    rm -rf /var/lib/apt/lists/*

# Cria um Virtual Environment (Boa Prática)
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Instala as dependências de PRODUÇÃO (do requirements.txt)
COPY requirements.txt .
RUN pip install --upgrade pip && \
    pip install -r requirements.txt

# Instala dependências de TESTE (para uso do builder)
RUN python -m venv /opt/testvenv \
 && /opt/testvenv/bin/pip install --upgrade pip \
 && /opt/testvenv/bin/pip install pytest asgi-lifespan mysql-connector-python

# --- Fase 2: Final (Produção) ---
FROM python:3.11-slim AS final

WORKDIR /app

# Instala APENAS o SWI-Prolog (dependência de sistema necessária)
RUN apt-get update && \
    apt-get install -y --no-install-recommends swi-prolog && \
    rm -rf /var/lib/apt/lists/*

# Copia APENAS o Virtual Environment (com as libs de produção) da fase 'builder'
COPY --from=builder /opt/venv /opt/venv

# Copia APENAS os ficheiros da nossa aplicação
COPY app/ ./app/
COPY backend/ ./backend/
COPY prolog/ ./prolog/
COPY frontend/ ./frontend/

# Define o PATH para usar o venv
ENV PATH="/opt/venv/bin:$PATH"

# Expõe a porta que o Uvicorn usará
EXPOSE 8000

# Define o novo CMD padrão para iniciar o servidor
CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000"]