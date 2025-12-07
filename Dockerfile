# syntax=docker/dockerfile:1.7

############################################
# STAGE 1: Build Frontend Angular
############################################
FROM node:20-alpine AS frontend-builder

WORKDIR /frontend

# Copia arquivos de dependências
COPY frontend-angular/package.json frontend-angular/package-lock.json* ./

# Dependências para pacotes que usam node-gyp (compilação nativa)
RUN apk add --no-cache python3 make g++

# Instala dependências do frontend
RUN npm install --force || npm install --legacy-peer-deps

# Copia código fonte do frontend
COPY frontend-angular/ ./

# Build de produção (fallback para build padrão)
RUN npm run build -- --configuration production || npm run build

############################################
# STAGE 2: Build Backend Python
############################################
FROM python:3.11-slim AS backend-builder

WORKDIR /app

# Instala SWI-Prolog (necessário para backend) e ferramentas básicas
RUN apt-get update && \
    apt-get install -y --no-install-recommends swi-prolog && \
    rm -rf /var/lib/apt/lists/*

# Cria Virtualenv
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Instala dependências de produção do Python
COPY requirements.txt .
RUN --mount=type=cache,target=/root/.cache/pip \
    pip install --upgrade pip && \
    pip install -r requirements.txt

############################################
# STAGE 3: Final - Backend + Frontend via Nginx
############################################
FROM backend-builder AS final

WORKDIR /app

# Instala Nginx (SWI-Prolog e venv já vieram do backend-builder)
RUN apt-get update && \
    apt-get install -y --no-install-recommends nginx && \
    rm -rf /var/lib/apt/lists/*

# Copia código da aplicação backend e Prolog
COPY app/ ./app/
COPY prolog/ ./prolog/
COPY data_netflix/ ./data_netflix/

# Copia build do frontend para o diretório padrão do Nginx
COPY --from=frontend-builder /frontend/dist/netflix-prolog-chatbot /usr/share/nginx/html

# Copia configuração do Nginx (substitui o default)
COPY frontend-angular/nginx.conf /etc/nginx/sites-available/default

# Garante que o venv está no PATH (herdado, mas reforçamos)
ENV PATH="/opt/venv/bin:$PATH"

# Script de inicialização para subir Nginx + Uvicorn
RUN echo '#!/bin/bash\n\
set -e\n\
nginx\n\
exec uvicorn app.main:app --host 0.0.0.0 --port 8000\n\
' > /start.sh && chmod +x /start.sh

# Exposição de portas
EXPOSE 80 8000

# Comando padrão
CMD ["/start.sh"]
