# multi-stage Dockerfile para construir Backend (FastAPI + Prolog) e Frontend (Angular)

# ============================================
# STAGE 1: Build Frontend Angular
# ============================================
FROM node:20-alpine AS frontend-builder

WORKDIR /frontend

# Copiar package files
COPY frontend-angular/package.json frontend-angular/package-lock.json* ./

# Instalar dependências
RUN npm install --force || npm install --legacy-peer-deps

# Copiar código fonte
COPY frontend-angular/ ./

# Build produção
RUN npm run build -- --configuration production || npm run build

# ============================================
# STAGE 2: Build Backend Python
# ============================================
FROM python:3.11-slim AS backend-builder

WORKDIR /app

# Instala o SWI-Prolog (dependência do sistema)
RUN apt-get update && \
    apt-get install -y --no-install-recommends swi-prolog && \
    rm -rf /var/lib/apt/lists/*

# Cria um Virtual Environment
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Instala as dependências de PRODUÇÃO
COPY requirements.txt .
RUN pip install --upgrade pip && \
    pip install -r requirements.txt

# Instala modelo spaCy português para NLU
RUN python -m spacy download pt_core_news_sm

# ============================================
# STAGE 3: Final - Backend + Frontend servido via Nginx
# ============================================
FROM python:3.11-slim AS final

WORKDIR /app

# Instala SWI-Prolog e Nginx
RUN apt-get update && \
    apt-get install -y --no-install-recommends swi-prolog nginx && \
    rm -rf /var/lib/apt/lists/*

# Copia Virtual Environment do backend
COPY --from=backend-builder /opt/venv /opt/venv

# Copia código da aplicação
COPY app/ ./app/
COPY prolog/ ./prolog/

# Copia frontend buildado para Nginx
COPY --from=frontend-builder /frontend/dist/netflix-prolog-chatbot /usr/share/nginx/html
COPY frontend-angular/nginx.conf /etc/nginx/sites-available/default

# Define o PATH para usar o venv
ENV PATH="/opt/venv/bin:$PATH"

# Cria script de inicialização para rodar Backend e Nginx
RUN echo '#!/bin/bash\n\
nginx\n\
exec uvicorn app.main:app --host 0.0.0.0 --port 8000\n\
' > /start.sh && chmod +x /start.sh

# Expõe portas
EXPOSE 80 8000

# Inicia ambos os serviços
CMD ["/start.sh"]