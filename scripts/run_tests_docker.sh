#!/bin/bash
# Script para rodar testes dentro do container Docker
# 
# Uso:
#   docker-compose exec app bash /app/scripts/run_tests_docker.sh
#   ou
#   docker exec -it <container_id> bash /app/scripts/run_tests_docker.sh

set -e

echo "=========================================="
echo "RODANDO TESTES DENTRO DO DOCKER"
echo "=========================================="

# Cores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 1. Verificar dependências
echo -e "\n${YELLOW}[1/5] Verificando dependências...${NC}"
python -c "import sentence_transformers; print('✅ sentence-transformers instalado')"
python -c "import torch; print('✅ torch instalado')"
python -c "import spacy; print('✅ spacy instalado')"

# 2. Verificar modelo spaCy
echo -e "\n${YELLOW}[2/5] Verificando modelo spaCy...${NC}"
python -m spacy validate | grep pt_core_news_sm || python -m spacy download pt_core_news_sm

# 3. Verificar setup semântico
echo -e "\n${YELLOW}[3/5] Verificando setup semântico...${NC}"
if [ -f "/app/scripts/verify_semantic_setup.py" ]; then
    python /app/scripts/verify_semantic_setup.py
else
    echo "⚠️  Script de verificação não encontrado, continuando..."
fi

# 4. Rodar testes Step 1 e 2
echo -e "\n${YELLOW}[4/5] Rodando testes Steps 1-2...${NC}"
pytest tests/test_semantic_classifier.py -v || echo -e "${RED}❌ Falha em test_semantic_classifier.py${NC}"
pytest tests/test_prolog_normalizer.py -v || echo -e "${RED}❌ Falha em test_prolog_normalizer.py${NC}"
pytest tests/test_augment_intent_data.py -v || echo -e "${RED}❌ Falha em test_augment_intent_data.py${NC}"

# 5. Rodar testes Step 3 (NLUEngine semântico)
echo -e "\n${YELLOW}[5/5] Rodando testes Step 3 (NLUEngine semântico)...${NC}"
pytest tests/test_nlu_engine_semantic.py -v || echo -e "${RED}❌ Falha em test_nlu_engine_semantic.py${NC}"

echo -e "\n=========================================="
echo -e "${GREEN}✅ TESTES CONCLUÍDOS!${NC}"
echo "=========================================="

# Mostrar sumário
echo -e "\nPara ver relatório detalhado:"
echo "  pytest tests/ -v --tb=short"
echo ""
echo "Para rodar teste específico:"
echo "  pytest tests/test_nlu_engine_semantic.py::TestHighConfidenceSemantic -v"
