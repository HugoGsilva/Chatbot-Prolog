# Auditoria de Segurança - Chatbot Netflix-Prolog

## Data: Fase 5 - Thin Client Migration

## Resumo

A aplicação foi auditada para verificar os principais vetores de segurança em aplicações web.

---

## 1. Input Validation ✅

### ChatRequest (app/schemas.py)
- **message**: 1-1000 caracteres, strip aplicado
- **session_id**: 1-100 caracteres, validação regex `^[a-zA-Z0-9_-]+$`

### Validações implementadas:
```python
@field_validator('message')
def validate_message(cls, v: str) -> str:
    v = v.strip()
    if not v:
        raise ValueError("Mensagem não pode ser vazia")
    return v

@field_validator('session_id')
def validate_session_id(cls, v: str) -> str:
    # Aceita apenas alfanuméricos, underscores e hífens
    if not re.match(r'^[a-zA-Z0-9_-]+$', v):
        raise ValueError("session_id contém caracteres inválidos")
    return v
```

---

## 2. Rate Limiting ✅

### Limites implementados (app/rate_limiter.py):
- **Por IP**: 20 requisições/minuto
- **Por Sessão**: 10 requisições/minuto
- **Janela**: Sliding window de 60 segundos

### Mecanismo:
- Armazenamento em Redis com TTL automático
- Resposta HTTP 429 com header `Retry-After: 60`

### Testes:
- ✅ Cypress: `performance_tests.cy.js` - Testa que 429 é retornado quando limite excedido

---

## 3. XSS Protection ✅

### Frontend (frontend/main.js):

**Funções de sanitização implementadas:**

```javascript
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

function escapeAttr(text) {
    return text.replace(/"/g, '&quot;').replace(/'/g, '&#39;');
}
```

**Uso consistente:**
- Todos os dados de usuário passam por `escapeHtml()` antes de renderização
- Atributos `data-*` usam `escapeAttr()` para prevenir injeção
- Strings estáticas são construídas com templates, não concatenação
- `textContent` usado onde apropriado para conteúdo puramente texto

---

## 4. Session Security ✅

### Implementação (app/session_manager.py):
- **TTL**: 24 horas (86400 segundos)
- **ID Format**: UUID v4 gerado pelo servidor
- **Storage**: Redis com prefixo `session:`
- **Auto-cleanup**: Expira automaticamente após TTL

---

## 5. Headers de Segurança

### Recomendações para Produção:

Adicionar ao FastAPI ou proxy reverso:

```python
# Exemplo para FastAPI
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from starlette.middleware.cors import CORSMiddleware

app.add_middleware(
    CORSMiddleware,
    allow_origins=["https://seu-dominio.com"],
    allow_methods=["POST", "GET", "DELETE"],
)

# Ou via nginx/proxy:
# X-Content-Type-Options: nosniff
# X-Frame-Options: DENY
# Content-Security-Policy: default-src 'self'
```

---

## 6. Vulnerabilidades Mitigadas

| Vulnerabilidade | Status | Mitigação |
|----------------|--------|-----------|
| XSS (Reflected) | ✅ Mitigado | escapeHtml() em todas as saídas |
| XSS (Stored) | ✅ Mitigado | Histórico não renderiza HTML raw |
| SQL Injection | ✅ N/A | Não usa SQL, apenas Prolog queries |
| Prolog Injection | ✅ Mitigado | Entidades validadas contra caches |
| DoS | ✅ Mitigado | Rate limiting por IP e sessão |
| Session Hijacking | ⚠️ Parcial | UUID seguro, mas sem HTTPS obrigatório |

---

## 7. Recomendações para Produção

1. **HTTPS obrigatório** - Usar certificado SSL/TLS
2. **CSP Headers** - Content-Security-Policy restritivo
3. **CORS configurado** - Limitar origens permitidas
4. **Logs de auditoria** - Já implementado com logging estruturado
5. **Monitoramento** - Integrar com sistema de alertas

---

## Conclusão

A aplicação implementa os controles de segurança essenciais para uma aplicação de chat web:
- Input validation completo
- Rate limiting funcional
- XSS protection consistente
- Session management seguro

O código está pronto para deploy em ambiente controlado. Para produção pública, 
adicionar HTTPS e headers de segurança adicionais.
