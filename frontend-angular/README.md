# Netflix Prolog Chatbot - Angular Frontend

## 🚀 Setup e Desenvolvimento

### Pré-requisitos
- Node.js 18+ 
- npm ou yarn
- Docker (para produção)

### Instalação

```bash
cd frontend-angular
npm install
```

### Desenvolvimento Local

```bash
# Servir em modo desenvolvimento (http://localhost:4200)
npm start

# Build de desenvolvimento
npm run build

# Build de produção
npm run build -- --configuration production
```

### Executar com Docker

```bash
# Build e iniciar todos os serviços (incluindo Angular)
docker-compose -f docker-compose.angular.yml up --build

# Acessar:
# - Frontend Angular: http://localhost:4200
# - Frontend Vanilla (antigo): http://localhost:8080
# - Backend API: http://localhost:8000
```

## 📁 Estrutura do Projeto

```
frontend-angular/
├── src/
│   ├── app/
│   │   ├── components/
│   │   │   ├── header/              # Cabeçalho com tema toggle
│   │   │   ├── status-bar/          # Barra de status e latência
│   │   │   ├── quick-actions/       # Botões de ação rápida
│   │   │   ├── message-list/        # Lista de mensagens
│   │   │   ├── message-bubble/      # Bubble individual com markdown
│   │   │   └── input-area/          # Input com rate limit warning
│   │   ├── services/
│   │   │   ├── chat.service.ts      # Comunicação com API
│   │   │   ├── session.service.ts   # Gerenciamento de sessão
│   │   │   └── theme.service.ts     # Tema claro/escuro
│   │   ├── models/
│   │   │   └── chat.model.ts        # Tipos TypeScript
│   │   ├── app.component.*          # Componente raiz
│   │   └── ...
│   ├── styles.css                    # Estilos globais
│   ├── index.html
│   └── main.ts
├── angular.json
├── package.json
├── tsconfig.json
└── nginx.conf                        # Config nginx para produção
```

## 🎨 Features Implementadas

### Componentes
- ✅ **StatusBar**: Indicador de conexão e latência em tempo real
- ✅ **Header**: Logo, título e botões (tema + limpar chat)
- ✅ **QuickActions**: 4 botões de ação rápida personalizáveis
- ✅ **MessageList**: Lista de mensagens com scroll automático
- ✅ **MessageBubble**: Renderização de markdown, botão copiar
- ✅ **InputArea**: Input com rate limit countdown visual

### Serviços
- ✅ **ChatService**: HttpClient para API, tratamento de erros 429
- ✅ **SessionService**: localStorage para session_id persistente
- ✅ **ThemeService**: Toggle tema escuro/claro com RxJS

### Design System
- 🎨 **Metro Style**: Design inspirado em Windows Metro
- 🌓 **Dark Theme**: Tema escuro completo com transições suaves
- 📱 **Responsivo**: Layout adaptável (mobile-first)
- ⚡ **Animações**: fadeIn, slideIn, pulse com CSS/Angular animations

### Recursos Avançados
- 📝 **Markdown Rendering**: marked.js + DOMPurify
- 📋 **Copy to Clipboard**: Botão copiar em mensagens do bot
- ⏱️ **Rate Limit Warning**: Countdown visual com barra de progresso
- 🔄 **Auto Scroll**: Scroll automático para última mensagem
- 💾 **Persistência**: Sessão e tema salvos no localStorage

## 🐳 Docker

### Arquivos Docker
- `Dockerfile.angular`: Multi-stage build (Node + Nginx)
- `docker-compose.angular.yml`: Orquestração completa
- `nginx.conf`: Config SPA com fallback para index.html

### Build Manual

```bash
# Build da imagem
docker build -f Dockerfile.angular -t chatbot-angular .

# Executar container
docker run -p 4200:80 chatbot-angular
```

## 🔧 Configuração

### API URL
Editar `src/app/services/chat.service.ts`:

```typescript
private apiUrl = 'http://localhost:8000/chat';
```

### Theme Default
Editar `src/app/services/theme.service.ts`:

```typescript
const savedTheme = localStorage.getItem('theme') || 'dark'; // 'light' ou 'dark'
```

## 📊 Performance

- **Build Size**: ~500KB (gzip)
- **First Load**: <2s
- **Lazy Loading**: Componentes standalone
- **Tree Shaking**: Otimização automática do Angular

## 🎯 Próximos Passos (Opcional)

- [ ] Lazy loading de rotas (se adicionar mais páginas)
- [ ] PWA (Service Workers para offline)
- [ ] Testes unitários (Jasmine/Karma)
- [ ] E2E tests (Cypress/Playwright)
- [ ] i18n (internacionalização)
- [ ] Animations mais complexas
- [ ] WebSocket para chat em tempo real

## 🆚 Comparação com Vanilla JS

| Feature | Vanilla JS | Angular |
|---------|-----------|---------|
| Linhas de código | ~850 | ~1200 (+ types) |
| Build time | 0s | ~30s |
| Bundle size | ~50KB | ~500KB |
| Type safety | ❌ | ✅ |
| Manutenibilidade | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| Escalabilidade | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| Curva aprendizado | Fácil | Média |
| Ecossistema | Limitado | Rico |

## 📝 Notas

- Projeto usa **Standalone Components** (Angular 17+)
- Não precisa de `NgModule`
- Configuração minimalista
- Pronto para produção
