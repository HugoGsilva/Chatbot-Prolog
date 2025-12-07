import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HeaderComponent } from './components/header/header.component';
import { StatusBarComponent } from './components/status-bar/status-bar.component';
import { QuickActionsComponent } from './components/quick-actions/quick-actions.component';
import { MessageListComponent } from './components/message-list/message-list.component';
import { InputAreaComponent } from './components/input-area/input-area.component';
import { ChatService } from './services/chat.service';
import { SessionService } from './services/session.service';
import { ThemeService } from './services/theme.service';
import { Message, ChatResponse } from './models/chat.model';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [
    CommonModule,
    HeaderComponent,
    StatusBarComponent,
    QuickActionsComponent,
    MessageListComponent,
    InputAreaComponent
  ],
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  messages: Message[] = [];
  isConnected = false;
  latency = 0;

  constructor(
    private chatService: ChatService,
    private sessionService: SessionService,
    private themeService: ThemeService
  ) {
    this.checkConnection();
    this.addWelcomeMessage();
  }

  checkConnection(): void {
    this.chatService.checkHealth().subscribe({
      next: () => {
        this.isConnected = true;
      },
      error: () => {
        this.isConnected = false;
        setTimeout(() => this.checkConnection(), 5000);
      }
    });
  }

  addWelcomeMessage(): void {
    const welcomeMessage: Message = {
      id: this.generateMessageId(),
      text: '',
      isBot: true,
      timestamp: new Date(),
      response: {
        type: 'text' as any,
        content: '👋 **Bem-vindo ao Netflix Prolog Assistant!**\n\n' +
                 'Estou aqui para ajudar você a explorar o catálogo Netflix. ' +
                 'Você pode:\n\n' +
                 '🎬 Buscar filmes por ator, diretor ou gênero\n' +
                 '🎲 Obter recomendações aleatórias\n' +
                 '📊 Ver detalhes e informações de filmes\n\n' +
                 'Digite **"ajuda"** para ver todos os comandos disponíveis!'
      }
    };
    this.messages.push(welcomeMessage);
  }

  onSendMessage(text: string): void {
    // Adicionar mensagem do usuário
    const userMessage: Message = {
      id: this.generateMessageId(),
      text,
      isBot: false,
      timestamp: new Date()
    };
    this.messages.push(userMessage);

    // Enviar para API
    const sessionId = this.sessionService.getCurrentSessionId();
    this.chatService.sendMessage(text, sessionId).subscribe({
      next: ({ response, latency }) => {
        this.latency = latency;
        const botMessage: Message = {
          id: this.generateMessageId(),
          text: '',
          isBot: true,
          timestamp: new Date(),
          response
        };
        this.messages.push(botMessage);
      },
      error: (error) => {
        // Erro será tratado no componente de input
        console.error('Error sending message:', error);
      }
    });
  }

  onQuickAction(query: string): void {
    this.onSendMessage(query);
  }

  onClearChat(): void {
    this.messages = [];
    this.sessionService.resetSession();
    this.addWelcomeMessage();
  }

  private generateMessageId(): string {
    return `msg_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
  }
}
