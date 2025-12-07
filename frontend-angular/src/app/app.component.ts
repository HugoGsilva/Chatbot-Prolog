import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HeaderComponent } from './components/header/header.component';
import { StatusBarComponent } from './components/status-bar/status-bar.component';
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
  isTyping = false;
  showHelpMenu = false;

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

    // Ativar indicador de digitação
    this.isTyping = true;

    // Enviar para API
    const sessionId = this.sessionService.getCurrentSessionId();
    this.chatService.sendMessage(text, sessionId).subscribe({
      next: ({ response, latency }) => {
        this.latency = latency;
        this.isTyping = false;
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
        this.isTyping = false;
        console.error('Error sending message:', error);
      }
    });
  }

  onClearChat(): void {
    this.messages = [];
    this.sessionService.resetSession();
    this.addWelcomeMessage();
  }

  toggleHelpMenu(): void {
    this.showHelpMenu = !this.showHelpMenu;
  }

  closeHelpMenu(): void {
    this.showHelpMenu = false;
  }

  onHelpOption(option: string): void {
    this.closeHelpMenu();

    const devFeatures = ['top_rated'];

    if (devFeatures.includes(option)) {
      const devMessage: Message = {
        id: this.generateMessageId(),
        text: '',
        isBot: true,
        timestamp: new Date(),
        response: {
          type: 'text' as any,
          content: '⚠️ **Funcionalidade em desenvolvimento**\n\n' +
                   'Esta opção estará disponível em breve. ' +
                   'Por enquanto, experimente perguntar sobre gêneros, atores ou diretores!'
        }
      };
      this.messages.push(devMessage);
      return;
    }

    if (option === 'como_usar') {
      // Enviar comando direto sem quebras estranhas
      this.onSendMessage('ajuda');
    }
  }

  private generateMessageId(): string {
    return `msg_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
  }
}
