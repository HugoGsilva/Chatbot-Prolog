import { Component, Input, OnInit, SecurityContext } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import { Message } from '../../models/chat.model';
import { marked } from 'marked';
import DOMPurify from 'dompurify';

@Component({
  selector: 'app-message-bubble',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './message-bubble.component.html',
  styleUrls: ['./message-bubble.component.css']
})
export class MessageBubbleComponent implements OnInit {
  @Input() message!: Message;
  renderedContent: SafeHtml = '';

  constructor(private sanitizer: DomSanitizer) {}

  ngOnInit(): void {
    this.renderContent();
  }

  async renderContent(): Promise<void> {
    const content = this.message.response?.content || this.message.text;
    
    if (this.message.isBot) {
      // Renderizar markdown para mensagens do bot
      const htmlContent = await marked.parse(content);
      const cleanHtml = DOMPurify.sanitize(htmlContent);
      this.renderedContent = this.sanitizer.sanitize(SecurityContext.HTML, cleanHtml) || '';
    } else {
      // Texto simples para mensagens do usuário
      this.renderedContent = content;
    }
  }

  copyToClipboard(text: string): void {
    if (navigator.clipboard) {
      navigator.clipboard.writeText(text).then(
        () => console.log('Texto copiado!'),
        (err) => console.error('Erro ao copiar:', err)
      );
    }
  }

  getContentText(): string {
    return this.message.response?.content || this.message.text;
  }
}
