import { Component, Input, OnInit } from '@angular/core';
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
  listItems: any[] = [];
  visibleListCount = 3;

  constructor(private sanitizer: DomSanitizer) {}

  ngOnInit(): void {
    this.renderContent();
  }

  async renderContent(): Promise<void> {
    const rawContent = this.message.response?.content || this.message.text || '';
    const responseType = this.message.response?.type;
    
    if (!rawContent) {
      console.warn('Empty content for message:', this.message);
      this.renderedContent = '';
      return;
    }
    
    if (this.message.isBot) {
      try {
        let textContent = '';
        
        // Converter diferentes tipos de conteúdo em string
        if (responseType === 'help') {
          // Conteúdo de ajuda é um objeto
          const helpContent = rawContent as any;
          textContent = this.formatHelpContent(helpContent);
        } else if (responseType === 'list') {
          // Conteúdo de lista é um array de objetos
          const listContent = (rawContent as any[]) || [];
          this.prepareListContent(listContent);
          return; // lista renderizada separadamente
        } else if (typeof rawContent === 'string') {
          // Conteúdo já é string
          textContent = rawContent;
        } else {
          // Fallback: converter para JSON formatado
          textContent = '```json\n' + JSON.stringify(rawContent, null, 2) + '\n```';
        }
        
        // Renderizar markdown
        const htmlContent = await marked.parse(textContent);
        const cleanHtml = DOMPurify.sanitize(htmlContent);
        this.renderedContent = this.sanitizer.bypassSecurityTrustHtml(cleanHtml);
      } catch (error) {
        console.error('Error rendering markdown:', error);
        this.renderedContent = this.sanitizer.bypassSecurityTrustHtml(String(rawContent));
      }
    } else {
      // Texto simples para mensagens do usuário (escapar HTML)
      const escaped = String(rawContent).replace(/</g, '&lt;').replace(/>/g, '&gt;');
      this.renderedContent = this.sanitizer.bypassSecurityTrustHtml(escaped);
    }
  }

  private prepareListContent(items: any[]): void {
    this.listItems = items || [];
    this.visibleListCount = Math.min(3, this.listItems.length);
    this.renderedContent = '';
  }

  isListResponse(): boolean {
    return this.message.response?.type === 'list';
  }

  get visibleListItems(): any[] {
    return this.listItems.slice(0, this.visibleListCount);
  }

  get hasMoreItems(): boolean {
    return this.listItems.length > this.visibleListCount;
  }

  showMoreFilms(): void {
    this.visibleListCount = Math.min(this.listItems.length, this.visibleListCount + 10);
  }

  get displayCount(): number {
    return Math.min(this.visibleListCount, this.listItems.length);
  }

  getItemMeta(item: any): string | null {
    const parts: string[] = [];
    if (item.ano) {
      parts.push(String(item.ano));
    }
    if (item.genero) {
      parts.push(item.genero);
    }
    if (item.diretor) {
      parts.push(`Dir: ${item.diretor}`);
    }
    return parts.length ? parts.join(' • ') : null;
  }

  getItemDescription(item: any): string | null {
    return item.sinopse || item.descricao || item.descricao_curta || null;
  }

  private formatHelpContent(help: any): string {
    let md = `### ${help.message}\n\n`;
    
    if (help.examples) {
      md += '**Exemplos de comandos:**\n\n';
      for (const [category, examples] of Object.entries(help.examples)) {
        md += `**${category}:**\n`;
        for (const example of examples as string[]) {
          md += `- ${example}\n`;
        }
        md += '\n';
      }
    }
    
    return md;
  }

  getContentText(): string | undefined {
    const content = this.message.response?.content;
    if (typeof content === 'string') {
      return content;
    }
    if (typeof this.message.text === 'string') {
      return this.message.text;
    }
    return undefined;
  }

  copyToClipboard(text?: string): void {
    if (!text) {
      return;
    }
    navigator.clipboard?.writeText(text).catch((err) => console.error('Erro ao copiar:', err));
  }
}
