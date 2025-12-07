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
  helpContent: { title: string; intro: string; sections: { title: string; examples: string[] }[] } | null = null;

  constructor(private sanitizer: DomSanitizer) {}

  ngOnInit(): void {
    this.renderContent();
  }

  async renderContent(): Promise<void> {
    const rawContent = this.message.response?.content || this.message.text || '';
    const responseType = this.message.response?.type;

    // Reset derived state before rendering the new payload
    this.helpContent = null;
    this.listItems = [];
    this.visibleListCount = 3;
    
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
          // Renderizar ajuda com layout próprio quando o conteúdo é um objeto válido
          if (typeof rawContent === 'object' && rawContent !== null && !Array.isArray(rawContent)) {
            this.helpContent = this.buildHelpContent(rawContent);
            this.renderedContent = '';
            return;
          }
          textContent = this.formatHelpContent(rawContent);
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

  isHelpResponse(): boolean {
    return Boolean(this.helpContent);
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

  private buildHelpContent(help: any): { title: string; intro: string; sections: { title: string; examples: string[] }[] } {
    const examples = (help?.examples && typeof help.examples === 'object') ? help.examples : {};
    const sections = Object.entries(examples)
      .map(([title, values]) => ({ title, examples: Array.isArray(values) ? values : [] }))
      .filter(section => section.examples.length > 0);

    return {
      title: help?.message || 'Como usar o bot',
      intro: 'Peça filmes por gênero, ator ou diretor. Clique em um exemplo para copiar e mandar rápido.',
      sections
    };
  }

  private formatHelpContent(help: any): string {
    if (typeof help === 'string') {
      return help;
    }

    const message = (help && typeof help === 'object' && help.message) ? help.message : 'Como usar o bot';
    const examples = (help && typeof help === 'object' && help.examples) ? help.examples : null;

    let md = `### ${message}\n\n`;
    
    if (examples && typeof examples === 'object') {
      md += '**Exemplos de comandos:**\n\n';
      for (const [category, samples] of Object.entries(examples as Record<string, string[]>)) {
        md += `**${category}:**\n`;
        for (const example of samples) {
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
