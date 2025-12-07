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
          const listContent = rawContent as any[];
          textContent = this.formatListContent(listContent);
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

  private formatListContent(items: any[]): string {
    if (!items || items.length === 0) {
      return '**Nenhum resultado encontrado.**';
    }
    
    let md = `### 🎬 Encontrei ${items.length} ${items.length === 1 ? 'filme' : 'filmes'}:\n\n`;
    
    // Limitar a 50 filmes para não sobrecarregar a UI
    const displayItems = items.slice(0, 50);
    
    for (const item of displayItems) {
      if (item.titulo) {
        md += `- **${item.titulo}**`;
        if (item.ano) md += ` (${item.ano})`;
        if (item.diretor) md += ` - Dir: ${item.diretor}`;
        md += '\n';
      }
    }
    
    if (items.length > 50) {
      md += `\n*... e mais ${items.length - 50} filmes*`;
    }
    
    return md;
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
