import { Component, Input, AfterViewChecked, ElementRef, ViewChild, OnChanges, SimpleChanges, Output, EventEmitter } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Message } from '../../models/chat.model';
import { MessageBubbleComponent } from '../message-bubble/message-bubble.component';

@Component({
  selector: 'app-message-list',
  standalone: true,
  imports: [CommonModule, MessageBubbleComponent],
  templateUrl: './message-list.component.html',
  styleUrls: ['./message-list.component.css']
})
export class MessageListComponent implements AfterViewChecked, OnChanges {
  @Input() messages: Message[] = [];
  @Input() isTyping = false;
  @Output() exampleClick = new EventEmitter<string>();
  @ViewChild('chatLog') chatLog?: ElementRef;
  @ViewChild('bottomAnchor') bottomAnchor?: ElementRef;

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['messages'] || changes['isTyping']) {
      // Aguarda o DOM renderizar antes de rolar
      setTimeout(() => this.scrollToBottom(), 0);
    }
  }

  ngAfterViewChecked(): void {
    this.scrollToBottom();
  }

  trackByMessageId(index: number, message: Message): string {
    return message.id;
  }

  onExampleClick(text: string): void {
    this.exampleClick.emit(text);
  }

  private scrollToBottom(): void {
    if (this.chatLog) {
      const element = this.chatLog.nativeElement;
      element.scrollTo({ top: element.scrollHeight, behavior: 'smooth' });
      // Garantir que o último item fique visível mesmo que o contêiner não role
      if (this.bottomAnchor) {
        this.bottomAnchor.nativeElement.scrollIntoView({ behavior: 'smooth', block: 'end' });
      }
    }
  }
}
