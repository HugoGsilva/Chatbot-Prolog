import { Component, Input, AfterViewChecked, ElementRef, ViewChild } from '@angular/core';
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
export class MessageListComponent implements AfterViewChecked {
  @Input() messages: Message[] = [];
  @ViewChild('chatLog') chatLog?: ElementRef;

  ngAfterViewChecked(): void {
    this.scrollToBottom();
  }

  trackByMessageId(index: number, message: Message): string {
    return message.id;
  }

  private scrollToBottom(): void {
    if (this.chatLog) {
      const element = this.chatLog.nativeElement;
      element.scrollTop = element.scrollHeight;
    }
  }
}
