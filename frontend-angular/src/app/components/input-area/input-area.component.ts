import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { ChatService } from '../../services/chat.service';
import { SessionService } from '../../services/session.service';

@Component({
  selector: 'app-input-area',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './input-area.component.html',
  styleUrls: ['./input-area.component.css']
})
export class InputAreaComponent {
  @Output() sendMessage = new EventEmitter<string>();
  
  userInput = '';
  isDisabled = false;
  rateLimitWarning: { active: boolean; seconds: number; intervalId?: any } = {
    active: false,
    seconds: 0
  };

  constructor(
    private chatService: ChatService,
    private sessionService: SessionService
  ) {}

  onSend(): void {
    const text = this.userInput.trim();
    if (!text || this.isDisabled) return;

    this.userInput = '';
    this.isDisabled = true;

    const sessionId = this.sessionService.getCurrentSessionId();
    this.chatService.sendMessage(text, sessionId).subscribe({
      next: ({ response }) => {
        this.sendMessage.emit(text);
        this.isDisabled = false;
      },
      error: (error) => {
        if (error.isRateLimit) {
          this.showRateLimitWarning(error.waitSeconds);
        } else {
          console.error('Error:', error);
          this.isDisabled = false;
        }
      }
    });
  }

  onKeyPress(event: KeyboardEvent): void {
    if (event.key === 'Enter' && !event.shiftKey) {
      event.preventDefault();
      this.onSend();
    }
  }

  private showRateLimitWarning(seconds: number): void {
    this.rateLimitWarning.active = true;
    this.rateLimitWarning.seconds = seconds;
    this.isDisabled = true;

    // Countdown
    this.rateLimitWarning.intervalId = setInterval(() => {
      this.rateLimitWarning.seconds--;
      
      if (this.rateLimitWarning.seconds <= 0) {
        this.hideRateLimitWarning();
      }
    }, 1000);
  }

  private hideRateLimitWarning(): void {
    if (this.rateLimitWarning.intervalId) {
      clearInterval(this.rateLimitWarning.intervalId);
    }
    this.rateLimitWarning.active = false;
    this.rateLimitWarning.seconds = 0;
    this.isDisabled = false;
  }
}
