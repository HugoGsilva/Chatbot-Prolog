import { Component, Input } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-status-bar',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './status-bar.component.html',
  styleUrls: ['./status-bar.component.css']
})
export class StatusBarComponent {
  @Input() isConnected = false;
  @Input() latency = 0;

  get statusText(): string {
    return this.isConnected ? 'Conectado' : 'Conectando...';
  }

  get latencyText(): string {
    if (this.latency === 0) return '';
    return `${this.latency}ms`;
  }

  get latencyIcon(): string {
    if (this.latency < 500) return '⚡';
    if (this.latency < 1500) return '⏱️';
    return '🐌';
  }

  get latencyClass(): string {
    if (this.latency < 500) return 'fast';
    if (this.latency < 1500) return 'medium';
    return 'slow';
  }
}
