import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-header',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.css']
})
export class HeaderComponent {
  @Output() clearChat = new EventEmitter<void>();
  @Output() toggleHelp = new EventEmitter<void>();

  onClearClick(): void {
    this.clearChat.emit();
  }

  onHelpClick(): void {
    this.toggleHelp.emit();
  }
}
