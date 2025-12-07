import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ThemeService } from '../../services/theme.service';

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
  isDarkTheme = false;

  constructor(private themeService: ThemeService) {
    this.themeService.isDarkTheme().subscribe(isDark => {
      this.isDarkTheme = isDark;
    });
  }

  onClearClick(): void {
    this.clearChat.emit();
  }

  onToggleTheme(): void {
    this.themeService.toggleTheme();
  }

  onHelpClick(): void {
    this.toggleHelp.emit();
  }
}
