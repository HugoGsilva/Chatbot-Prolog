import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';

interface QuickAction {
  icon: string;
  label: string;
  query: string;
}

@Component({
  selector: 'app-quick-actions',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './quick-actions.component.html',
  styleUrls: ['./quick-actions.component.css']
})
export class QuickActionsComponent {
  @Output() actionClick = new EventEmitter<string>();

  actions: QuickAction[] = [
    { icon: '🎲', label: 'Filme Aleatório', query: 'filme aleatório' },
    { icon: '💥', label: 'Filmes de Ação', query: 'filmes de ação' },
    { icon: '😂', label: 'Comédias', query: 'filmes de comédia' },
    { icon: '💡', label: 'Ajuda', query: 'ajuda' }
  ];

  onActionClick(query: string): void {
    this.actionClick.emit(query);
  }
}
