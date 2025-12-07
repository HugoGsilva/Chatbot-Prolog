import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';

interface QuickAction {
  icon: string;
  label: string;
  query: string;
  type: 'query' | 'help';
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
  @Output() toggleHelp = new EventEmitter<void>();

  actions: QuickAction[] = [
    { icon: '🔀', label: 'Filme Aleatório', query: 'filme aleatório', type: 'query' },
    { icon: '⚔️', label: 'Filmes de Ação', query: 'filmes de ação', type: 'query' },
    { icon: '😂', label: 'Comédias', query: 'filmes de comédia', type: 'query' },
    { icon: '❓', label: 'Ajuda', query: '', type: 'help' }
  ];

  onActionClick(action: QuickAction): void {
    if (action.type === 'help') {
      this.toggleHelp.emit();
    } else {
      this.actionClick.emit(action.query);
    }
  }
}
