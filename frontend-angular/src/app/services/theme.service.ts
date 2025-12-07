import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class ThemeService {
  private darkThemeSubject: BehaviorSubject<boolean>;
  
  constructor() {
    const savedTheme = localStorage.getItem('theme') || 'light';
    const isDark = savedTheme === 'dark';
    this.darkThemeSubject = new BehaviorSubject<boolean>(isDark);
    
    // Aplicar tema inicial
    if (isDark) {
      document.body.classList.add('dark-theme');
    }
  }

  isDarkTheme(): Observable<boolean> {
    return this.darkThemeSubject.asObservable();
  }

  toggleTheme(): void {
    const isDark = !this.darkThemeSubject.value;
    this.darkThemeSubject.next(isDark);
    
    if (isDark) {
      document.body.classList.add('dark-theme');
      localStorage.setItem('theme', 'dark');
    } else {
      document.body.classList.remove('dark-theme');
      localStorage.setItem('theme', 'light');
    }
  }

  getCurrentTheme(): boolean {
    return this.darkThemeSubject.value;
  }
}
