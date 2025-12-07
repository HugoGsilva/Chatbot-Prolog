import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class SessionService {
  private sessionId: string;
  private sessionIdSubject: BehaviorSubject<string>;

  constructor() {
    this.sessionId = this.getOrCreateSessionId();
    this.sessionIdSubject = new BehaviorSubject<string>(this.sessionId);
  }

  getSessionId(): Observable<string> {
    return this.sessionIdSubject.asObservable();
  }

  getCurrentSessionId(): string {
    return this.sessionId;
  }

  resetSession(): void {
    this.sessionId = this.generateSessionId();
    localStorage.setItem('chatbot_session_id', this.sessionId);
    this.sessionIdSubject.next(this.sessionId);
  }

  private getOrCreateSessionId(): string {
    let sessionId = localStorage.getItem('chatbot_session_id');
    
    if (!sessionId) {
      sessionId = this.generateSessionId();
      localStorage.setItem('chatbot_session_id', sessionId);
    }
    
    return sessionId;
  }

  private generateSessionId(): string {
    return `session_${Date.now()}_${Math.random().toString(36).substring(2, 15)}`;
  }
}
