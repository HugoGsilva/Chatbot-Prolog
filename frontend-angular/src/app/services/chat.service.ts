import { Injectable } from '@angular/core';
import { HttpClient, HttpErrorResponse, HttpHeaders } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { catchError, map } from 'rxjs/operators';
import { ChatResponse, RateLimitError } from '../models/chat.model';

@Injectable({
  providedIn: 'root'
})
export class ChatService {
  private apiUrl = '/chat';  // Usa proxy do nginx
  private healthUrl = '/health';  // Usa proxy do nginx

  constructor(private http: HttpClient) {}

  sendMessage(message: string, sessionId: string): Observable<{ response: ChatResponse, latency: number }> {
    const startTime = performance.now();
    
    const headers = new HttpHeaders({
      'Content-Type': 'application/json'
    });

    const body = {
      message: message,
      session_id: sessionId
    };

    return this.http.post<ChatResponse>(this.apiUrl, body, { headers }).pipe(
      map(response => ({
        response,
        latency: Math.round(performance.now() - startTime)
      })),
      catchError(this.handleError)
    );
  }

  checkHealth(): Observable<any> {
    return this.http.get(this.healthUrl).pipe(
      catchError(() => throwError(() => new Error('API offline')))
    );
  }

  private handleError(error: HttpErrorResponse): Observable<never> {
    if (error.status === 429) {
      // Rate limit error
      const retryAfter = error.headers.get('Retry-After');
      const waitSeconds = retryAfter ? parseInt(retryAfter, 10) : 60;
      
      const rateLimitError: RateLimitError = {
        isRateLimit: true,
        waitSeconds,
        message: error.error?.detail || 'Muitas requisições. Aguarde um momento.'
      };
      
      return throwError(() => rateLimitError);
    }

    // Other errors
    const errorMessage = error.error?.detail || error.message || 'Erro ao processar mensagem';
    return throwError(() => new Error(errorMessage));
  }
}
