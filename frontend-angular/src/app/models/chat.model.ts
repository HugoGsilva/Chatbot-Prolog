export enum ResponseType {
  TEXT = 'text',
  LIST = 'list',
  TABLE = 'table',
  ERROR = 'error'
}

export interface ChatResponse {
  type: ResponseType;
  content: string;
  suggestions?: string[];
  metadata?: {
    query?: string;
    results_count?: number;
    execution_time?: number;
  };
}

export interface Message {
  id: string;
  text: string;
  isBot: boolean;
  timestamp: Date;
  response?: ChatResponse;
}

export interface RateLimitError {
  isRateLimit: boolean;
  waitSeconds: number;
  message: string;
}
