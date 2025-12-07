export enum ResponseType {
  TEXT = 'text',
  LIST = 'list',
  TABLE = 'table',
  ERROR = 'error',
  HELP = 'help'
}

export interface HelpContent {
  message: string;
  examples: {
    [key: string]: string[];
  };
}

export interface ListItem {
  titulo: string;
  [key: string]: any;
}

export interface ChatResponse {
  type: ResponseType | string;
  content: string | HelpContent | ListItem[] | any;
  suggestions?: string[];
  metadata?: {
    query?: string;
    results_count?: number;
    execution_time?: number;
    intent?: string;
    confidence?: number;
    entities?: any;
    corrected_text?: string | null;
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
