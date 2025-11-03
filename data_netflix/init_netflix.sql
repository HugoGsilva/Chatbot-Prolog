-- Remove tabelas antigas, se existirem, para permitir re-execução
DROP TABLE IF EXISTS netflix_titles;

-- Tabela principal de Títulos (baseado no schema fornecido)
CREATE TABLE netflix_titles (
    show_id VARCHAR(10) PRIMARY KEY,
    type VARCHAR(10),
    title TEXT,
    director TEXT,
    cast TEXT,        -- Importamos como TEXTO, vamos processar no Python (Fase 1.4)
    country TEXT,
    date_added VARCHAR(20),
    release_year INT,
    rating VARCHAR(10),
    duration VARCHAR(20),
    listed_in TEXT,   -- (Géneros) Importamos como TEXTO, vamos processar no Python
    description TEXT
);