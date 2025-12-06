"""
ETL pipeline para importar dados Netflix -> MySQL, gerar factos Prolog
e pré-carregar caches NLU no Redis.
"""

import time
import os
import sys
import pandas as pd
import redis
import json
from sqlalchemy import create_engine, text
from sqlalchemy.exc import OperationalError

# --- Configuração ---
DB_USER = "root"
DB_PASS = "root"
DB_HOST = "mysql" # O nome do serviço no docker-compose
DB_NAME = "moviedb"
REDIS_HOST = "redis" # O nome do serviço no docker-compose
REDIS_PORT = 6379

CSV_PATH = "/app/data_netflix/netflix_titles.csv"
SQL_INIT_PATH = "/app/data_netflix/init_netflix.sql"
PROLOG_FACTS_PATH = "/app/prolog/knowledge/imdb_kb.pl" # (O volume montado)


def wait_for_services(retries=60, delay=5):
    """
    Espera que MySQL e Redis fiquem disponíveis e devolve engine Redis.
    Propósito: Garantir que serviços dependentes estejam prontos antes do ETL.
    """
    print("[Pipeline] A aguardar pelos serviços (MySQL, Redis)...")

    # Esperar pelo MySQL
    db_ready = False
    db_engine = create_engine(f"mysql+mysqlconnector://{DB_USER}:{DB_PASS}@{DB_HOST}/{DB_NAME}")
    for _ in range(retries):
        try:
            db_engine.connect()
            db_ready = True
            print("[Pipeline] MySQL está PRONTO.")
            break
        except OperationalError:
            print("[Pipeline] MySQL ainda não está pronto, a aguardar...")
            time.sleep(delay)

    # Esperar pelo Redis
    redis_ready = False
    r = redis.Redis(host=REDIS_HOST, port=REDIS_PORT)
    for _ in range(retries):
        try:
            r.ping()
            redis_ready = True
            print("[Pipeline] Redis está PRONTO.")
            break
        except Exception:
            print("[Pipeline] Redis ainda não está pronto, a aguardar...")
            time.sleep(delay)

    if not db_ready or not redis_ready:
        print("[Pipeline] [ERRO] Serviços não arrancaram. A abortar.")
        sys.exit(1)

    return db_engine, r

def execute_sql_script(engine, script_path):
    """
    Executa um script SQL para criar/atualizar o esquema no MySQL.
    Propósito: Preparar tabelas necessárias para o carregamento.
    """
    print(f"[Pipeline] A executar script SQL: {script_path}")
    with open(script_path, 'r') as f:
        sql_script = f.read()

    with engine.connect() as connection:
        # O SQLAlchemy não gosta de múltiplos comandos. Executamos um a um.
        for statement in sql_script.split(';'):
            if statement.strip():
                connection.execute(text(statement))
    print("[Pipeline] Tabelas SQL criadas.")

def load_csv_to_mysql(engine, csv_path):
    """
    Lê CSV sujo, processa colunas essenciais e carrega para a tabela netflix_titles.
    Propósito: Popular a base MySQL com dados brutos do dataset.
    """
    print(f"[Pipeline] A ler CSV: {csv_path}")
    try:
        df = pd.read_csv(csv_path)

        # Limpeza básica (essencial)
        df = df.dropna(subset=['show_id', 'title', 'listed_in', 'cast'])
        df = df.where(pd.notnull(df), None) # Substitui NaN por None (que o SQL entende como NULL)

        # Manter apenas as colunas que definimos no init_netflix.sql
        df_filtered = df[[
            'show_id', 'type', 'title', 'director', 'cast', 'country',
            'date_added', 'release_year', 'rating', 'duration', 
            'listed_in', 'description'
        ]]

        print("[Pipeline] A carregar dados CSV para o MySQL (pode demorar)...")
        df_filtered.to_sql('netflix_titles', con=engine, if_exists='replace', index=False)
        print("[Pipeline] Carregamento do CSV para o MySQL concluído.")

    except Exception as e:
        print(f"[Pipeline] [ERRO] Falha ao ler ou carregar o CSV: {e}")
        sys.exit(1)

def generate_prolog_and_caches(engine, redis_client, prolog_path):
    """
    Gera o ficheiro de factos Prolog a partir do conteúdo da DB
    e guarda caches NLU (atores, géneros, filmes, diretores) no Redis.
    Propósito: Produzir o KB usado pelo motor Prolog e acelerar NLU.
    """
    print("[Pipeline] A gerar factos Prolog e caches NLU...")

    def quote_sql(value: str) -> str:
        """Escapa aspas simples para uso em factos Prolog/SQL-like."""
        safe = str(value).replace("'", "''")
        return f"'{safe}'"

    all_facts = []
    all_titles = set()
    all_genres = set()
    all_actors = set()
    # [V4.13] Novo conjunto para Diretores
    all_directors = set()

    # Usar Pandas para ler do SQL é mais fácil
    df_full = pd.read_sql("SELECT * FROM netflix_titles", engine)

    for _, row in df_full.iterrows():
        show_id_pl = quote_sql(row['show_id'])
        title_pl = quote_sql(row['title'])
        type_pl = quote_sql(row['type'])  # Movie ou TV Show

        # 1. Factos de Títulos (agora com type)
        all_facts.append(f"netflix_title({show_id_pl}, {title_pl}, {row['release_year']}, {type_pl}).")
        all_titles.add(row['title'].upper())

        # 2. Factos de Género (listed_in)
        if row['listed_in']:
            genres = [g.strip().upper() for g in row['listed_in'].split(',')]
            for genre in genres:
                if genre:
                    genre_pl = quote_sql(genre)
                    all_facts.append(f"netflix_genre({show_id_pl}, {genre_pl}).")
                    all_genres.add(genre)

        # 3. Factos de Atores (cast)
        if row['cast']:
            actors = [a.strip().upper() for a in row['cast'].split(',')]
            for actor in actors:
                if actor:
                    actor_pl = quote_sql(actor)
                    all_facts.append(f"netflix_actor({show_id_pl}, {actor_pl}).")
                    all_actors.add(actor)

        # 4. Factos de Diretor (director)
        # Alguns registos têm múltiplos diretores separados por vírgula
        if row.get('director') and pd.notna(row['director']):
            directors = [d.strip().upper() for d in str(row['director']).split(',')]
            for director in directors:
                if director:
                    director_pl = quote_sql(director)
                    all_facts.append(f"netflix_director({show_id_pl}, {director_pl}).")
                    all_directors.add(director)

    # Escrever o ficheiro Prolog
    print(f"[Pipeline] A escrever {len(all_facts)} factos em {prolog_path}...")
    with open(prolog_path, 'w', encoding='utf-8') as f:
        # [V4.13] Exporta também netflix_director/2 no módulo
        f.write(":- module(imdb_kb, [netflix_title/4, netflix_genre/2, netflix_actor/2, netflix_director/2]).\n")
        # [V4.13] Declara predicados como discontiguous para facilitar geração por blocos
        f.write(":- discontiguous(netflix_title/4).\n")
        f.write(":- discontiguous(netflix_genre/2).\n")
        f.write(":- discontiguous(netflix_actor/2).\n")
        f.write(":- discontiguous(netflix_director/2).\n\n")
        f.write("\n".join(all_facts))
    print("[Pipeline] Ficheiro de factos Prolog gerado.")

    # [Rec. 1] Gerar e guardar Caches NLU no Redis
    print("[Pipeline] A guardar Caches NLU no Redis...")
    redis_client.set("nlu_actors_cache", json.dumps(list(all_actors)))
    redis_client.set("nlu_genres_cache", json.dumps(list(all_genres)))
    redis_client.set("nlu_films_cache", json.dumps(list(all_titles)))
    # [V4.13] Nova cache NLU de diretores
    redis_client.set("nlu_directors_cache", json.dumps(list(all_directors)))
    print(f"[Pipeline] Caches NLU guardadas: {len(all_actors)} atores, {len(all_genres)} géneros, {len(all_titles)} filmes, {len(all_directors)} diretores.")

# --- Ponto de Entrada Principal ---
if __name__ == "__main__":
    print("=== INICIANDO PIPELINE ETL (db-init V4.3) ===")

    # 1. Esperar
    db_engine, redis_client = wait_for_services()

    # 2. Criar Tabelas
    execute_sql_script(db_engine, SQL_INIT_PATH)

    # 3. Carregar CSV -> MySQL
    load_csv_to_mysql(db_engine, CSV_PATH)

    # 4. Gerar Factos Prolog e Caches Redis
    generate_prolog_and_caches(db_engine, redis_client, PROLOG_FACTS_PATH)

    print("=== PIPELINE ETL CONCLUÍDO COM SUCESSO ===")