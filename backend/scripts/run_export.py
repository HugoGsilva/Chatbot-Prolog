"""
Script de exportação da base Sakila (MySQL) para fatos Prolog.

Dependência externa: mysql-connector-python
Lê credenciais de config.ini e gera arquivo .pl com fatos:
  - actor(ActorID, Name)
  - film(FilmID, Title, Description, ReleaseYear, LengthMinutes)
  - acted_in(ActorID, FilmID)
  - category(CategoryID, Name)
  - film_category(FilmID, CategoryID)

Arquitetura orientada a objetos:
  - DatabaseConnector: gerencia conexão MySQL
  - FactWriter: manipula escrita de fatos Prolog
  - SakilaExporter: orquestra exportação e validação
"""

from __future__ import annotations

import os
import sys
import configparser
from typing import Callable, List, Dict, Any

try:
    import mysql.connector
    from mysql.connector import Error
except Exception as e:
    print("[ERRO] Dependência 'mysql-connector-python' não encontrada.")
    print("Instale com: pip install mysql-connector-python")
    raise


def _escape_prolog_atom(text: str) -> str:
    """Escapa aspas simples para uso em átomos Prolog ('O''Brien')."""
    return text.replace("'", "''")


def _format_arg(value: Any) -> str:
    """Formata o argumento para sintaxe de fato Prolog.
    - Strings: envolvidas por aspas simples e com aspas internas duplicadas
    - None: atom 'null'
    - Números: sem aspas
    - Outros: convertidos para string e tratados como strings
    """
    if value is None:
        return "null"
    if isinstance(value, (int, float)):
        return str(value)
    s = str(value)
    return f"'{_escape_prolog_atom(s)}'"


class DatabaseConnector:
    """Gerencia leitura das credenciais e a conexão com MySQL."""

    def __init__(self, config_file: str = "config.ini") -> None:
        self.config_file = config_file
        self.config = configparser.ConfigParser()
        if not os.path.exists(self.config_file):
            raise FileNotFoundError(
                f"Arquivo de configuração não encontrado: {self.config_file}"
            )
        self.config.read(self.config_file)
        if "mysql" not in self.config:
            raise ValueError("Seção [mysql] ausente em config.ini")
        self.host = self.config.get("mysql", "host", fallback=None)
        self.user = self.config.get("mysql", "user", fallback=None)
        self.password = self.config.get("mysql", "password", fallback=None)
        self.database = self.config.get("mysql", "database", fallback=None)
        self.port = self.config.getint("mysql", "port", fallback=3306)
        for key, val in {
            "host": self.host,
            "user": self.user,
            "password": self.password,
            "database": self.database,
        }.items():
            if val in (None, ""):
                raise ValueError(f"Chave '{key}' ausente ou vazia em [mysql]")

        self._conn = None

    def connect(self):
        """Estabelece a conexão e retorna o objeto de conexão."""
        try:
            self._conn = mysql.connector.connect(
                host=self.host,
                port=self.port,
                user=self.user,
                password=self.password,
                database=self.database,
            )
            return self._conn
        except Error as e:
            print("[ERRO] Falha ao conectar ao MySQL.")
            print(f"Detalhes: {e}")
            raise

    def close(self):
        try:
            if self._conn:
                self._conn.close()
        except Exception:
            pass


class FactWriter:
    """Manipula escrita do arquivo .pl de saída."""

    def __init__(self, filepath: str) -> None:
        self.filepath = filepath
        out_dir = os.path.dirname(self.filepath)
        if out_dir and not os.path.exists(out_dir):
            os.makedirs(out_dir, exist_ok=True)
        self._fp = open(self.filepath, "w", encoding="utf-8", newline="\n")
        self._count = 0

    def write_fact(self, fact_string: str) -> None:
        """Escreve uma linha e garante ponto final."""
        fact_string = fact_string.strip()
        if not fact_string.endswith('.'):
            fact_string += '.'
        self._fp.write(fact_string + "\n")
        self._count += 1

    def get_line_count(self) -> int:
        return self._count

    def close(self) -> None:
        try:
            if self._fp:
                self._fp.flush()
                self._fp.close()
        except Exception:
            pass


# Cabeçalho de módulo para o dataset COMPLETO (produção)
PROLOG_MODULE_HEADER = """
/**
 * Módulo de Fatos Sakila (Gerado Automaticamente)
 *
 * Este módulo contém dados (fatos) completos do domínio Sakila,
 * gerados pelo script run_export.py.
 */
 :- module(sakila_facts, [
     actor/2,
     film/5,
     acted_in/2,
     category/2,
     film_category/2
 ]).
"""


class HeaderFactWriter(FactWriter):
    """Extensão que escreve cabeçalho de módulo ao abrir o arquivo."""

    def __init__(self, filepath: str) -> None:
        super().__init__(filepath)
        self.write_header()

    def write_header(self) -> None:
        try:
            self._fp.write(PROLOG_MODULE_HEADER.strip() + "\n\n")
            self._fp.flush()
        except Exception:
            pass


class ReferentialIntegrityChecker:
    """Carrega e valida integridade referencial entre tabelas Sakila."""

    def __init__(self) -> None:
        self.valid_actor_ids = set()
        self.valid_film_ids = set()
        self.valid_category_ids = set()

    def load_primary_keys(self, db_conn, table_name: str, pk_column: str) -> int:
        """Carrega todos os IDs primários da tabela e popula o set correspondente.
        Retorna a quantidade carregada.
        """
        query = f"SELECT {pk_column} FROM {table_name};"
        ids = set()
        with db_conn.cursor(dictionary=False) as cur:
            cur.execute(query)
            for row in cur:
                # row é uma tupla de uma coluna
                ids.add(int(row[0]))

        if table_name == "actor":
            self.valid_actor_ids = ids
        elif table_name == "film":
            self.valid_film_ids = ids
        elif table_name == "category":
            self.valid_category_ids = ids
        # Retorna contagem carregada
        return len(ids)

    def validate_acted_in(self, actor_id: int, film_id: int) -> bool:
        return (actor_id in self.valid_actor_ids) and (film_id in self.valid_film_ids)

    def validate_film_category(self, film_id: int, category_id: int) -> bool:
        return (film_id in self.valid_film_ids) and (category_id in self.valid_category_ids)


class SakilaExporter:
    """Orquestra a exportação das tabelas e valida contagens."""

    def __init__(self, db_connector: DatabaseConnector, fact_writer: FactWriter) -> None:
        self.db_connector = db_connector
        self.fact_writer = fact_writer
        self.conn = self.db_connector.connect()
        self.summary: List[Dict[str, Any]] = []
        self.checker = ReferentialIntegrityChecker()
        self.error_log_path = os.environ.get("SAKILA_ERROR_LOG", "integrity_errors.log")
        self._err_fp = open(self.error_log_path, "w", encoding="utf-8")

    def _build_fact(self, predicate: str, args: List[Any]) -> str:
        formatted = ", ".join(_format_arg(a) for a in args)
        return f"{predicate}({formatted})"

    def _get_table_count(self, table_name: str) -> int:
        query = f"SELECT COUNT(*) AS cnt FROM {table_name};"
        with self.conn.cursor(dictionary=True) as cur:
            cur.execute(query)
            row = cur.fetchone()
            return int(row["cnt"]) if row and "cnt" in row else 0

    def export_table(
        self,
        table_name: str,
        prolog_predicate: str,
        column_mapping: Callable[[Dict[str, Any]], List[Any]],
    ) -> None:
        """Exporta uma tabela para fatos Prolog.
        - Obtém contagem em MySQL
        - Exporta todas as linhas, aplicando mapeamento -> args
        - Valida a contagem de fatos gerados
        - Registra resumo
        """
        # Exporta tabela "pai" com resumo em linha
        mysql_count = self._get_table_count(table_name)

        facts_count = 0
        with self.conn.cursor(dictionary=True) as cur:
            cur.execute(f"SELECT * FROM {table_name};")
            for row in cur:
                try:
                    args = column_mapping(row)
                    fact = self._build_fact(prolog_predicate, args)
                    self.fact_writer.write_fact(fact)
                    facts_count += 1
                except Exception as e:
                    print(f"  [!] Erro ao processar linha: {e}")

        status = "OK" if mysql_count == facts_count else "DIVERGÊNCIA"
        print(f"[+] Exportando '{table_name}'... ({mysql_count} registros) -> {status} ({facts_count} fatos escritos)")

        self.summary.append(
            {
                "table": table_name,
                "predicate": prolog_predicate,
                "mysql_count": mysql_count,
                "facts_count": facts_count,
                "status": status,
            }
        )

    def _log_rejection(self, label: str, msg: str) -> None:
        try:
            self._err_fp.write(f"REJEITADO ({label}): {msg}\n")
        except Exception:
            pass

    def run_export(self) -> None:
        print("Iniciando exportação da base Sakila para fatos Prolog...")

        # Mapeamentos para cada tabela/predicado
        def map_actor(row: Dict[str, Any]) -> List[Any]:
            name = f"{row.get('first_name', '')} {row.get('last_name', '')}".strip()
            return [row.get("actor_id"), name]

        def map_film(row: Dict[str, Any]) -> List[Any]:
            length = row.get("length") if row.get("length") is not None else 0
            release_year = row.get("release_year") if row.get("release_year") is not None else 0
            description = row.get("description") if row.get("description") is not None else ""
            return [
                row.get("film_id"),
                row.get("title", ""),
                description,
                release_year,
                length,
            ]

        def map_category(row: Dict[str, Any]) -> List[Any]:
            return [row.get("category_id"), row.get("name", "")]

        def map_film_actor(row: Dict[str, Any]) -> List[Any]:
            return [row.get("actor_id"), row.get("film_id")]

        def map_film_category(row: Dict[str, Any]) -> List[Any]:
            return [row.get("film_id"), row.get("category_id")]

        # 1) Carregar chaves primárias
        print("\n[+] Carregando chaves primárias...")
        count_actor_ids = self.checker.load_primary_keys(self.conn, "actor", "actor_id")
        print(f"    - Carregados {count_actor_ids} IDs de 'actor'.")
        count_film_ids = self.checker.load_primary_keys(self.conn, "film", "film_id")
        print(f"    - Carregados {count_film_ids} IDs de 'film'.")
        count_category_ids = self.checker.load_primary_keys(self.conn, "category", "category_id")
        print(f"    - Carregados {count_category_ids} IDs de 'category'.")

        # 2) Exportar tabelas pai (com resumo em linha)
        self.export_table("actor", "actor", map_actor)
        self.export_table("film", "film", map_film)
        self.export_table("category", "category", map_category)

        # 3) Processar tabelas filho com validação
        # acted_in
        print("\n[+] Processando 'acted_in'...")
        mysql_read = self._get_table_count("film_actor")
        valid_written = 0
        orphan_rejected = 0
        with self.conn.cursor(dictionary=True) as cur:
            cur.execute("SELECT * FROM film_actor;")
            for row in cur:
                actor_id = int(row.get("actor_id"))
                film_id = int(row.get("film_id"))
                if self.checker.validate_acted_in(actor_id, film_id):
                    fact = self._build_fact("acted_in", [actor_id, film_id])
                    self.fact_writer.write_fact(fact)
                    valid_written += 1
                else:
                    orphan_rejected += 1
                    self._log_rejection(
                        "acted_in",
                        f"Registro órfão. ActorID '{actor_id}' ou FilmID '{film_id}' não encontrado.",
                    )
        status = "OK" if mysql_read == valid_written and orphan_rejected == 0 else "DIVERGÊNCIA"
        print(f"    - Registros lidos do MySQL: {mysql_read}")
        print(f"    - Fatos válidos escritos: {valid_written}")
        print(f"    - Registros órfãos (rejeitados): {orphan_rejected}")
        print(f"    - Status: {status}")

        # film_category
        print("\n[+] Processando 'film_category'...")
        mysql_read = self._get_table_count("film_category")
        valid_written = 0
        orphan_rejected = 0
        with self.conn.cursor(dictionary=True) as cur:
            cur.execute("SELECT * FROM film_category;")
            for row in cur:
                film_id = int(row.get("film_id"))
                category_id = int(row.get("category_id"))
                if self.checker.validate_film_category(film_id, category_id):
                    fact = self._build_fact("film_category", [film_id, category_id])
                    self.fact_writer.write_fact(fact)
                    valid_written += 1
                else:
                    orphan_rejected += 1
                    self._log_rejection(
                        "film_category",
                        f"Registro órfão. FilmID '{film_id}' ou CategoryID '{category_id}' não encontrado.",
                    )
        status = "OK" if mysql_read == valid_written and orphan_rejected == 0 else "DIVERGÊNCIA"
        print(f"    - Registros lidos do MySQL: {mysql_read}")
        print(f"    - Fatos válidos escritos: {valid_written}")
        print(f"    - Registros órfãos (rejeitados): {orphan_rejected}")
        print(f"    - Status: {status}")

        print("\nExportação finalizada.")
        try:
            print(f"Arquivo gerado em: {self.fact_writer.filepath}")
            print(f"Log de erros em: {self.error_log_path}")
        except Exception:
            pass


def main():
    # Default seguro: backend/config/config.ini relativo ao script
    script_dir = os.path.dirname(os.path.abspath(__file__))
    default_config = os.path.normpath(os.path.join(script_dir, "..", "config", "config.ini"))
    config_path = os.environ.get("SAKILA_CONFIG", default_config)
    output_path = os.environ.get(
        "SAKILA_OUTPUT",
        os.path.join("prolog", "knowledge", "sakila_kb.pl")
    )
    print(f"[INFO] Usando config: {config_path}")
    print(f"[INFO] Saída: {output_path}")

    try:
        db = DatabaseConnector(config_path)
        writer = HeaderFactWriter(output_path)
        exporter = SakilaExporter(db, writer)
        exporter.run_export()
        print(f"Arquivo gerado em: {output_path}")
    except Exception as e:
        print("\n[ERRO] Exportação interrompida.")
        print(f"Detalhes: {e}")
        sys.exit(1)
    finally:
        try:
            writer.close()
        except Exception:
            pass
        try:
            db.close()
        except Exception:
            pass


if __name__ == "__main__":
    main()