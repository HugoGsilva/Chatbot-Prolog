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


class SakilaExporter:
    """Orquestra a exportação das tabelas e valida contagens."""

    def __init__(self, db_connector: DatabaseConnector, fact_writer: FactWriter) -> None:
        self.db_connector = db_connector
        self.fact_writer = fact_writer
        self.conn = self.db_connector.connect()
        self.summary: List[Dict[str, Any]] = []

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
        print(f"\n[+] Exportando '{table_name}'...")
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
                    # Em caso de erro de linha, mantemos processo e relatamos divergência
                    print(f"  [!] Erro ao processar linha: {e}")

        status = "OK" if mysql_count == facts_count else "DIVERGÊNCIA"
        print(f"    - Registros no MySQL: {mysql_count}")
        print(f"    - Fatos Prolog gerados: {facts_count}")
        print(f"    - Status: {status}")

        self.summary.append(
            {
                "table": table_name,
                "predicate": prolog_predicate,
                "mysql_count": mysql_count,
                "facts_count": facts_count,
                "status": status,
            }
        )

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

        # Exportações
        self.export_table("actor", "actor", map_actor)
        self.export_table("film", "film", map_film)
        self.export_table("category", "category", map_category)
        self.export_table("film_actor", "acted_in", map_film_actor)
        self.export_table("film_category", "film_category", map_film_category)

        print("\nExportação finalizada com sucesso.")


def main():
    # Caminhos padrão
    config_path = os.environ.get("SAKILA_CONFIG", "config.ini")
    output_path = os.environ.get(
        "SAKILA_OUTPUT", os.path.join("prolog", "knowledge", "sakila_kb.pl")
    )

    try:
        db = DatabaseConnector(config_path)
        writer = FactWriter(output_path)
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