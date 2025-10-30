"""
Script de exportação de SUBCONJUNTO da base Sakila (MySQL) para fatos Prolog.

Objetivo: gerar um arquivo pequeno e referencialmente íntegro com:
  - Atores: apenas os 20 primeiros (ordenados por actor_id)
  - Filmes: apenas os 50 primeiros (ordenados por film_id)
  - Categorias: todas as 16 categorias
  - Relações: acted_in/2 e film_category/2 SOMENTE quando os IDs existem no subconjunto

Reuso: utiliza DatabaseConnector e FactWriter definidos em backend/scripts/run_export.py

Saída: prolog/knowledge/sakila.pl
Credenciais: backend/config/config.ini (ou SAKILA_CONFIG via ambiente)
"""

from __future__ import annotations

import os
import sys
from typing import Any, Dict, List, Tuple

try:
    # Reuso das classes utilitárias
    from backend.scripts.run_export import DatabaseConnector, FactWriter, _format_arg
except Exception as e:
    print("[ERRO] Falha ao importar utilitários de exportação.")
    print("Certifique-se de executar a partir da raiz do projeto e de estar usando Python 3.11.")
    print(f"Detalhes: {e}")
    sys.exit(1)


PROLOG_MODULE_HEADER = """
/**
 * Módulo de Fatos Sakila (Gerado Automaticamente)
 *
 * Este módulo contém exclusivamente dados (fatos) do domínio Sakila,
 * gerados pelo script run_export_subset.py.
 */
 :- module(sakila_facts, [
     actor/2,
     film/5,
     acted_in/2,
     category/2,
     film_category/2
 ]).
"""


def _build_fact(predicate: str, args: List[Any]) -> str:
    """Constroi string de fato Prolog usando o formatador compartilhado."""
    formatted = ", ".join(_format_arg(a) for a in args)
    return f"{predicate}({formatted})"


class SubsetIntegrity:
    """Mantém na memória os IDs do subconjunto e valida relacionamentos."""

    def __init__(self, conn) -> None:
        self.conn = conn
        self.actor_ids: set[int] = set()
        self.film_ids: set[int] = set()
        self.category_ids: set[int] = set()

    def load(self) -> Tuple[int, int, int]:
        """Carrega IDs do subconjunto: 20 atores, 50 filmes, todas categorias."""
        # Atores (20 primeiros)
        with self.conn.cursor(dictionary=False) as cur:
            cur.execute("SELECT actor_id FROM actor ORDER BY actor_id LIMIT 20;")
            for row in cur:
                self.actor_ids.add(int(row[0]))

        # Filmes (50 primeiros)
        with self.conn.cursor(dictionary=False) as cur:
            cur.execute("SELECT film_id FROM film ORDER BY film_id LIMIT 50;")
            for row in cur:
                self.film_ids.add(int(row[0]))

        # Categorias (todas)
        with self.conn.cursor(dictionary=False) as cur:
            cur.execute("SELECT category_id FROM category ORDER BY category_id;")
            for row in cur:
                self.category_ids.add(int(row[0]))

        return (len(self.actor_ids), len(self.film_ids), len(self.category_ids))

    def allow_acted_in(self, actor_id: int, film_id: int) -> bool:
        return (actor_id in self.actor_ids) and (film_id in self.film_ids)

    def allow_film_category(self, film_id: int, category_id: int) -> bool:
        return (film_id in self.film_ids) and (category_id in self.category_ids)


class HeaderFactWriter(FactWriter):
    """Extensão de FactWriter que escreve cabeçalho de módulo ao abrir o arquivo."""

    def __init__(self, filepath: str) -> None:
        super().__init__(filepath)
        self.write_header()

    def write_header(self) -> None:
        try:
            self._fp.write(PROLOG_MODULE_HEADER.strip() + "\n\n")
            self._fp.flush()
        except Exception:
            pass


def _export_actors(conn, writer: FactWriter) -> int:
    """Exporta atores do subconjunto (20 primeiros)."""
    count = 0
    with conn.cursor(dictionary=True) as cur:
        cur.execute(
            "SELECT actor_id, first_name, last_name FROM actor ORDER BY actor_id LIMIT 20;"
        )
        for row in cur:
            name = f"{row.get('first_name', '')} {row.get('last_name', '')}".strip()
            fact = _build_fact("actor", [row.get("actor_id"), name])
            writer.write_fact(fact)
            count += 1
    print(f"[actor] Escritos {count} fatos (limite 20).")
    return count


def _export_films(conn, writer: FactWriter) -> int:
    """Exporta filmes do subconjunto (50 primeiros)."""
    count = 0
    with conn.cursor(dictionary=True) as cur:
        cur.execute(
            """
            SELECT film_id, title, description, release_year, length
            FROM film
            ORDER BY film_id
            LIMIT 50;
            """
        )
        for row in cur:
            length = row.get("length") if row.get("length") is not None else 0
            release_year = (
                row.get("release_year") if row.get("release_year") is not None else 0
            )
            description = row.get("description") if row.get("description") is not None else ""
            title = row.get("title") or ""
            fact = _build_fact(
                "film",
                [row.get("film_id"), title, description, release_year, length],
            )
            writer.write_fact(fact)
            count += 1
    print(f"[film] Escritos {count} fatos (limite 50).")
    return count


def _export_categories(conn, writer: FactWriter) -> int:
    """Exporta todas as categorias (16)."""
    count = 0
    with conn.cursor(dictionary=True) as cur:
        cur.execute("SELECT category_id, name FROM category ORDER BY category_id;")
        for row in cur:
            fact = _build_fact("category", [row.get("category_id"), row.get("name") or ""]) 
            writer.write_fact(fact)
            count += 1
    print(f"[category] Escritos {count} fatos (esperado 16).")
    return count


def _export_relationships(conn, writer: FactWriter, subset: SubsetIntegrity) -> None:
    """Exporta relacionamentos dentro do subconjunto: acted_in/2 e film_category/2."""
    # film_actor -> acted_in/2
    print("\n[+] Processando relacionamentos 'acted_in' (film_actor)...")
    total_read = 0
    written = 0
    rejected = 0
    with conn.cursor(dictionary=True) as cur:
        cur.execute("SELECT actor_id, film_id FROM film_actor;")
        for row in cur:
            total_read += 1
            actor_id = int(row.get("actor_id"))
            film_id = int(row.get("film_id"))
            if subset.allow_acted_in(actor_id, film_id):
                writer.write_fact(_build_fact("acted_in", [actor_id, film_id]))
                written += 1
            else:
                rejected += 1
    print(f"    - Lidos: {total_read} | Escritos (válidos): {written} | Rejeitados: {rejected}")

    # film_category -> film_category/2
    print("[+] Processando relacionamentos 'film_category'...")
    total_read = 0
    written = 0
    rejected = 0
    with conn.cursor(dictionary=True) as cur:
        cur.execute("SELECT film_id, category_id FROM film_category;")
        for row in cur:
            total_read += 1
            film_id = int(row.get("film_id"))
            category_id = int(row.get("category_id"))
            if subset.allow_film_category(film_id, category_id):
                writer.write_fact(_build_fact("film_category", [film_id, category_id]))
                written += 1
            else:
                rejected += 1
    print(f"    - Lidos: {total_read} | Escritos (válidos): {written} | Rejeitados: {rejected}")


def main() -> None:
    # Resolvem caminhos padrão
    script_dir = os.path.dirname(os.path.abspath(__file__))
    default_config = os.path.normpath(os.path.join(script_dir, "..", "config", "config.ini"))
    config_path = os.environ.get("SAKILA_CONFIG", default_config)

    # Saída fixa conforme requisito
    output_path = os.path.join("prolog", "knowledge", "sakila.pl")
    print(f"[INFO] Usando config: {config_path}")
    print(f"[INFO] Saída: {output_path}")

    db = None
    writer = None
    try:
        db = DatabaseConnector(config_path)
        conn = db.connect()
        writer = HeaderFactWriter(output_path)

        # Carrega subconjunto
        print("\n[+] Carregando subconjunto de IDs (20 atores, 50 filmes, 16 categorias)...")
        subset = SubsetIntegrity(conn)
        n_actors, n_films, n_categories = subset.load()
        print(f"    - Atores: {n_actors} | Filmes: {n_films} | Categorias: {n_categories}")

        # Exporta entidades principais
        print("\n[+] Exportando entidades do subconjunto...")
        _export_actors(conn, writer)
        _export_films(conn, writer)
        _export_categories(conn, writer)

        # Exporta relacionamentos restritos ao subconjunto
        _export_relationships(conn, writer, subset)

        print("\nExportação de subconjunto finalizada.")
        print(f"Arquivo gerado em: {writer.filepath}")

    except Exception as e:
        print("\n[ERRO] Exportação de subconjunto interrompida.")
        print(f"Detalhes: {e}")
        sys.exit(1)
    finally:
        try:
            if writer:
                writer.close()
        except Exception:
            pass
        try:
            if db:
                db.close()
        except Exception:
            pass


if __name__ == "__main__":
    main()