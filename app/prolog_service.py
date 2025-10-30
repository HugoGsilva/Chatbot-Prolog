from typing import List, Dict
from pyswip import Prolog


class PrologConnector:
    """Conector para o engine SWI-Prolog usando PySWIP.

    Mantém uma instância única de `Prolog()` e oferece utilitários para
    consultar e carregar regras.
    """

    def __init__(self) -> None:
        self.prolog = Prolog()
        self._loaded_files: set[str] = set()

    def load_rules(self, file_path: str) -> None:
        """Carrega regras Prolog a partir de um arquivo (consult).

        Garante carregamento único por arquivo durante o ciclo de vida
        da aplicação.
        """
        if file_path in self._loaded_files:
            print(f"[PrologConnector] Regras já carregadas de: {file_path}")
            return
        self.prolog.consult(file_path)
        self._loaded_files.add(file_path)
        print(f"[PrologConnector] Regras Prolog de {file_path} carregadas.")

    def query(self, query_string: str) -> List[Dict]:
        """Executa uma consulta Prolog e retorna lista de dicts Python."""
        results = list(self.prolog.query(query_string))
        print(f"[PrologConnector] Consulta executada: {query_string} -> {len(results)} resultados")
        return results


# Instância única para ser usada em toda a aplicação
prolog_service = PrologConnector()