from typing import List, Dict, Optional
from pyswip import Prolog
import asyncio
import logging
from concurrent.futures import ThreadPoolExecutor, TimeoutError as FuturesTimeoutError
import functools

logger = logging.getLogger(__name__)


class PrologTimeoutError(Exception):
    """Exceção levantada quando uma query Prolog excede o timeout."""
    pass


class PrologConnector:
    """Conector para o engine SWI-Prolog usando PySWIP.

    Mantém uma instância única de `Prolog()` e oferece utilitários para
    consultar e carregar regras.
    """
    
    DEFAULT_TIMEOUT = 2.0  # segundos

    def __init__(self) -> None:
        self.prolog = Prolog()
        self._loaded_files: set[str] = set()
        # ThreadPoolExecutor para queries com timeout
        # Nota: Usamos ThreadPool pois ProcessPool não funciona bem com pyswip
        # O timeout é "best effort" - se o Prolog travar, o thread pode não liberar
        self._executor = ThreadPoolExecutor(max_workers=4, thread_name_prefix="prolog_query")

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
    
    def _execute_query(self, query_string: str) -> List[Dict]:
        """
        Executa query Prolog de forma síncrona (para uso em thread/processo).
        """
        try:
            results = list(self.prolog.query(query_string))
            return results
        except Exception as e:
            logger.error(f"Erro na query Prolog '{query_string}': {e}")
            raise
    
    async def query_with_timeout(
        self, 
        query_string: str, 
        timeout: float = DEFAULT_TIMEOUT
    ) -> List[Dict]:
        """
        Executa uma consulta Prolog com timeout.
        
        Usa ThreadPoolExecutor para executar a query em thread separada,
        permitindo cancelamento por timeout.
        
        Args:
            query_string: Query Prolog a executar
            timeout: Timeout em segundos (default: 2.0)
            
        Returns:
            Lista de dicionários com resultados
            
        Raises:
            PrologTimeoutError: Se a query exceder o timeout
            Exception: Outros erros da query Prolog
        """
        loop = asyncio.get_event_loop()
        
        try:
            # Executa a query em thread separada com timeout
            future = loop.run_in_executor(
                self._executor,
                functools.partial(self._execute_query, query_string)
            )
            
            results = await asyncio.wait_for(future, timeout=timeout)
            
            logger.debug(f"Query Prolog executada com sucesso: {query_string} -> {len(results)} resultados")
            return results
            
        except asyncio.TimeoutError:
            logger.warning(f"Timeout ({timeout}s) na query Prolog: {query_string}")
            raise PrologTimeoutError(
                f"A consulta excedeu o tempo limite de {timeout} segundos. "
                "Por favor, tente uma consulta mais específica."
            )
        except Exception as e:
            logger.error(f"Erro na query Prolog '{query_string}': {e}")
            raise
    
    def query_sync_with_timeout(
        self, 
        query_string: str, 
        timeout: float = DEFAULT_TIMEOUT
    ) -> List[Dict]:
        """
        Versão síncrona do query_with_timeout (para uso em handlers não-async).
        
        Args:
            query_string: Query Prolog a executar
            timeout: Timeout em segundos (default: 2.0)
            
        Returns:
            Lista de dicionários com resultados
            
        Raises:
            PrologTimeoutError: Se a query exceder o timeout
        """
        from concurrent.futures import wait, FIRST_COMPLETED
        
        future = self._executor.submit(self._execute_query, query_string)
        
        try:
            results = future.result(timeout=timeout)
            logger.debug(f"Query Prolog (sync) executada: {query_string} -> {len(results)} resultados")
            return results
        except FuturesTimeoutError:
            logger.warning(f"Timeout ({timeout}s) na query Prolog (sync): {query_string}")
            # Tentar cancelar a future (pode não funcionar se já iniciou)
            future.cancel()
            raise PrologTimeoutError(
                f"A consulta excedeu o tempo limite de {timeout} segundos. "
                "Por favor, tente uma consulta mais específica."
            )
        except Exception as e:
            logger.error(f"Erro na query Prolog (sync) '{query_string}': {e}")
            raise


# Instância única para ser usada em toda a aplicação
prolog_service = PrologConnector()