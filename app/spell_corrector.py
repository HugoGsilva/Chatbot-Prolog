"""
Corretor Ortográfico para o Chatbot usando SymSpell.

Este módulo implementa correção ortográfica otimizada para português,
usando o algoritmo SymSpell para correções rápidas com edit distance ≤ 2.

O corretor é inicializado com vocabulário das caches NLU (atores, gêneros,
filmes, diretores) para corrigir nomes próprios e termos específicos do domínio.
"""

import logging
import re
from typing import List, Optional, Tuple

from symspellpy import SymSpell, Verbosity

logger = logging.getLogger(__name__)


class SpellCorrector:
    """
    Corretor ortográfico otimizado para o domínio de filmes.
    
    Usa SymSpell com vocabulário customizado das caches NLU para
    corrigir erros de digitação em queries do usuário.
    
    Attributes:
        symspell: Instância do SymSpell configurada
        max_edit_distance: Distância máxima de edição para correções
        _initialized: Flag indicando se o vocabulário foi carregado
    """
    
    # Palavras comuns em português que não devem ser "corrigidas"
    COMMON_WORDS = {
        # Preposições e artigos
        "de", "do", "da", "dos", "das", "em", "no", "na", "nos", "nas",
        "por", "pelo", "pela", "pelos", "pelas", "com", "para", "pra",
        "a", "o", "as", "os", "um", "uma", "uns", "umas",
        # Palavras-chave do chatbot
        "filmes", "filme", "ator", "atriz", "diretor", "diretora",
        "genero", "gênero", "generos", "gêneros", "ano", "anos",
        "recomendar", "recomendação", "recomendacoes", "recomendações",
        "contar", "quantos", "qual", "quais", "listar", "lista",
        "aleatorio", "aleatório", "random", "sugerir", "sugestão",
        # Conectores
        "e", "ou", "que", "com", "sem", "mais", "menos",
    }
    
    # Palavras que indicam início de entidade (após estas, vem o nome)
    ENTITY_MARKERS = {"por", "do", "da", "de", "com", "pelo", "pela"}
    
    def __init__(self, max_edit_distance: int = 2):
        """
        Inicializa o corretor ortográfico.
        
        Args:
            max_edit_distance: Distância máxima de edição para correções (default: 2)
        """
        self.max_edit_distance = max_edit_distance
        self.symspell = SymSpell(max_dictionary_edit_distance=max_edit_distance)
        self._initialized = False
        self._vocabulary_size = 0
        
        logger.info(f"SpellCorrector inicializado (max_edit_distance={max_edit_distance})")
    
    def load_vocabulary(
        self,
        actors: Optional[List[str]] = None,
        genres: Optional[List[str]] = None,
        films: Optional[List[str]] = None,
        directors: Optional[List[str]] = None
    ) -> int:
        """
        Carrega vocabulário das caches NLU no SymSpell.
        
        Cada termo recebe uma frequência baseada na sua importância:
        - Atores: frequência 100
        - Gêneros: frequência 150 (mais comuns em queries)
        - Filmes: frequência 80
        - Diretores: frequência 90
        
        Args:
            actors: Lista de nomes de atores
            genres: Lista de gêneros
            films: Lista de títulos de filmes
            directors: Lista de nomes de diretores
            
        Returns:
            Número total de termos carregados
        """
        count = 0
        
        # Adiciona palavras comuns com alta frequência
        for word in self.COMMON_WORDS:
            self.symspell.create_dictionary_entry(word.lower(), 1000)
            count += 1
        
        # Processa cada cache
        if actors:
            for actor in actors:
                count += self._add_name_to_dictionary(actor, frequency=100)
        
        if genres:
            for genre in genres:
                count += self._add_term_to_dictionary(genre, frequency=150)
        
        if films:
            for film in films:
                count += self._add_term_to_dictionary(film, frequency=80)
        
        if directors:
            for director in directors:
                count += self._add_name_to_dictionary(director, frequency=90)
        
        self._vocabulary_size = count
        self._initialized = True
        
        logger.info(f"Vocabulário carregado: {count} termos")
        return count
    
    def _add_name_to_dictionary(self, name: str, frequency: int) -> int:
        """
        Adiciona um nome próprio ao dicionário.
        
        Para nomes compostos (ex: "Tom Hanks"), adiciona:
        - O nome completo em lowercase
        - Cada parte do nome individualmente
        
        Args:
            name: Nome a adicionar
            frequency: Frequência do termo
            
        Returns:
            Número de entradas adicionadas
        """
        if not name or not name.strip():
            return 0
        
        count = 0
        name_clean = name.strip()
        
        # Adiciona nome completo
        self.symspell.create_dictionary_entry(name_clean.lower(), frequency)
        count += 1
        
        # Adiciona partes do nome (para nomes compostos)
        parts = name_clean.split()
        if len(parts) > 1:
            for part in parts:
                if len(part) >= 2:  # Ignora iniciais
                    self.symspell.create_dictionary_entry(part.lower(), frequency // 2)
                    count += 1
        
        return count
    
    def _add_term_to_dictionary(self, term: str, frequency: int) -> int:
        """
        Adiciona um termo genérico ao dicionário.
        
        Args:
            term: Termo a adicionar
            frequency: Frequência do termo
            
        Returns:
            Número de entradas adicionadas
        """
        if not term or not term.strip():
            return 0
        
        term_clean = term.strip().lower()
        self.symspell.create_dictionary_entry(term_clean, frequency)
        return 1
    
    def correct(self, text: str) -> Tuple[str, bool]:
        """
        Corrige erros ortográficos no texto.
        
        Aplica correção palavra por palavra, preservando:
        - Estrutura da frase
        - Capitalização original (quando possível)
        - Palavras que já estão corretas
        
        Args:
            text: Texto a corrigir
            
        Returns:
            Tupla (texto_corrigido, houve_correcao)
            
        Examples:
            >>> corrector.correct("flmes por tom hanks")
            ("filmes por tom hanks", True)
            
            >>> corrector.correct("filmes de ação")
            ("filmes de ação", False)
        """
        if not text or not text.strip():
            return text, False
        
        if not self._initialized:
            logger.warning("SpellCorrector não inicializado - retornando texto original")
            return text, False
        
        original_text = text.strip()
        words = self._tokenize(original_text)
        corrected_words = []
        was_corrected = False
        
        for i, word in enumerate(words):
            # Preserva espaços e pontuação
            if not word.strip() or not word.isalpha():
                corrected_words.append(word)
                continue
            
            # Não corrige palavras muito curtas
            if len(word) <= 2:
                corrected_words.append(word)
                continue
            
            # Verifica se é palavra comum (não corrigir)
            if word.lower() in self.COMMON_WORDS:
                corrected_words.append(word)
                continue
            
            # Tenta corrigir
            corrected = self._correct_word(word)
            if corrected.lower() != word.lower():
                was_corrected = True
                # Preserva capitalização original
                if word[0].isupper():
                    corrected = corrected.capitalize()
                elif word.isupper():
                    corrected = corrected.upper()
            
            corrected_words.append(corrected)
        
        result = "".join(corrected_words)
        
        if was_corrected:
            logger.debug(f"Correção aplicada: '{original_text}' -> '{result}'")
        
        return result, was_corrected
    
    def _tokenize(self, text: str) -> List[str]:
        """
        Tokeniza texto preservando espaços e pontuação.
        
        Args:
            text: Texto a tokenizar
            
        Returns:
            Lista de tokens (palavras, espaços, pontuação)
        """
        # Regex que separa palavras de espaços e pontuação
        return re.findall(r'\w+|[^\w]', text)
    
    def _correct_word(self, word: str) -> str:
        """
        Corrige uma única palavra usando SymSpell.
        
        Args:
            word: Palavra a corrigir
            
        Returns:
            Palavra corrigida (ou original se não houver correção)
        """
        suggestions = self.symspell.lookup(
            word.lower(),
            Verbosity.CLOSEST,
            max_edit_distance=self.max_edit_distance
        )
        
        if suggestions and suggestions[0].distance > 0:
            return suggestions[0].term
        
        return word
    
    def get_suggestions(self, word: str, max_suggestions: int = 3) -> List[str]:
        """
        Retorna sugestões de correção para uma palavra.
        
        Args:
            word: Palavra para obter sugestões
            max_suggestions: Número máximo de sugestões
            
        Returns:
            Lista de palavras sugeridas
        """
        if not self._initialized:
            return []
        
        suggestions = self.symspell.lookup(
            word.lower(),
            Verbosity.ALL,
            max_edit_distance=self.max_edit_distance
        )
        
        return [s.term for s in suggestions[:max_suggestions]]
    
    @property
    def vocabulary_size(self) -> int:
        """Retorna o tamanho do vocabulário carregado."""
        return self._vocabulary_size
    
    @property
    def is_initialized(self) -> bool:
        """Retorna True se o vocabulário foi carregado."""
        return self._initialized


# =============================================================================
# SINGLETON INSTANCE
# =============================================================================

_spell_corrector_instance: Optional[SpellCorrector] = None


def get_spell_corrector() -> SpellCorrector:
    """
    Retorna a instância singleton do SpellCorrector.
    
    Returns:
        Instância do SpellCorrector (pode não estar inicializada)
    """
    global _spell_corrector_instance
    if _spell_corrector_instance is None:
        _spell_corrector_instance = SpellCorrector()
    return _spell_corrector_instance


def initialize_spell_corrector(
    actors: Optional[List[str]] = None,
    genres: Optional[List[str]] = None,
    films: Optional[List[str]] = None,
    directors: Optional[List[str]] = None
) -> SpellCorrector:
    """
    Inicializa o singleton do SpellCorrector com vocabulário.
    
    Deve ser chamado durante o startup da aplicação.
    
    Args:
        actors: Lista de nomes de atores
        genres: Lista de gêneros
        films: Lista de títulos de filmes
        directors: Lista de nomes de diretores
        
    Returns:
        Instância inicializada do SpellCorrector
    """
    corrector = get_spell_corrector()
    corrector.load_vocabulary(
        actors=actors,
        genres=genres,
        films=films,
        directors=directors
    )
    return corrector
