"""
NLU Engine - Natural Language Understanding para o Chatbot

Este módulo implementa o processamento de linguagem natural usando spaCy,
permitindo detecção de intenções e extração de entidades de queries do usuário.

Atualizado para arquitetura Thin Client:
- Integração com SpellCorrector para correção ortográfica
- Retorno do schema NLUResult com confidence scoring
- Método get_suggestions para casos de baixa confiança
"""

import spacy
import json
import logging
import re
import unicodedata
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union
from thefuzz import fuzz

from .schemas import NLUResult
from .nlu import is_valid_genre, find_best_genre

logger = logging.getLogger(__name__)


def normalize_text(text: str) -> str:
    """Remove acentos e normaliza texto para comparação."""
    nfkd = unicodedata.normalize('NFKD', text)
    return ''.join(c for c in nfkd if not unicodedata.combining(c)).lower()


class NLUEngine:
    """
    Motor de NLU que processa texto em português e extrai intenções e entidades.
    
    Integra com SpellCorrector para correção ortográfica antes do processamento.
    Retorna NLUResult com confidence scoring para roteamento de confiança.
    """
    
    # Thresholds de confiança para roteamento
    HIGH_CONFIDENCE_THRESHOLD = 0.7
    MEDIUM_CONFIDENCE_THRESHOLD = 0.4
    
    def __init__(self, spell_corrector=None):
        """
        Inicializa o motor NLU carregando o modelo spaCy e padrões de intenção.
        
        Args:
            spell_corrector: Instância opcional do SpellCorrector para correção ortográfica
        """
        self._spell_corrector = spell_corrector
        
        try:
            self.nlp = spacy.load("pt_core_news_sm")
            logger.info("✅ Modelo spaCy 'pt_core_news_sm' carregado com sucesso")
        except OSError:
            logger.error("❌ Modelo spaCy não encontrado. Execute: python -m spacy download pt_core_news_sm")
            raise
        
        self.intent_patterns = self._load_patterns()
        logger.info(f"✅ {len(self.intent_patterns)} padrões de intenção carregados")
    
    def set_spell_corrector(self, spell_corrector) -> None:
        """
        Define o SpellCorrector para correção ortográfica.
        
        Args:
            spell_corrector: Instância do SpellCorrector
        """
        self._spell_corrector = spell_corrector
        logger.info("SpellCorrector configurado no NLUEngine")
    
    def _load_patterns(self) -> Dict:
        """Carrega os padrões de intenção do arquivo JSON."""
        patterns_path = Path(__file__).parent / "intent_patterns.json"
        
        try:
            with open(patterns_path, "r", encoding="utf-8") as f:
                return json.load(f)
        except FileNotFoundError:
            logger.warning(f"⚠️ Arquivo de padrões não encontrado: {patterns_path}")
            return {}
    
    def parse(self, text: str, apply_spell_correction: bool = True) -> NLUResult:
        """
        Analisa o texto e retorna a intenção detectada com entidades extraídas.
        
        Args:
            text: Texto do usuário para análise
            apply_spell_correction: Se True, aplica correção ortográfica antes do parse
            
        Returns:
            NLUResult com:
                - intent: Intenção detectada (ex: "filmes_por_ator")
                - entities: Dicionário de entidades extraídas
                - confidence: Score de confiança (0.0 a 1.0)
                - original_text: Texto original
                - corrected_text: Texto após correção (ou None)
                
        Examples:
            >>> nlu = NLUEngine()
            >>> result = nlu.parse("flmes por tom hanks")
            >>> result.intent
            'filmes_por_ator'
            >>> result.confidence
            0.92
        """
        original_text = text.strip()
        corrected_text = None
        text_to_process = original_text
        
        # 0. VERIFICAÇÃO PRIORITÁRIA ANTES DA CORREÇÃO ORTOGRÁFICA
        # Palavras-chave de intents simples devem ser verificadas
        # ANTES da correção para evitar correções indesejadas
        original_lower = original_text.lower().strip()
        original_normalized = normalize_text(original_text)  # Sem acentos
        
        # 0.0 VERIFICAR SE É BUSCA POR ANO (números não devem ser corrigidos para nomes)
        year_match = re.search(r'\b(19\d{2}|20[0-3]\d)\b', original_lower)
        if year_match and ('filme' in original_lower or 'lançad' in original_lower or 'ano' in original_lower):
            ano = year_match.group(1)
            logger.debug(f"Intent de ano detectado: filmes_por_ano, ano={ano}")
            return NLUResult(
                intent="filmes_por_ano",
                entities={"ano": ano},
                confidence=0.95,
                original_text=original_text,
                corrected_text=None
            )
        
        # 0.1 Intents com match exato (ajuda, saudacao, identidade, despedida)
        exact_match_intents = ["ajuda", "saudacao", "identidade", "despedida"]
        for intent_name in exact_match_intents:
            if intent_name in self.intent_patterns:
                pattern = self.intent_patterns[intent_name]
                for keyword in pattern["keywords"]:
                    keyword_normalized = normalize_text(keyword)
                    # Match com e sem acentos
                    if (original_lower == keyword or 
                        original_normalized == keyword_normalized or
                        original_lower.startswith(keyword + " ") or 
                        original_normalized.startswith(keyword_normalized + " ") or
                        original_lower.endswith(" " + keyword) or 
                        original_normalized.endswith(" " + keyword_normalized) or
                        keyword in original_lower or
                        keyword_normalized in original_normalized):
                        logger.debug(f"Intent prioritário (exact) detectado: '{intent_name}' via keyword '{keyword}'")
                        return NLUResult(
                            intent=intent_name,
                            entities={},
                            confidence=0.95,
                            original_text=original_text,
                            corrected_text=None
                        )
        
        # 0.2 Intents com match por keyword contida (filme_aleatorio, recomendar_filme, small_talk)
        # Inclui intents de filme para evitar que a correção ortográfica distorça títulos.
        keyword_match_intents = [
            "filme_aleatorio",
            "recomendar_filme",
            "small_talk",
            "diretor_do_filme",
            "atores_do_filme",
            "genero_do_filme",
            "filmes_por_diretor",
        ]
        for intent_name in keyword_match_intents:
            if intent_name in self.intent_patterns:
                pattern = self.intent_patterns[intent_name]
                for keyword in pattern["keywords"]:
                    keyword_normalized = normalize_text(keyword)
                    if keyword in original_lower or keyword_normalized in original_normalized:
                        entities = {}
                        
                        # Para intents ligadas a filme, extrair a entidade título antes de corrigir texto
                        if intent_name in ["diretor_do_filme", "atores_do_filme", "genero_do_filme"]:
                            filme_candidate = None
                            
                            # Estratégia 1: Busca após o keyword primeiro (mais confiável)
                            matched_keyword = keyword if keyword in original_lower else keyword_normalized
                            search_text = original_lower if keyword in original_lower else original_normalized
                            
                            if matched_keyword in search_text:
                                idx = search_text.find(matched_keyword)
                                after_keyword = original_text[idx + len(matched_keyword):].strip()
                                
                                # Remove preposições iniciais se houver
                                preps = ["de ", "do ", "da ", "em ", "no ", "na "]
                                for prep in preps:
                                    if after_keyword.lower().startswith(prep):
                                        after_keyword = after_keyword[len(prep):].strip()
                                        break
                                
                                filme_candidate = after_keyword.rstrip("?")
                                logger.debug(f"Filme extraído após keyword '{matched_keyword}': '{filme_candidate}'")
                            
                            # Estratégia 2: Se não extraiu nada, busca última preposição
                            if not filme_candidate or len(filme_candidate) < 2:
                                preps = [" de ", " do ", " da "]  # Com espaço antes para evitar "em" em "quem"
                                for prep in preps:
                                    if prep in original_lower:
                                        idx = original_lower.rfind(prep)
                                        filme_candidate = original_text[idx + len(prep):].strip().rstrip("?")
                                        logger.debug(f"Filme extraído via preposição '{prep}': '{filme_candidate}'")
                                        break
                            
                            if filme_candidate:
                                entities["filme"] = filme_candidate

                        if intent_name == "filmes_por_diretor":
                            director_candidate = None

                            matched_keyword = keyword if keyword in original_lower else keyword_normalized
                            search_text_lower = original_lower if keyword in original_lower else original_normalized

                            if matched_keyword in search_text_lower:
                                idx = search_text_lower.find(matched_keyword)
                                after_keyword = original_text[idx + len(matched_keyword):].strip()

                                # Remove prefixos comuns antes do nome do diretor
                                prefixes = [
                                    "do diretor ", "da diretora ", "do direitor ", "da direitora ",
                                    "do ", "da ", "por ", "pelo ", "pela ", "de ", "dirigido por ",
                                ]
                                for pref in prefixes:
                                    if after_keyword.lower().startswith(pref):
                                        after_keyword = after_keyword[len(pref):].strip()
                                        break

                                director_candidate = after_keyword.rstrip("?")

                            # Estratégia 2: Se não extraiu nada, busca última preposição
                            if not director_candidate or len(director_candidate) < 2:
                                preps = [" por ", " de ", " do ", " da ", " pelo ", " pela "]
                                for prep in preps:
                                    if prep in original_lower:
                                        idx = original_lower.rfind(prep)
                                        director_candidate = original_text[idx + len(prep):].strip().rstrip("?")
                                        break

                            if director_candidate:
                                entities["diretor"] = director_candidate
                        
                        # Para recomendar_filme, extrair gênero se presente
                        if intent_name == "recomendar_filme":
                            for prep in ["de ", "do ", "da "]:
                                if prep in original_lower:
                                    idx = original_lower.find(prep)
                                    genero_candidate = original_lower[idx + len(prep):].strip().rstrip("?")
                                    # Remove palavras comuns
                                    for word in ["um ", "uma ", "filme ", "filmes "]:
                                        genero_candidate = genero_candidate.replace(word, "").strip()
                                    if genero_candidate and genero_candidate not in ["um", "uma", "filme", "filmes"]:
                                        entities["genero"] = genero_candidate
                                    break
                        
                        logger.debug(f"Intent prioritário (keyword) detectado: '{intent_name}' via keyword '{keyword}', entities={entities}")
                        return NLUResult(
                            intent=intent_name,
                            entities=entities,
                            confidence=0.90,
                            original_text=original_text,
                            corrected_text=None
                        )
        
        # 1. Aplicar correção ortográfica (se disponível)
        if apply_spell_correction and self._spell_corrector and self._spell_corrector.is_initialized:
            corrected, was_corrected = self._spell_corrector.correct(original_text)
            if was_corrected:
                corrected_text = corrected
                text_to_process = corrected
                logger.debug(f"Correção ortográfica: '{original_text}' -> '{corrected}'")
        
        text_lower = text_to_process.lower().strip()
        doc = self.nlp(text_to_process)
        
        # 2. Detectar intenção
        intent, intent_confidence = self._detect_intent(text_lower, doc)
        
        # 3. Extrair entidades baseado no tipo de intenção
        entities = self._extract_entities(text_to_process, doc, intent)
        
        # 4. Calcular confiança geral
        overall_confidence = self._calculate_overall_confidence(
            intent_confidence, entities, intent
        )
        
        result = NLUResult(
            intent=intent,
            entities=entities,
            confidence=overall_confidence,
            original_text=original_text,
            corrected_text=corrected_text
        )
        
        logger.debug(f"NLU Parse: intent={intent}, confidence={overall_confidence:.2f}, entities={entities}")
        return result
    
    def parse_dict(self, text: str, apply_spell_correction: bool = True) -> Dict:
        """
        Versão do parse que retorna Dict (para compatibilidade com código existente).
        
        Args:
            text: Texto do usuário para análise
            apply_spell_correction: Se True, aplica correção ortográfica
            
        Returns:
            Dict com intent, entities, confidence, original_text
        """
        result = self.parse(text, apply_spell_correction)
        return {
            "intent": result.intent,
            "entities": result.entities,
            "confidence": result.confidence,
            "original_text": result.original_text,
            "corrected_text": result.corrected_text
        }
    
    def _calculate_overall_confidence(
        self, 
        intent_confidence: float, 
        entities: Dict[str, str],
        intent: str
    ) -> float:
        """
        Calcula a confiança geral baseada em múltiplos fatores.
        
        Fatores considerados:
        - Confiança da detecção de intenção (peso 0.6)
        - Presença de entidades esperadas (peso 0.3)
        - Qualidade das entidades extraídas (peso 0.1)
        
        Args:
            intent_confidence: Confiança da detecção de intenção
            entities: Entidades extraídas
            intent: Nome da intenção detectada
            
        Returns:
            Score de confiança entre 0.0 e 1.0
        """
        # Peso base da detecção de intenção
        weighted_intent = intent_confidence * 0.6
        
        # Peso das entidades
        entity_score = 0.0
        if intent in self.intent_patterns:
            pattern = self.intent_patterns[intent]
            entity_type = pattern.get("entity_type", "NONE")
            
            if entity_type == "NONE":
                # Intenções sem entidade (ex: filme aleatório)
                entity_score = 1.0
            elif entities:
                # Entidade extraída com sucesso
                entity_score = 1.0
            else:
                # Esperava entidade mas não encontrou
                entity_score = 0.3
        else:
            # Intent desconhecido
            entity_score = 0.5 if entities else 0.2
        
        weighted_entity = entity_score * 0.3
        
        # Peso da qualidade (tamanho razoável das entidades)
        quality_score = 0.5
        if entities:
            avg_len = sum(len(v) for v in entities.values()) / len(entities)
            if avg_len >= 3:  # Entidades com pelo menos 3 caracteres
                quality_score = 1.0
            elif avg_len >= 2:
                quality_score = 0.7
        
        weighted_quality = quality_score * 0.1
        
        overall = weighted_intent + weighted_entity + weighted_quality
        return min(1.0, max(0.0, overall))
    
    def _detect_intent(self, text_lower: str, doc) -> Tuple[str, float]:
        """
        Detecta a intenção principal do texto.
        
        Usa validação de entidades para desambiguar entre intenções similares:
        - Se encontrar um gênero válido, prioriza filmes_por_genero
        - Caso contrário, usa matching de padrões tradicional
        
        Returns:
            Tuple com (intent_name, confidence_score)
        """
        best_intent = "unknown"
        best_score = 0.0
        
        # ===== VERIFICAÇÃO PRIORITÁRIA: Intents simples (ajuda, saudacao) =====
        # Estas intents têm precedência quando há match exato das keywords
        priority_intents = [
            "ajuda",
            "saudacao",
            "contar_filmes",
            "atores_do_filme",
            "diretor_do_filme",
            "genero_do_filme",
            "filmes_por_diretor",
        ]
        for intent_name in priority_intents:
            if intent_name in self.intent_patterns:
                pattern = self.intent_patterns[intent_name]
                for keyword in pattern["keywords"]:
                    # Match exato ou texto começa com a keyword
                    if text_lower == keyword or text_lower.startswith(keyword + " ") or text_lower.endswith(" " + keyword):
                        logger.debug(f"Intent prioritário detectado: '{intent_name}' via keyword '{keyword}'")
                        return intent_name, 1.0
        
        # ===== PRÉ-VALIDAÇÃO: Verificar se há gênero válido no texto =====
        # Extrai possível entidade após preposição para validar
        potential_entity = self._extract_entity_candidate(text_lower)
        has_valid_genre = False
        
        if potential_entity:
            has_valid_genre = is_valid_genre(potential_entity)
            if has_valid_genre:
                logger.debug(f"Gênero válido detectado: '{potential_entity}'")
        
        for intent_name, pattern in self.intent_patterns.items():
            # Skip priority intents já verificados
            if intent_name in priority_intents:
                continue
                
            score = 0.0
            matches = 0
            
            # Verifica presença de keywords
            for keyword in pattern["keywords"]:
                # [FIX] Usa regex boundary para evitar substring matching parcial e duplicado
                if re.search(r'\b' + re.escape(keyword) + r'\b', text_lower):
                    matches += 1
                    score += 0.4
                    
                    # [FIX] Boost para keywords fortes (verbos/interrogativos) para desambiguação
                    # Isso garante que "quantos filmes..." vença "filmes..."
                    if keyword in ["quantos", "contar", "recomendar", "sugira", "ajuda", "quem", "qual", "gênero", "genero", "tipo", "categoria"]:
                        score += 0.3
            
            # Verifica presença de preposições (se houver)
            if pattern["prepositions"]:
                for prep in pattern["prepositions"]:
                    if prep in text_lower:
                        score += 0.2
                        break
            
            # Bônus se encontrou pelo menos uma keyword
            if matches > 0:
                score += 0.2
            
            # ===== DESAMBIGUAÇÃO: Ajustar scores baseado em validação =====
            # ===== DESAMBIGUAÇÃO: Ajustar scores baseado em validação =====
            if has_valid_genre:
                # Se há gênero válido, prioriza intenções que usam GENRE ou MIXED
                entity_type = pattern.get("entity_type", "NONE")
                if entity_type in ["GENRE", "MIXED", "MULTI_GENRE", "GENRE_YEAR"]:
                    score += 0.3  # Bônus moderado para intents compatíveis
                
                # Se a intenção for especificamente filmes_por_genero E não houver keywords de outras ações
                if intent_name == "filmes_por_genero":
                    # Só dá boost extra se não tiver keywords de contagem/recomendação explícita noutros lugares
                    score += 0.1
                elif intent_name == "filmes_por_ator":
                    score -= 0.3  # Penaliza intenção de ator se só achou gênero
            
            # Normaliza score (apenas limite inferior)
            score = max(score, 0.0)
            
            logger.debug(f"Intent '{intent_name}': score={score:.2f} (matches={matches})")
            
            if score > best_score:
                best_score = score
                best_intent = intent_name
        
        return best_intent, best_score
    
    def _extract_entity_candidate(self, text_lower: str) -> Optional[str]:
        """
        Extrai candidato a entidade do texto para validação prévia.
        Busca texto após preposições comuns.
        
        Returns:
            String candidata a entidade ou None
        """
        prepositions = ["de", "do", "da", "por", "com"]
        
        for prep in prepositions:
            pattern = f" {prep} "
            if pattern in text_lower:
                idx = text_lower.find(pattern)
                after_prep = text_lower[idx + len(pattern):].strip()
                
                # Remove palavras comuns no final
                stop_words = [" em ", " e ", " ou ", " que "]
                for stop in stop_words:
                    if stop in after_prep:
                        after_prep = after_prep[:after_prep.find(stop)].strip()
                
                if after_prep and len(after_prep) >= 2:
                    return after_prep
        
        return None
    
    def _extract_entities(self, text: str, doc, intent: str) -> Dict[str, str]:
        """
        Extrai entidades do texto baseado no tipo de intenção.
        
        Returns:
            Dicionário de entidades {tipo: valor}
        """
        entities = {}
        
        if intent not in self.intent_patterns:
            return entities
        
        pattern = self.intent_patterns[intent]
        entity_type = pattern.get("entity_type", "NONE")
        
        if entity_type == "NONE":
            return entities
        
        # Para PERSON (ator/diretor): extrai nome próprio
        if entity_type == "PERSON":
            # Tenta extrair entidades PERSON do spaCy
            persons = [ent.text for ent in doc.ents if ent.label_ == "PER"]
            
            if persons:
                if "ator" in intent or "diretor" in intent:
                    key = "ator" if "ator" in intent else "diretor"
                    entities[key] = persons[0]
            else:
                # Fallback: extrai texto após preposição
                key = "diretor" if "diretor" in intent else "ator"
                entities = self._extract_after_preposition(text, pattern["prepositions"], key)
        
        # Para GENRE: extrai gênero
        elif entity_type == "GENRE":
            entities = self._extract_after_preposition(text, pattern["prepositions"], "genero")
        
        # Para MOVIE: extrai nome do filme
        elif entity_type == "MOVIE":
            entities = self._extract_after_preposition(text, pattern["prepositions"], "filme")
        
        # Para MIXED: pode ter múltiplas entidades
        elif entity_type == "MIXED":
            # Tenta extrair ator
            ator_entities = self._extract_after_preposition(text, ["por", "com", "do"], "ator")
            entities.update(ator_entities)
            
            # Tenta extrair gênero (busca mais agressiva)
            text_lower = text.lower()
            genero_entities = self._extract_after_preposition(text_lower, ["de", "do", "da"], "genero")
            
            # [FIX] Validar se o gênero extraído é realmente um gênero válido
            if "genero" in genero_entities:
                potential_genre = genero_entities["genero"]
                # Remove palavras comuns que não são gêneros
                if potential_genre not in ["um", "uma", "filme", "filmes", "algum"]:
                    from .nlu import is_valid_genre
                    if is_valid_genre(potential_genre):
                        entities.update(genero_entities)
                    else:
                        # Tenta buscar gênero em qualquer parte do texto
                        from .nlu import GENRE_CACHE
                        for genre in GENRE_CACHE:
                            if genre.lower() in text_lower:
                                entities["genero"] = genre
                                break
            
            # [FIX] Tenta extrair ano para intents como contar_filmes
            year_match = re.search(r'\b(19|20)\d{2}\b', text)
            if year_match:
                entities["ano"] = year_match.group()
        
        # Para MULTI_GENRE: extrai dois gêneros
        elif entity_type == "MULTI_GENRE":
            entities = self._extract_multi_genre(text)
        
        # Para GENRE_YEAR: extrai gênero e ano
        elif entity_type == "GENRE_YEAR":
            entities = self._extract_genre_year(text)
        
        return entities
    
    def _extract_after_preposition(self, text: str, prepositions: List[str], entity_key: str) -> Dict[str, str]:
        """
        Extrai texto que vem após uma preposição.
        
        Returns:
            Dict com {entity_key: extracted_text}
        """
        text_lower = text.lower()
        
        for prep in prepositions:
            pattern = f" {prep} "
            if pattern in text_lower:
                # Encontra posição da preposição
                idx = text_lower.find(pattern)
                # Extrai tudo após a preposição
                after_prep = text[idx + len(pattern):].strip()
                
                # Remove palavras comuns no final
                stop_words = [" em ", " e ", " ou ", " que ", " de "]
                for stop in stop_words:
                    if stop in after_prep.lower():
                        after_prep = after_prep[:after_prep.lower().find(stop)].strip()
                
                if after_prep:
                    return {entity_key: after_prep}
        
        return {}
    
    def _extract_multi_genre(self, text: str) -> Dict[str, str]:
        """
        Extrai dois gêneros de uma query (ex: "filmes de ação e comédia").
        
        Returns:
            Dict com {genero1: valor, genero2: valor}
        """
        entities = {}
        text_lower = text.lower()
        
        # Padrão: "de {genero1} e {genero2}"
        if " de " in text_lower and " e " in text_lower:
            try:
                de_idx = text_lower.find(" de ")
                e_idx = text_lower.find(" e ", de_idx)
                
                if de_idx < e_idx:
                    genero1 = text[de_idx + 4:e_idx].strip()
                    genero2 = text[e_idx + 3:].strip()
                    
                    # Remove stop words do final do genero2
                    for stop in [" em ", " com ", " por "]:
                        if stop in genero2.lower():
                            genero2 = genero2[:genero2.lower().find(stop)].strip()
                    
                    if genero1:
                        entities["genero1"] = genero1
                    if genero2:
                        entities["genero2"] = genero2
            except Exception:
                pass
        
        return entities
    
    def _extract_genre_year(self, text: str) -> Dict[str, str]:
        """
        Extrai gênero e ano de uma query (ex: "filmes de ação em 2023").
        
        Returns:
            Dict com {genero: valor, ano: valor}
        """
        import re
        
        entities = {}
        text_lower = text.lower()
        
        # Extrai ano (4 dígitos)
        year_match = re.search(r'\b(19|20)\d{2}\b', text)
        if year_match:
            entities["ano"] = year_match.group()
        
        # Extrai gênero (entre "de" e "em")
        if " de " in text_lower:
            de_idx = text_lower.find(" de ")
            # Encontra onde termina o gênero
            end_idx = len(text)
            for marker in [" em ", " no ", " de "]:
                if marker in text_lower[de_idx + 4:]:
                    end_idx = de_idx + 4 + text_lower[de_idx + 4:].find(marker)
                    break
            
            genero = text[de_idx + 4:end_idx].strip()
            if genero:
                entities["genero"] = genero
        
        return entities
    
    def get_suggestions(self, text: str, intent: Optional[str] = None, top_n: int = 5) -> List[str]:
        """
        Retorna sugestões de queries baseadas no texto e intenção.
        
        Para casos de baixa confiança, retorna exemplos organizados por categoria.
        
        Args:
            text: Texto do usuário
            intent: Intenção detectada (opcional, para filtrar sugestões)
            top_n: Número de sugestões a retornar
            
        Returns:
            Lista de queries de exemplo
        """
        suggestions = []
        
        if intent and intent in self.intent_patterns:
            # Retorna exemplos específicos da intenção detectada
            pattern = self.intent_patterns[intent]
            examples = pattern.get("examples", [])
            suggestions.extend(examples[:top_n])
        else:
            # Retorna exemplos variados de diferentes intenções
            for intent_name, pattern in self.intent_patterns.items():
                examples = pattern.get("examples", [])
                if examples:
                    suggestions.append(examples[0])
                if len(suggestions) >= top_n:
                    break
        
        return suggestions[:top_n]
    
    def get_similar_queries(self, text: str, top_n: int = 3) -> List[str]:
        """
        Retorna queries de exemplo similares ao texto fornecido.
        
        Args:
            text: Texto do usuário
            top_n: Número de sugestões a retornar
            
        Returns:
            Lista de queries de exemplo
        """
        all_examples = []
        
        # Coleta todos os exemplos de todos os intents
        for intent_name, pattern in self.intent_patterns.items():
            examples = pattern.get("examples", [])
            for example in examples:
                similarity = fuzz.ratio(text.lower(), example.lower())
                all_examples.append((example, similarity))
        
        # Ordena por similaridade e retorna top N
        all_examples.sort(key=lambda x: x[1], reverse=True)
        return [ex[0] for ex in all_examples[:top_n]]
    
    def get_help_by_category(self) -> Dict[str, List[str]]:
        """
        Retorna exemplos de queries organizados por categoria.
        
        Útil para mostrar ajuda quando a confiança é muito baixa.
        
        Returns:
            Dict com {categoria: [exemplos]}
        """
        help_dict = {}
        
        category_map = {
            "filmes_por_ator": "Por ator",
            "filmes_por_genero": "Por gênero",
            "filmes_por_diretor": "Por diretor",
            "genero_do_filme": "Gênero de um filme",
            "filme_aleatorio": "Filme aleatório",
            "recomendar_ator_e_genero": "Por ator e gênero",
            "recomendar_dois_generos": "Por dois gêneros",
            "contar_filmes": "Contar filmes",
        }
        
        for intent_name, pattern in self.intent_patterns.items():
            category = category_map.get(intent_name, intent_name)
            examples = pattern.get("examples", [])[:2]  # Max 2 exemplos por categoria
            
            if examples:
                help_dict[category] = examples
        
        return help_dict
    
    def is_low_confidence(self, confidence: float) -> bool:
        """Verifica se a confiança é baixa (< 0.4)."""
        return confidence < self.MEDIUM_CONFIDENCE_THRESHOLD
    
    def is_high_confidence(self, confidence: float) -> bool:
        """Verifica se a confiança é alta (≥ 0.7)."""
        return confidence >= self.HIGH_CONFIDENCE_THRESHOLD


# Singleton instance (lazy loaded)
_nlu_instance: Optional[NLUEngine] = None


def get_nlu_engine() -> NLUEngine:
    """
    Retorna a instância singleton do NLU Engine.
    
    Returns:
        Instância do NLUEngine
    """
    global _nlu_instance
    if _nlu_instance is None:
        _nlu_instance = NLUEngine()
    return _nlu_instance


def initialize_nlu_engine(spell_corrector=None) -> NLUEngine:
    """
    Inicializa o singleton do NLU Engine com configurações.
    
    Args:
        spell_corrector: Instância do SpellCorrector para correção ortográfica
        
    Returns:
        Instância inicializada do NLUEngine
    """
    global _nlu_instance
    _nlu_instance = NLUEngine(spell_corrector=spell_corrector)
    return _nlu_instance
