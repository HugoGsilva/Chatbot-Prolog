"""
NLU leve para normalização de entidades e fuzzy matching.

Inclui funções de normalização e helpers para encontrar a melhor
correspondência usando thefuzz contra caches carregadas no startup.
"""

from thefuzz import process

from typing import Optional
import json

# --- CACHES NLU GLOBAIS ---
# (Definidas aqui, mas populadas pelo main.py no startup)
ACTOR_CACHE: list[str] = []
GENRE_CACHE: list[str] = []
FILM_CACHE: list[str] = []
DIRECTOR_CACHE: list[str] = []

GENRE_TRANSLATION_MAP = {
    # ===== GÊNEROS SIMPLES (mais comuns nas buscas) =====
    "ACAO": "ACTION & ADVENTURE",
    "AÇÃO": "ACTION & ADVENTURE",
    "ACTION": "ACTION & ADVENTURE",
    "AVENTURA": "ACTION & ADVENTURE",
    "COMEDIA": "COMEDIES",
    "COMÉDIA": "COMEDIES",
    "COMEDY": "COMEDIES",
    "DRAMA": "DRAMAS",
    "DRAMAS": "DRAMAS",
    "TERROR": "HORROR MOVIES",
    "HORROR": "HORROR MOVIES",
    "SUSPENSE": "THRILLERS",
    "THRILLER": "THRILLERS",
    "ROMANCE": "ROMANTIC MOVIES",
    "ROMANTICO": "ROMANTIC MOVIES",
    "ROMÂNTICO": "ROMANTIC MOVIES",
    "FICÇÃO CIENTÍFICA": "SCI-FI & FANTASY",
    "FICCAO CIENTIFICA": "SCI-FI & FANTASY",
    "SCI-FI": "SCI-FI & FANTASY",
    "SCIFI": "SCI-FI & FANTASY",
    "FANTASIA": "SCI-FI & FANTASY",
    "FANTASY": "SCI-FI & FANTASY",
    "DOCUMENTARIO": "DOCUMENTARIES",
    "DOCUMENTÁRIO": "DOCUMENTARIES",
    "DOCUMENTARY": "DOCUMENTARIES",
    "ANIME": "ANIME FEATURES",
    "ANIMACAO": "ANIME FEATURES",
    "ANIMAÇÃO": "ANIME FEATURES",
    "MUSICAL": "MUSIC & MUSICALS",
    "CRIME": "CRIME TV SHOWS",
    "POLICIAL": "CRIME TV SHOWS",
    "MISTERIO": "TV MYSTERIES",
    "MISTÉRIO": "TV MYSTERIES",
    "MYSTERY": "TV MYSTERIES",
    "INFANTIL": "CHILDREN & FAMILY MOVIES",
    "KIDS": "KIDS' TV",
    "CRIANCA": "CHILDREN & FAMILY MOVIES",
    "CRIANÇA": "CHILDREN & FAMILY MOVIES",
    "FAMILIA": "CHILDREN & FAMILY MOVIES",
    "FAMÍLIA": "CHILDREN & FAMILY MOVIES",
    "FAMILY": "CHILDREN & FAMILY MOVIES",
    "ESPORTE": "SPORTS MOVIES",
    "SPORTS": "SPORTS MOVIES",
    
    # ===== GÊNEROS COMPOSTOS =====
    # Action & Adventure
    "AÇÃO E AVENTURA (FILMES)": "ACTION & ADVENTURE", # (Compound, para o protótipo)
    "FILME DE AÇÃO": "ACTION & ADVENTURE",
    "FILME DE AVENTURA": "ACTION & ADVENTURE",
    "AÇÃO (FILME)": "ACTION & ADVENTURE",
    "AVENTURA (FILME)": "ACTION & ADVENTURE",
    
    # Anime Features
    "FILMES DE ANIME": "ANIME FEATURES",
    "FILME DE ANIME": "ANIME FEATURES",
    "LONGA DE ANIME": "ANIME FEATURES",
    "LONGA-METRAGEM DE ANIME": "ANIME FEATURES",
    "ANIME (FILME)": "ANIME FEATURES",
    
    # Anime Series
    "SÉRIES DE ANIME": "ANIME SERIES",
    "SÉRIE DE ANIME": "ANIME SERIES",
    "ANIMES": "ANIME SERIES",
    "SERIADO DE ANIME": "ANIME SERIES",
    "ANIME (SÉRIE)": "ANIME SERIES",
    
    # British TV Shows
    "SÉRIES BRITÂNICAS": "BRITISH TV SHOWS",
    "PROGRAMAS BRITÂNICOS": "BRITISH TV SHOWS",
    "TV INGLESA": "BRITISH TV SHOWS",
    "SÉRIES DA INGLATERRA": "BRITISH TV SHOWS",
    
    # Children & Family Movies
    "FILMES PARA CRIANÇAS E FAMÍLIA": "CHILDREN & FAMILY MOVIES",
    "FILME INFANTIL": "CHILDREN & FAMILY MOVIES",
    "FILME PRA FAMÍLIA": "CHILDREN & FAMILY MOVIES",
    "FILME DE CRIANÇA": "CHILDREN & FAMILY MOVIES",
    "INFANTIL": "CHILDREN & FAMILY MOVIES",
    "FAMÍLIA": "CHILDREN & FAMILY MOVIES", # (Nota: 'Family' também é um género atómico)
    
    # Classic & Cult TV
    "TV CLÁSSICA E CULT": "CLASSIC & CULT TV",
    "SÉRIES CLÁSSICAS": "CLASSIC & CULT TV",
    "SÉRIES CULT": "CLASSIC & CULT TV",     
    "PROGRAMAS ANTIGOS": "CLASSIC & CULT TV",
    "TV ANTIGA": "CLASSIC & CULT TV",
    
    # Classic Movies
    "FILMES CLÁSSICOS": "CLASSIC MOVIES",
    "CLÁSSICOS DO CINEMA": "CLASSIC MOVIES",
    "FILME ANTIGO": "CLASSIC MOVIES",
    
    # Comedies
    "COMÉDIAS (FILMES)": "COMEDIES",
    "FILME DE COMÉDIA": "COMEDIES",
    "COMÉDIA (FILME)": "COMEDIES",
    "FILME ENGRAÇADO": "COMEDIES",
    "FILME PRA RIR": "COMEDIES",
    
    # Crime TV Shows
    "SÉRIES POLICIAIS": "CRIME TV SHOWS",
    "SÉRIE POLICIAL": "CRIME TV SHOWS",
    "SÉRIE DE CRIME": "CRIME TV SHOWS",
    "INVESTIGAÇÃO": "CRIME TV SHOWS",
    "SÉRIE DE INVESTIGAÇÃO": "CRIME TV SHOWS",
    
    # Cult Movies
    "FILMES CULT": "CULT MOVIES",
    "FILME CULT": "CULT MOVIES",
    
    # Documentaries
    "DOCUMENTÁRIOS": "DOCUMENTARIES",
    "DOCUMENTÁRIO": "DOCUMENTARIES",
    "DOC": "DOCUMENTARIES",
    "DOCS": "DOCUMENTARIES",
    
    # Docuseries
    "DOCUMENTÁRIOS (SÉRIES)": "DOCUSERIES",
    "SÉRIE DOCUMENTAL": "DOCUSERIES",
    "DOCUSSÉRIE": "DOCUSERIES",
    "DOCSÉRIES": "DOCUSERIES",
    "DOCUMENTÁRIO (SÉRIE)": "DOCUSERIES",
    
    # Dramas
    "DRAMAS (FILMES)": "DRAMAS",
    "FILME DE DRAMA": "DRAMAS",
    "DRAMA (FILME)": "DRAMAS",
    
    # Faith & Spirituality
    "FÉ E ESPIRITUALIDADE": "FAITH & SPIRITUALITY",
    "FILME RELIGIOSO": "FAITH & SPIRITUALITY",
    "FILME ESPIRITUAL": "FAITH & SPIRITUALITY",
    "FILME GOSPEL": "FAITH & SPIRITUALITY",
    "FÉ": "FAITH & SPIRITUALITY",
    "RELIGIÃO": "FAITH & SPIRITUALITY",
    
    # Horror Movies
    "FILMES DE TERROR": "HORROR MOVIES",
    "FILME DE TERROR": "HORROR MOVIES",
    "TERROR (FILME)": "HORROR MOVIES",
    "FILME DE SUSTO": "HORROR MOVIES",
    "FILME ASSUSTADOR": "HORROR MOVIES",
    
    # Independent Movies
    "FILMES INDEPENDENTES": "INDEPENDENT MOVIES",
    "FILME INDEPENDENTE": "INDEPENDENT MOVIES",
    "FILME INDIE": "INDEPENDENT MOVIES",
    "CINEMA INDEPENDENTE": "INDEPENDENT MOVIES",
    "INDIE": "INDEPENDENT MOVIES",
    
    # International Movies
    "FILMES INTERNACIONAIS": "INTERNATIONAL MOVIES",
    "FILME INTERNACIONAL": "INTERNATIONAL MOVIES",
    "FILME ESTRANGEIRO": "INTERNATIONAL MOVIES",
    "CINEMA MUNDIAL": "INTERNATIONAL MOVIES",
    "FILME DE FORA": "INTERNATIONAL MOVIES",
    
    # International TV Shows
    "SÉRIES INTERNACIONAIS": "INTERNATIONAL TV SHOWS",
    "SÉRIE INTERNACIONAL": "INTERNATIONAL TV SHOWS",
    "SÉRIE ESTRANGEIRA": "INTERNATIONAL TV SHOWS",
    "PROGRAMA INTERNACIONAL": "INTERNATIONAL TV SHOWS",
    
    # Kids' TV
    "TV INFANTIL": "KIDS' TV",
    "SÉRIE INFANTIL": "KIDS' TV",
    "DESENHO": "KIDS' TV",
    "DESENHO ANIMADO": "KIDS' TV",
    "PROGRAMA PRA CRIANÇA": "KIDS' TV",
    
    # Korean TV Shows
    "SÉRIES COREANAS": "KOREAN TV SHOWS",
    "SÉRIE COREANA": "KOREAN TV SHOWS",
    "DORAMA": "KOREAN TV SHOWS",
    "K-DRAMA": "KOREAN TV SHOWS",
    "NOVELA COREANA": "KOREAN TV SHOWS",
    
    # LGBTQ Movies
    "FILMES LGBTQ": "LGBTQ MOVIES",
    "FILME LGBT": "LGBTQ MOVIES",
    "FILME GAY": "LGBTQ MOVIES",
    "TEMÁTICA LGBT": "LGBTQ MOVIES",
    "CINEMA QUEER": "LGBTQ MOVIES",
    
    # Movies
    "FILMES": "MOVIES",
    "FILME": "MOVIES",
    "LONGA": "MOVIES",
    "LONGA-METRAGEM": "MOVIES",
    
    # Music & Musicals
    "MÚSICA E MUSICAIS": "MUSIC & MUSICALS",
    "MUSICAL": "MUSIC & MUSICALS",
    "FILME MUSICAL": "MUSIC & MUSICALS",
    "FILME DE MÚSICA": "MUSIC & MUSICALS",
    "SHOWS": "MUSIC & MUSICALS",
    "CONCERTOS": "MUSIC & MUSICALS",
    
    # Reality TV
    "REALITY TV": "REALITY TV",
    "REALITY SHOW": "REALITY TV",
    "REALITY": "REALITY TV",
    "PROGRAMA DE REALIDADE": "REALITY TV",
    
    # Romantic Movies
    "FILMES ROMÂNTICOS": "ROMANTIC MOVIES",
    "FILME DE ROMANCE": "ROMANTIC MOVIES",
    "FILME ROMÂNTICO": "ROMANTIC MOVIES",
    "ROMANCE (FILME)": "ROMANTIC MOVIES",
    "FILME DE AMOR": "ROMANTIC MOVIES",
    
    # Romantic TV Shows
    "SÉRIES ROMÂNTICAS": "ROMANTIC TV SHOWS",
    "SÉRIE DE ROMANCE": "ROMANTIC TV SHOWS",
    "SÉRIE ROMÂNTICA": "ROMANTIC TV SHOWS",
    "ROMANCE (SÉRIE)": "ROMANTIC TV SHOWS",
    "SÉRIE DE AMOR": "ROMANTIC TV SHOWS",
    
    # Sci-Fi & Fantasy
    "FICÇÃO CIENTÍFICA E FANTASIA (FILMES)": "SCI-FI & FANTASY",
    "FILME DE FICÇÃO CIENTÍFICA": "SCI-FI",
    "FILME DE FANTASIA": "FANTASY",
    "SCI-FI (FILME)": "SCI-FI",
    "FANTASIA (FILME)": "FANTASY",
    
    # Science & Nature TV
    "TV SOBRE CIÊNCIA E NATUREZA": "SCIENCE & NATURE TV",
    "DOCUMENTÁRIO DE NATUREZA": "SCIENCE & NATURE TV",
    "PROGRAMA DE CIÊNCIA": "SCIENCE & NATURE TV",
    "NATUREZA": "SCIENCE & NATURE TV",
    "ANIMAIS": "SCIENCE & NATURE TV",
    
    # Spanish-Language TV Shows
    "SÉRIES EM ESPANHOL": "SPANISH-LANGUAGE TV SHOWS",
    "SÉRIE EM ESPANHOL": "SPANISH-LANGUAGE TV SHOWS",
    "SÉRIE LATINA": "SPANISH-LANGUAGE TV SHOWS",
    "PROGRAMA EM ESPANHOL": "SPANISH-LANGUAGE TV SHOWS",
    "NOVELA": "SPANISH-LANGUAGE TV SHOWS", # (Nota: "Novela Coreana" é mais específica)
    
    # Sports Movies
    "FILMES DE ESPORTE": "SPORTS MOVIES",
    "FILME DE ESPORTE": "SPORTS MOVIES",
    "FILME DE FUTEBOL": "SPORTS MOVIES",
    "ESPORTES": "SPORTS MOVIES",
    
    # Stand-Up Comedy
    "COMÉDIA STAND-UP": "STAND-UP COMEDY",
    "COMÉDIA EM PÉ": "STAND-UP COMEDY",
    # (Chaves ambíguas resolvidas para a mais específica)
    "STAND UP": "STAND-UP COMEDY",
    "STAND-UP": "STAND-UP COMEDY",
    "SHOW DE COMÉDIA": "STAND-UP COMEDY",
    
    # Stand-Up Comedy & Talk Shows
    "STAND-UP E TALK SHOWS": "STAND-UP COMEDY & TALK SHOWS",
    "TALK SHOW": "TALK SHOWS",
    "PROGRAMA DE ENTREVISTA": "TALK SHOWS",
    
    # TV Action & Adventure
    "SÉRIES DE AÇÃO E AVENTURA": "TV ACTION & ADVENTURE",
    "SÉRIE DE AÇÃO": "TV ACTION",
    "SÉRIE DE AVENTURA": "TV ADVENTURE",
    "AÇÃO (SÉRIE)": "TV ACTION",
    "AVENTURA (SÉRIE)": "TV ADVENTURE",
    
    # TV Comedies
    "SÉRIES DE COMÉDIA": "TV COMEDIES",
    "SÉRIE DE COMÉDIA": "TV COMEDIES",
    "COMÉDIA (SÉRIE)": "TV COMEDIES",
    "SÉRIE ENGRAÇADA": "TV COMEDIES",
    "SITCOM": "TV COMEDIES",
    
    # TV Dramas
    "SÉRIES DE DRAMA": "TV DRAMAS",
    "SÉRIE DE DRAMA": "TV DRAMAS",
    "DRAMA (SÉRIE)": "TV DRAMAS",
    "SERIADO DRAMÁTICO": "TV DRAMAS",
    
    # TV Horror
    "SÉRIES DE TERROR": "TV HORROR",
    "SÉRIE DE TERROR": "TV HORROR",
    "TERROR (SÉRIE)": "TV HORROR",
    "SÉRIE DE SUSTO": "TV HORROR",
    "SÉRIE ASSUSTADORA": "TV HORROR",
    
    # TV Mysteries
    "SÉRIES DE MISTÉRIO": "TV MYSTERIES",
    "SÉRIE DE MISTÉRIO": "TV MYSTERIES",
    "MISTÉRIO": "TV MYSTERIES",
    "SUSPENSE (SÉRIE)": "TV MYSTERIES", # (Nota: Thrillers é separado)
    
    # TV Sci-Fi & Fantasy
    "SÉRIES DE FICÇÃO E FANTASIA": "TV SCI-FI & FANTASY",
    "SÉRIE DE FICÇÃO CIENTÍFICA": "TV SCI-FI",
    "SÉRIE DE FANTASIA": "TV FANTASY",
    "SCI-FI (SÉRIE)": "TV SCI-FI",
    "FANTASIA (SÉRIE)": "TV FANTASY",
    
    # TV Shows
    "SÉRIES": "TV SHOWS",
    "SERIADOS": "TV SHOWS",
    "PROGRAMA DE TV": "TV SHOWS",
    "PROGRAMAS": "TV SHOWS",
    "MINISSÉRIE": "TV SHOWS",
    
    # TV Thrillers
    "SÉRIES DE SUSPENSE": "TV THRILLERS",
    "THRILLER (SÉRIE)": "TV THRILLERS",
    
    # Teen TV Shows
    "SÉRIES ADOLESCENTES": "TEEN TV SHOWS",
    "SÉRIE ADOLESCENTE": "TEEN TV SHOWS",
    "SÉRIE TEEN": "TEEN TV SHOWS",
    "SÉRIE JOVEM": "TEEN TV SHOWS",
    "PARA ADOLESCENTES": "TEEN TV SHOWS",
    
    # Thrillers
    "SUSPENSE (FILMES)": "THRILLERS",
    "FILME DE SUSPENSE": "THRILLERS",
    "SUSPENSE (FILME)": "THRILLERS",
    "THRILLER": "THRILLERS",
}

# (2) A CACHE FUZZY (Usada pelo Nível 2 - Backend)
# Lista de todas as chaves PT para o find_best_match
GENRE_CACHE_PT = list(GENRE_TRANSLATION_MAP.keys())

def normalize_actor_name(name: str) -> str:
    """Normaliza o nome do ator para o formato esperado pelo Prolog.

    Estratégia: converter para maiúsculas. Futuras melhorias podem incluir
    remoção de acentos, trimming, e regras de padronização adicionais.
    """
    return name.upper()


def normalize_genre_name(name: str) -> str:
    """Normaliza o nome do gênero para o formato esperado pelo Prolog.

    Estratégia: capitalizar a primeira letra e manter o restante minúsculo,
    alinhando com valores como "Action", "Comedy", "Sci-Fi".
    """
    return name.capitalize()


def normalize_film_title(title: str) -> str:
    """Normaliza o título do filme para o formato esperado pelo Prolog.

    Estratégia: converter para maiúsculas, alinhando com fatos como
    "ACADEMY DINOSAUR" no Sakila.
    """
    return title.upper()


# ---------------------------------------------------------------------------
# Fuzzy Matching Helpers
# ---------------------------------------------------------------------------

def find_best_match(query: str, cache: list[str], threshold: int = 75) -> Optional[str]:
    """
    Retorna o melhor match para `query` dentro de `cache` com validações rigorosas.
    
    Validações aplicadas:
    1. Match exato tem prioridade
    2. Fuzzy match com score mínimo
    3. Validação de proporção de tamanho (evita "TITANIC" → "ATTACK ON TITAN")
    4. Rejeita matches com diferença de tamanho > 60% e score < 85
    
    Args:
        query: String de busca
        cache: Lista de strings para buscar
        threshold: Score mínimo (padrão 75)
        
    Returns:
        Melhor match ou None se não passar nas validações
    """
    if not query or not cache:
        return None

    query_upper = query.upper().strip()
    
    # 1. Match exato tem prioridade
    if query_upper in cache:
        return query_upper
    
    # 2. Fuzzy matching
    result = process.extractOne(query_upper, cache)
    if not result:
        return None

    matched_value, score = result[0], result[1]
    
    # 3. Validação básica de score
    if score < threshold:
        return None
    
    # 4. Validação de proporção de tamanho
    # Previne matches como "TITANIC" (7) → "ATTACK ON TITAN" (15)
    len_query = len(query_upper)
    len_match = len(matched_value)
    
    if len_query > 0 and len_match > 0:
        size_ratio = min(len_query, len_match) / max(len_query, len_match)
        
        # Se diferença de tamanho > 40%, exige score muito alto
        if size_ratio < 0.6:
            if score < 85:  # Precisa ser praticamente idêntico
                return None
    
    return matched_value


def find_best_match_with_suggestions(
    query: str, 
    cache: list[str], 
    threshold: int = 75,
    max_suggestions: int = 3
) -> dict:
    """
    Versão avançada de find_best_match que retorna match + sugestões.
    
    Retorna um dict com:
    - best_match: Melhor resultado que passou nas validações (ou None)
    - confidence: Score do melhor match
    - suggestions: Lista dos top N matches alternativos
    - rejected_reason: Motivo da rejeição se best_match é None
    
    Útil para handlers mostrarem alternativas quando não há match exato.
    
    Args:
        query: String de busca
        cache: Lista de strings para buscar
        threshold: Score mínimo
        max_suggestions: Número máximo de sugestões alternativas
        
    Returns:
        Dict com best_match, confidence, suggestions e rejected_reason
    """
    if not query or not cache:
        return {
            "best_match": None,
            "confidence": 0,
            "suggestions": [],
            "rejected_reason": "Query ou cache vazio"
        }
    
    query_upper = query.upper().strip()
    
    # 1. Match exato
    if query_upper in cache:
        return {
            "best_match": query_upper,
            "confidence": 100,
            "suggestions": [],
            "rejected_reason": None
        }
    
    # 2. Top N fuzzy matches
    top_matches = process.extract(query_upper, cache, limit=max_suggestions)
    if not top_matches:
        return {
            "best_match": None,
            "confidence": 0,
            "suggestions": [],
            "rejected_reason": "Nenhum match encontrado"
        }
    
    best_title, best_score = top_matches[0]
    rejected_reason = None
    
    # 3. Validações no melhor match
    best_match = None
    
    # Validação A: Score mínimo
    if best_score >= threshold:
        # Validação B: Proporção de tamanho
        len_query = len(query_upper)
        len_match = len(best_title)
        
        if len_query > 0 and len_match > 0:
            size_ratio = min(len_query, len_match) / max(len_query, len_match)
            
            if size_ratio < 0.6 and best_score < 85:
                rejected_reason = f"Diferença de tamanho muito grande (ratio={size_ratio:.2f})"
            else:
                best_match = best_title
        else:
            best_match = best_title
    else:
        rejected_reason = f"Score muito baixo ({best_score} < {threshold})"
    
    # 4. Formata sugestões
    suggestions = [
        {"title": title, "score": score} 
        for title, score in top_matches 
        if score >= 60  # Mostra apenas sugestões razoáveis
    ]
    
    return {
        "best_match": best_match,
        "confidence": best_score,
        "suggestions": suggestions,
        "rejected_reason": rejected_reason
    }


def is_valid_genre(query: str, threshold: int = 70) -> bool:
    """
    Verifica se a query corresponde a um gênero válido.
    Usado pelo NLUEngine para decidir entre filmes_por_genero vs filmes_por_ator.
    
    Returns:
        True se a query corresponde a um gênero conhecido
    """
    if not query:
        return False
    
    query_upper = query.upper().strip()
    
    # Verifica match direto no mapa de tradução
    if query_upper in GENRE_TRANSLATION_MAP:
        return True
    
    # Verifica match fuzzy no mapa de tradução PT
    best_pt = find_best_match(query_upper, GENRE_CACHE_PT, threshold=threshold)
    if best_pt:
        return True
    
    # Verifica match direto no cache de gêneros EN
    best_en = find_best_match(query_upper, GENRE_CACHE, threshold=threshold)
    if best_en:
        return True
    
    return False


def find_best_actor(query: str) -> Optional[str]:
    """Wrapper específico para atores: consulta a cache global de atores."""
    # Usa a ACTOR_CACHE global definida neste ficheiro
    return find_best_match(query, ACTOR_CACHE)

def find_best_director(query: str) -> Optional[str]:
    """Wrapper específico para diretores: consulta a cache global de diretores."""
    return find_best_match(query, DIRECTOR_CACHE)


def find_best_genre(query: str) -> Optional[str]:
    """
    Wrapper bilíngue e robusto para géneros.
    - Tenta primeiro um match fuzzy em PT e traduz para EN.
    - Em seguida, normaliza para o label EXATO do KB (GENRE_CACHE) via fuzzy.
    - Se PT falhar, tenta diretamente contra o KB em EN.
    Retorna SEMPRE o label exatamente como existe no KB (tipicamente UPPERCASE),
    garantindo compatibilidade com as regras Prolog.
    """
    query_upper = query.upper()

    # Atalhos específicos para alinhar entradas curtas em EN com categorias de TV do KB.
    # Isto evita que termos genéricos como "ACTION" casem incorretamente com gêneros amplos
    # (ex.: INTERNATIONAL MOVIES) quando o fuzzy é aplicado contra GENRE_CACHE.
    EN_SHORTCUTS = {
        "ACTION": "TV ACTION & ADVENTURE",
        "DRAMA": "DRAMAS",
        "THRILLER": "THRILLERS",
        "TRILER": "THRILLERS",   # tolera erro comum de digitação
        "TRILLER": "THRILLERS",  # outra variação
    }
    if query_upper in EN_SHORTCUTS:
        try:
            print(f"[NLU] EN shortcut aplicado para '{query_upper}' -> '{EN_SHORTCUTS[query_upper]}'")
        except Exception:
            pass
        mapped = EN_SHORTCUTS[query_upper]
        # Mapear para o label EXATO do KB (em UPPERCASE)
        kb_label = find_best_match(mapped.upper(), GENRE_CACHE, threshold=60)
        if kb_label:
            return kb_label
        return mapped

    # 1) Tenta em Português e traduz para EN
    best_pt = find_best_match(query_upper, GENRE_CACHE_PT, threshold=75)
    if best_pt:
        translated_en = GENRE_TRANSLATION_MAP.get(best_pt)
        if translated_en:
            # 1.1) Mapear para label EXATO do KB (GENRE_CACHE)
            # Usa threshold mais tolerante para variações (singular/plural, sufixos)
            kb_label = find_best_match(translated_en.upper(), GENRE_CACHE, threshold=60)
            if kb_label:
                return kb_label
            # Se não encontrar no KB, devolve a tradução (melhor esforço)
            return translated_en

    # 2) Fallback: procurar diretamente no KB com a query (EN ou PT uppercase)
    best_en = find_best_match(query_upper, GENRE_CACHE, threshold=60)
    return best_en


def find_best_film(query: str) -> Optional[str]:
    """
    Wrapper específico para títulos de filmes: usa a cache global de
    títulos carregada no startup da API para fuzzy matching.
    
    IMPORTANTE: Esta função usa validações rigorosas para evitar falsos positivos.
    Use find_best_film_with_suggestions() se quiser ver alternativas quando falha.
    """
    # Usa a FILM_CACHE global definida neste ficheiro
    return find_best_match(query, FILM_CACHE)


def find_best_film_with_suggestions(query: str) -> dict:
    """
    Versão avançada de find_best_film que retorna match + sugestões.
    
    Útil para handlers que querem mostrar alternativas ao usuário quando
    não há match perfeito (ex: "Titanic" não existe, mas pode sugerir
    "Attack on Titan" com aviso que pode não ser o desejado).
    
    Returns:
        Dict com:
        - best_match: Título validado ou None
        - confidence: Score 0-100
        - suggestions: Lista de alternativas [{title, score}]
        - rejected_reason: Motivo se rejeitado
    """
    return find_best_match_with_suggestions(query, FILM_CACHE, threshold=75, max_suggestions=3)