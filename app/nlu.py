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

GENRE_TRANSLATION_MAP = {
    # Action & Adventure
    "AÇÃO E AVENTURA (FILMES)": "ACTION & ADVENTURE", # (Compound, para o protótipo)
    "FILME DE AÇÃO": "ACTION",
    "FILME DE AVENTURA": "ADVENTURE",
    "AÇÃO (FILME)": "ACTION",
    "AVENTURA (FILME)": "ADVENTURE",
    
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
    """Retorna o melhor match para `query` dentro de `cache` se a pontuação
    atingir o `threshold`; caso contrário, retorna None.

    - Normaliza apenas para comparação (uppercase) antes do matching.
    - Usa `process.extractOne` que devolve `(string_encontrada, pontuação)`.
    """
    if not query or not cache:
        return None

    result = process.extractOne(query.upper(), cache)
    if not result:
        return None

    value, score = result[0], result[1]
    return value if score >= threshold else None


def find_best_actor(query: str) -> Optional[str]:
    """Wrapper específico para atores: consulta a cache global de atores."""
    # Usa a ACTOR_CACHE global definida neste ficheiro
    return find_best_match(query, ACTOR_CACHE)


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
    """Wrapper específico para títulos de filmes: usa a cache global de
    títulos carregada no startup da API para fuzzy matching.
    """
    # Usa a FILM_CACHE global definida neste ficheiro
    return find_best_match(query, FILM_CACHE)