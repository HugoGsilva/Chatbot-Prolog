% --- MÓDULO E CARREGAMENTO DE FACTOS (V2) ---

% (Mudamos o nome do módulo para evitar conflitos,
% e exportamos as novas regras)
:- module(imdb_rules, [
    filmes_por_ator/2,
    filmes_por_diretor/2,
    filmes_por_genero/2,
    genero_do_filme/2,
    contar_filmes_por_genero/2,
    contar_filmes_por_genero_e_ano/3,

    % As novas regras de recomendação (o seu pedido)
    recomendar_por_ator_e_genero/3,
    recomendar_por_dois_generos/3,

    % Nova funcionalidade: recomendação aleatória
    random_movie/1,
    random_movie_by_genre/2,
    random_movie_enriched/4,
    random_movie_by_genre_enriched/5,
    
    % Novas funcionalidades
    diretor_de_filme/2,
    filmes_por_ano/2,
    atores_do_filme/2

    % (As regras 'get_all_...' já não são necessárias aqui,
    % pois o pipeline ETL (Fase 1) trata disso)
]).

% Biblioteca necessária para random_member/2
:- use_module(library(random)).

% Carrega os novos factos (gerados pelo pipeline ETL)
:- use_module('../knowledge/imdb_kb.pl').

% --- REGRAS DE INFERÊNCIA (V2) ---
% (Reescritas para usar o schema netflix_.../...)

% filmes_por_ator(+NomeAtor, -TituloFilme)
% (Assume que NomeAtor já foi resolvido pelo NLU Nível 2)
% Filtra apenas Movies, excluindo TV Shows
filmes_por_ator(NomeAtor, TituloFilme) :-
    imdb_kb:netflix_actor(ShowID, NomeAtor),
    imdb_kb:netflix_title(ShowID, TituloFilme, _, 'Movie').

% filmes_por_genero(+NomeGenero, -TituloFilme)
% (Assume que NomeGenero já foi resolvido pelo NLU Nível 2)
% Filtra apenas Movies, excluindo TV Shows
filmes_por_genero(NomeGenero, TituloFilme) :-
    imdb_kb:netflix_genre(ShowID, NomeGenero),
    imdb_kb:netflix_title(ShowID, TituloFilme, _, 'Movie').

% genero_do_filme(+TituloFilmeRaw, -NomeGenero)
% (Case-insensitive: aceita título em qualquer caixa)
genero_do_filme(TituloFilmeRaw, NomeGenero) :-
    upcase_atom(TituloFilmeRaw, UpperParam),
    imdb_kb:netflix_title(ShowID, TituloOriginal, _, _),
    upcase_atom(TituloOriginal, UpperFact),
    UpperParam = UpperFact,
    imdb_kb:netflix_genre(ShowID, NomeGenero).

% contar_filmes_por_genero(+NomeGenero, -Contagem)
% Conta todos os filmes de um gênero específico.
% Filtra apenas Movies, excluindo TV Shows
contar_filmes_por_genero(NomeGenero, Contagem) :-
    findall(
        T,
        (
            imdb_kb:netflix_genre(ShowID, NomeGenero),
            imdb_kb:netflix_title(ShowID, T, _, 'Movie')
        ),
        Titulos
    ),
    length(Titulos, Contagem).

% contar_filmes_por_genero_e_ano(+NomeGenero, +Ano, -Contagem)
contar_filmes_por_genero_e_ano(NomeGenero, Ano, Contagem) :-
    findall(
        T,
        (
            imdb_kb:netflix_genre(ShowID, NomeGenero),
            imdb_kb:netflix_title(ShowID, T, Ano, 'Movie')
        ),
        Titulos
    ),
    length(Titulos, Contagem).

% --- NOVAS REGRAS DE RECOMENDAÇÃO (O SEU PEDIDO) ---

% recomendar_por_ator_e_genero(+NomeAtor, +NomeGenero, -TituloFilme)
recomendar_por_ator_e_genero(NomeAtor, NomeGenero, TituloFilme) :-
    filmes_por_ator(NomeAtor, TituloFilme),
    filmes_por_genero(NomeGenero, TituloFilme).

% recomendar_por_dois_generos(+Genero1, +Genero2, -TituloFilme)
recomendar_por_dois_generos(Genero1, Genero2, TituloFilme) :-
    filmes_por_genero(Genero1, TituloFilme),
    filmes_por_genero(Genero2, TituloFilme).

% --- Nova Regra ---

% filmes_por_diretor(+NomeDiretor, -TituloFilme)
% (Assume que NomeDiretor foi resolvido pelo NLU Nível 2)
% Filtra apenas Movies, excluindo TV Shows
filmes_por_diretor(NomeDiretor, TituloFilme) :-
    imdb_kb:netflix_director(ShowID, NomeDiretor),
    imdb_kb:netflix_title(ShowID, TituloFilme, _, 'Movie').

% --- Nova Regra ---

% random_movie(-TituloFilme)
% Encontra todos os títulos e escolhe um aleatoriamente.
% Filtra apenas Movies
random_movie(TituloFilme) :-
    % 1. Encontra todos os títulos de Movies (ignora Ano)
    findall(
        Titulo,
        imdb_kb:netflix_title(_, Titulo, _, 'Movie'),
        ListaDeFilmes
    ),
    % Garante que a lista não está vazia
    ListaDeFilmes \= [],
    % 2. Escolhe um membro aleatório da lista
    random_member(TituloFilme, ListaDeFilmes).

% random_movie_by_genre(+NomeGenero, -TituloFilme)
% Encontra um filme aleatório de um gênero específico.
% Filtra apenas Movies
random_movie_by_genre(NomeGenero, TituloFilme) :-
    findall(
        Titulo,
        (
            imdb_kb:netflix_genre(ShowID, NomeGenero),
            imdb_kb:netflix_title(ShowID, Titulo, _, 'Movie')
        ),
        ListaDeFilmes
    ),
    ListaDeFilmes \= [],
    random_member(TituloFilme, ListaDeFilmes).

% random_movie_enriched(-TituloFilme, -Ano, -Generos, -Diretor)
% Retorna filme aleatório com metadados completos (ano, gêneros, diretor)
random_movie_enriched(TituloFilme, Ano, Generos, Diretor) :-
    % Escolhe filme aleatório
    random_movie(TituloFilme),
    % Busca metadados do filme
    imdb_kb:netflix_title(ShowID, TituloFilme, Ano, 'Movie'),
    % Coleta todos os gêneros
    findall(G, imdb_kb:netflix_genre(ShowID, G), Generos),
    % Busca diretor (pode falhar se não houver)
    (imdb_kb:netflix_director(ShowID, Diretor) -> true ; Diretor = 'Unknown').

% random_movie_by_genre_enriched(+NomeGenero, -TituloFilme, -Ano, -Generos, -Diretor)
% Retorna filme aleatório de um gênero com metadados completos
random_movie_by_genre_enriched(NomeGenero, TituloFilme, Ano, Generos, Diretor) :-
    % Escolhe filme aleatório do gênero
    random_movie_by_genre(NomeGenero, TituloFilme),
    % Busca metadados
    imdb_kb:netflix_title(ShowID, TituloFilme, Ano, 'Movie'),
    findall(G, imdb_kb:netflix_genre(ShowID, G), Generos),
    (imdb_kb:netflix_director(ShowID, Diretor) -> true ; Diretor = 'Unknown').

% diretor_de_filme(+TituloFilme, -Diretor)
% Retorna o diretor de um filme específico.
diretor_de_filme(TituloFilmeRaw, Diretor) :-
    upcase_atom(TituloFilmeRaw, UpperParam),
    imdb_kb:netflix_title(ShowID, TituloOriginal, _, _),
    upcase_atom(TituloOriginal, UpperFact),
    UpperParam = UpperFact,
    imdb_kb:netflix_director(ShowID, Diretor).

% filmes_por_ano(+Ano, -TituloFilme)
% Retorna filmes lançados em um ano específico.
% Filtra apenas Movies
filmes_por_ano(Ano, TituloFilme) :-
    imdb_kb:netflix_title(_, TituloFilme, Ano, 'Movie').

% atores_do_filme(+TituloFilme, -Ator)
% Retorna os atores de um filme específico.
atores_do_filme(TituloFilmeRaw, Ator) :-
    upcase_atom(TituloFilmeRaw, UpperParam),
    imdb_kb:netflix_title(ShowID, TituloOriginal, _, _),
    upcase_atom(TituloOriginal, UpperFact),
    UpperParam = UpperFact,
    imdb_kb:netflix_actor(ShowID, Ator).
filmes_por_ano(Ano, TituloFilme) :-
    imdb_kb:netflix_title(_, TituloFilme, Ano).