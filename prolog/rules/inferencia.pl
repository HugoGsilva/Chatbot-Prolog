% --- MÓDULO E CARREGAMENTO DE FACTOS (V2) ---

% (Mudamos o nome do módulo para evitar conflitos,
% e exportamos as novas regras)
:- module(imdb_rules, [
    filmes_por_ator/2,
    filmes_por_genero/2,
    genero_do_filme/2,
    contar_filmes_por_genero_e_ano/3,

    % As novas regras de recomendação (o seu pedido)
    recomendar_por_ator_e_genero/3,
    recomendar_por_dois_generos/3

    % (As regras 'get_all_...' já não são necessárias aqui,
    % pois o pipeline ETL (Fase 1) trata disso)
]).

% Carrega os novos factos (gerados pelo pipeline ETL)
:- use_module('../knowledge/imdb_kb.pl').

% --- REGRAS DE INFERÊNCIA (V2) ---
% (Reescritas para usar o schema netflix_.../...)

% filmes_por_ator(+NomeAtor, -TituloFilme)
% (Assume que NomeAtor já foi resolvido pelo NLU Nível 2)
filmes_por_ator(NomeAtor, TituloFilme) :-
    imdb_kb:netflix_actor(ShowID, NomeAtor),
    imdb_kb:netflix_title(ShowID, TituloFilme, _).

% filmes_por_genero(+NomeGenero, -TituloFilme)
% (Assume que NomeGenero já foi resolvido pelo NLU Nível 2)
filmes_por_genero(NomeGenero, TituloFilme) :-
    imdb_kb:netflix_genre(ShowID, NomeGenero),
    imdb_kb:netflix_title(ShowID, TituloFilme, _).

% genero_do_filme(+TituloFilme, -NomeGenero)
% (Assume que TituloFilme já foi resolvido pelo NLU Nível 2)
genero_do_filme(TituloFilme, NomeGenero) :-
    imdb_kb:netflix_title(ShowID, TituloFilme, _),
    imdb_kb:netflix_genre(ShowID, NomeGenero).

% contar_filmes_por_genero_e_ano(+NomeGenero, +Ano, -Contagem)
contar_filmes_por_genero_e_ano(NomeGenero, Ano, Contagem) :-
    findall(
        T,
        (
            imdb_kb:netflix_genre(ShowID, NomeGenero),
            imdb_kb:netflix_title(ShowID, T, Ano)
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