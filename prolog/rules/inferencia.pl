/**
 * Módulo de Regras Sakila
 *
 * Este módulo define regras de inferência que operam sobre os
 * fatos expostos pelo módulo sakila_facts.
 *
 * Regras exportadas (consultadas pela API Python):
 * - filmes_por_ator/2
 * - genero_do_filme/2
 * - filmes_por_genero/2
 */
:- module(sakila_rules, [
    filmes_por_ator/2,
    genero_do_filme/2,
    filmes_por_genero/2,
    recomendar_por_ator/2,
    contar_filmes_por_genero_e_ano/3,
    get_all_actors/1
]).

% Importa o módulo de fatos pelo caminho relativo correto
:- if(\+ current_module(sakila_facts)).
:- use_module('../knowledge/sakila.pl').
:- endif.

% ---------------------------------------------------------------------------
% Implementações das regras de consulta básicas
% Observação: estas regras operam sobre os fatos expostos pelo módulo sakila_facts.
% ---------------------------------------------------------------------------

% filmes_por_ator(NomeAtor, TituloFilme)
% Sucede se o ator com nome NomeAtor atuou no filme com título TituloFilme.
filmes_por_ator(NomeAtor, TituloFilme) :-
    sakila_facts:actor(ActorID, NomeAtor),
    sakila_facts:acted_in(ActorID, FilmID),
    sakila_facts:film(FilmID, TituloFilme, _, _, _).

% genero_do_filme(TituloFilme, NomeGenero)
% Sucede se o filme com título TituloFilme pertence ao gênero NomeGenero.
genero_do_filme(TituloFilme, NomeGenero) :-
    sakila_facts:film(FilmID, TituloFilme, _, _, _),
    sakila_facts:film_category(FilmID, CategoryID),
    sakila_facts:category(CategoryID, NomeGenero).

% filmes_por_genero(NomeGenero, TituloFilme)
% Sucede se o gênero NomeGenero contém o filme com título TituloFilme.
filmes_por_genero(NomeGenero, TituloFilme) :-
    sakila_facts:category(CategoryID, NomeGenero),
    sakila_facts:film_category(FilmID, CategoryID),
    sakila_facts:film(FilmID, TituloFilme, _, _, _).

% ---------------------------------------------------------------------------
% Regra de recomendação baseada em gênero/ator
% recomendar_por_ator(NomeAtor, FilmeRecomendado)
% Sucede se FilmeRecomendado for diferente de um filme (FilmeOriginal)
% que o ator NomeAtor estrelou, mas compartilha um gênero com FilmeOriginal,
% e o ator NomeAtor não atuou em FilmeRecomendado.
% ---------------------------------------------------------------------------

recomendar_por_ator(NomeAtor, FilmeRecomendado) :-
    % Encontra um filme original em que o ator atuou
    filmes_por_ator(NomeAtor, FilmeOriginal),
    % Determina o gênero do filme original
    genero_do_filme(FilmeOriginal, Genero),
    % Busca filmes do mesmo gênero
    filmes_por_genero(Genero, FilmeRecomendado),
    % Filtro: filme recomendado deve ser diferente do original
    FilmeRecomendado \= FilmeOriginal,
    % Filtro: ator não atuou no filme recomendado
    \+ filmes_por_ator(NomeAtor, FilmeRecomendado).

% ---------------------------------------------------------------------------
% Regra de agregação: contagem de filmes por gênero e ano
% contar_filmes_por_genero_e_ano(NomeGenero, Ano, Contagem)
% Sucede unificando Contagem com o número total de filmes que pertencem
% ao gênero NomeGenero e foram lançados no ano Ano.
% ---------------------------------------------------------------------------

% Regra auxiliar (não exportada): determina se um filme pertence ao gênero e ano.
filme_do_genero_e_ano(NomeGenero, Ano, TituloFilme) :-
    sakila_facts:category(CategoryID, NomeGenero),
    sakila_facts:film_category(FilmID, CategoryID),
    sakila_facts:film(FilmID, TituloFilme, _, Ano, _).

% Regra principal de contagem usando agregação com findall/3 + length/2.
contar_filmes_por_genero_e_ano(NomeGenero, Ano, Contagem) :-
    findall(TituloFilme,
            filme_do_genero_e_ano(NomeGenero, Ano, TituloFilme),
            ListaDeFilmes),
    length(ListaDeFilmes, Contagem).

% ---------------------------------------------------------------------------
% Regra utilitária: lista de todos os atores (ordenada e sem duplicados)
% get_all_actors(-ListaNomes)
% Retorna uma lista ordenada e única de todos os nomes de atores.
% ---------------------------------------------------------------------------
get_all_actors(ListaNomes) :-
    setof(Nome, ID^sakila_facts:actor(ID, Nome), ListaNomes).