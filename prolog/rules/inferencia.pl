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
    filmes_por_genero/2
]).

% Importa o módulo de fatos pelo caminho relativo correto
:- use_module('../knowledge/sakila.pl').

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