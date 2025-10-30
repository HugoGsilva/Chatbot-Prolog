/**
 * Módulo de Fatos Sakila (Mock para Testes)
 *
 * Este módulo substitui o módulo real sakila_facts durante a execução
 * dos testes unitários, fornecendo um conjunto de dados hipotético
 * controlado para validação das regras.
 */
:- module(sakila_facts, [
    actor/2,
    film/5,
    acted_in/2,
    category/2,
    film_category/2
]).

% -----------------------------
% Fatos de Atores
% -----------------------------
actor(1, 'TOM HANKS').
actor(2, 'MERYL STREEP').

% -----------------------------
% Fatos de Filmes
% -----------------------------
film(101, 'THE MATRIX', _, 1999, _).
film(102, 'THE POST', _, 2017, _).
film(103, 'JULIE & JULIA', _, 2009, _).

% -----------------------------
% Fatos de Categorias
% -----------------------------
category(11, 'Sci-Fi').
category(12, 'Drama').
category(13, 'Comedy').

% -----------------------------------------------
% Fatos de Relacionamento (Atores <-> Filmes)
% -----------------------------------------------
acted_in(1, 102).  % Tom Hanks em 'THE POST'
acted_in(2, 102).  % Meryl Streep em 'THE POST'
acted_in(2, 103).  % Meryl Streep em 'JULIE & JULIA'

% -----------------------------------------------
% Fatos de Relacionamento (Filmes <-> Categorias)
% -----------------------------------------------
film_category(101, 11). % 'THE MATRIX' é Sci-Fi
film_category(102, 12). % 'THE POST' é Drama
film_category(103, 13). % 'JULIE & JULIA' é Comedy
film_category(103, 12). % 'JULIE & JULIA' também é Drama