/**
 * Testes unitários para o módulo de regras sakila_rules
 * Framework: plunit (SWI-Prolog)
 *
 * Observação: Estes testes assumem que o módulo de fatos sakila_facts
 * contém exclusivamente o conjunto de dados hipotético descrito.
 */

:- begin_tests(sakila_rules).

% Injeta o módulo de fatos mock antes de carregar as regras.
% Isso garante que o módulo sakila_facts já esteja carregado
% e evita que o arquivo real ../knowledge/sakila.pl seja carregado.
:- use_module('./mock_sakila_facts.pl').

% Importa o módulo de regras (que por sua vez importa os fatos)
:- use_module('../../prolog/rules/inferencia.pl').

% ---------------------------------------------------------------------------
% filmes_por_ator/2
% ---------------------------------------------------------------------------

test(filmes_por_ator_tom_hanks_post) :-
    sakila_rules:filmes_por_ator('TOM HANKS', 'THE POST').

test(filmes_por_ator_meryl_streep_dois_filmes_ordenados) :-
    setof(T,
          sakila_rules:filmes_por_ator('MERYL STREEP', T),
          Ts),
    assertion(Ts == ['JULIE & JULIA', 'THE POST']).

test(filmes_por_ator_tom_hanks_matrix, [fail]) :-
    sakila_rules:filmes_por_ator('TOM HANKS', 'THE MATRIX').

% ---------------------------------------------------------------------------
% filmes_por_genero/2
% ---------------------------------------------------------------------------

test(filmes_por_genero_drama_dois_filmes) :-
    setof(T,
          sakila_rules:filmes_por_genero('Drama', T),
          Ts),
    assertion(Ts == ['JULIE & JULIA', 'THE POST']).

test(filmes_por_genero_scifi_um_filme) :-
    setof(T,
          sakila_rules:filmes_por_genero('Sci-Fi', T),
          Ts),
    assertion(Ts == ['THE MATRIX']).

% ---------------------------------------------------------------------------
% recomendar_por_ator/2
% ---------------------------------------------------------------------------

test(recomendar_por_ator_tom_hanks_julie) :-
    sakila_rules:recomendar_por_ator('TOM HANKS', 'JULIE & JULIA').

test(recomendar_por_ator_tom_hanks_post, [fail]) :-
    sakila_rules:recomendar_por_ator('TOM HANKS', 'THE POST').

% ---------------------------------------------------------------------------
% contar_filmes_por_genero_e_ano/3
% ---------------------------------------------------------------------------

test(contar_filmes_drama_2017_um) :-
    sakila_rules:contar_filmes_por_genero_e_ano('Drama', 2017, Contagem),
    assertion(Contagem == 1).

test(contar_filmes_comedy_2009_um) :-
    sakila_rules:contar_filmes_por_genero_e_ano('Comedy', 2009, Contagem),
    assertion(Contagem == 1).

test(contar_filmes_scifi_2000_zero) :-
    sakila_rules:contar_filmes_por_genero_e_ano('Sci-Fi', 2000, Contagem),
    assertion(Contagem == 0).

:- end_tests(sakila_rules).