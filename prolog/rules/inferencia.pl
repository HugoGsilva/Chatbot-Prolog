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
% Stubs (rascunhos) das regras exportadas
% Observação: estes stubs permitem carregar o módulo sem erros.
% A implementação real será feita na Etapa 2.6.
% ---------------------------------------------------------------------------

filmes_por_ator(_AtorId, _FilmeId) :- false.

genero_do_filme(_FilmeId, _Genero) :- false.

filmes_por_genero(_Genero, _FilmeId) :- false.