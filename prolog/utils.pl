%% utils: Utilities module.
%
% This module provides useful predicates.
%
:- module(utils, [concatenate_conjunctive/3, known_fact/2, try/1]).

% This predicate warns on variables that appear once in a clause that don't start with
% an underscore.
:-style_check(-singleton).

% A prefix operator for negation, i.e. `not p`.
:-op(900, fy, not).

% Store known facts within a session.
:- dynamic known_fact/2.

%% concatenate_conjunctive(+ListX, +ListY, -ListZ)
%
% The concatenate_conjunctive/3 predicate concatenates two lists of conjunctive literals.
%
% @param ListX The first list.
% @param ListY The second list.
% @param ListZ The concatenated list.
%
% Base case: List is the conjunctive concatenation of true and List.
concatenate_conjunctive(true, List, List).
% Single-element case: (X, List) is the conjunctive concatenation of X and List if...
concatenate_conjunctive(X, List, (X, List)):-
  % X is not true and...
  not X = true,
  % X is not a conjunction itself (prevent infinite recursion).
  not X = (Y, Z).
% Recursive case: (X, ListZ) is the conjunctive concatenation of (X, ListX) and ListY if...
concatenate_conjunctive((X, ListX), ListY, (X, ListZ)):-
  % ListZ is the conjunctive concatenation of ListX and ListY.
  concatenate_conjunctive(ListX, ListY, ListZ).

%% try(+X)
%
% The try/1 predicate tries to prove a goal.
%
% @param X The goal to prove.
%
try(X):-not not X.