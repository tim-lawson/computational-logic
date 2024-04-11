%% utils: Utilities module.
%
% This module provides useful operators and predicates.
%
:- module(utils, [concatenate_conjunctive/3, known_fact/1, try/1]).

% This predicate warns on variables that appear once in a clause that don't start with
% an underscore.
:-style_check(-singleton).

% --- Configuration ---

% This predicate controls whether debug outputs are enabled.
debug_enabled(on).

% --- Operators ---

% A prefix operator for negation, i.e. `not p`.
% :-op(900, fy, is_not).

% --- Predicates ---

% Store known facts.
:- dynamic known_fact/1.

%% write_debug(+X)
%
% The write_debug/1 predicate writes a debug message to the console.
%
write_debug(X) :-
    % If debug outputs are enabled...
    debug_enabled(on),
    % Set the output colour to green.
    write(user_output, '\033[92m'),
    writeln(user_output, X),
    % Reset the output colour.
    write(user_output, '\033[0m'),
    flush_output(user_output).

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
  \+ X = true,
  % X is not a conjunction itself (prevent infinite recursion).
  \+ X = (Y, Z).
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
try(X):- \+ \+ X.