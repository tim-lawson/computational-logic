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

%% find_clause(+Clause:atom, +Fact:atom, +FactList:list)
%
% The find_clause/3 predicate finds a clause in the list of facts that unifies with the
% given clause and stores the fact in the output.
%
% @param +Clause: The clause to find.
% @param +Fact: The fact to store in the output.
% @param +FactList: The list of facts to search.
%

% Base case: If the clause is found, store the fact in the output.
find_clause(Clause, Fact, [Fact|_FactList]) :-
  % Avoid instantiating Fact!
  copy_term(Fact, [Clause]).

% Recursive case: If the clause is not found, search the rest of the list.
find_clause(Clause, Fact, [_Fact|FactList]) :-
  find_clause(Clause, Fact, FactList).

%% transform(+Term:atom, +Truth:atom, -ClauseList:list)
%
% The transform/2 predicate transforms a term into a list of clauses.
%
% @param +Term: The term to transform.
% @param +Truth: The truth-value of the term.
% @param -ClauseList: The list of clauses generated based on the term.
%

% Base case: If the term is a conjunction, transform each conjunct.
transform((Term1, Term2), Truth, [(Term1 :- Truth)|Rest]) :-
  !,
  transform(Term2, Truth, Rest).

% Recursive case: If the term is not a conjunction, transform it into a clause.
transform(Term, true, [implies(Term, true)]).
transform(Term, false, [negate(implies(Term, true))]).

%% try(+X)
%
% The try/1 predicate tries to prove a goal.
%
% @param X The goal to prove.
%
try(X):- \+ \+ X.

%% write_debugs(+List)
%
% The write_debugs/1 predicate writes a list of debug messages to the console.
%
% @param List: A list of debug messages.
%
write_debugs([]).
write_debugs([X|Rest]) :- write_debug(X), write_debugs(Rest).

%% write_debug(+X)
%
% The write_debug/1 predicate writes a debug message to the console.
%
% @param X: The debug message.
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
