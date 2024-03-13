% Utilities.

:- module(
  utils,
  [
    concatenate_conjunctive/3,
    known_fact/2,
    try/1
  ]
).

% Warn on variables appearing only once in a clause that don't start with "_".
:-style_check(-singleton).

% An infix operator for negation.
:-op(900, fy, not).

% Record additions to known facts within a session.
:- dynamic known_fact/2.

% Base case: List is the conjunctive concatenation of true and List.
concatenate_conjunctive(true,List,List).

% Single-element case: (X,List) is the conjunctive concatenation of X and List if...
concatenate_conjunctive(X,List,(X,List)):-
  % X is not true and...
  not X=true,
  % X is not a conjunction itself (prevent infinite recursion).
  not X=(Y,Z).

% Recursive case: (X,ListZ) is the conjunctive concatenation of (X,ListX) and ListY if...
concatenate_conjunctive((X,ListX),ListY,(X,ListZ)):-
  % ListZ is the conjunctive concatenation of ListX and ListY.
  concatenate_conjunctive(ListX,ListY,ListZ).

% Succeeds if the goal is provable.
try(X):-not not X.