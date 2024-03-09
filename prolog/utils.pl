% Utilities.

:- module(
  utils,
  [
    concatenate_conjunctive/3,
    concatenate_disjunctive/3,
    concatenate/3,
    copy_element/2,
    is_element/2,
    is_in_disjunction/2,
    is_proper_subset/2,
    is_subset/2,
    known_fact/2,
    remove_first_conjunctive/3,
    remove_first/3,
    same_predicate/2,
    setof0/3,
    try/1,
    variable_is_element/2,
    variable_is_proper_subset/2,
    variable_remove_one/3
  ]
).

% Warn on variables appearing only once in a clause (that don't start with "_").
:-style_check(-singleton).

% infix operators
:-op(900, fy, not).

% --- Facts ---

% Record additions to known facts within a session
:- dynamic known_fact/2.

% --- Lists ---

% base: X is an element of [X|List].
is_element(X,[X|List]).
% recursive: X is an element of [Y|List] if X is an element of List
is_element(X,[Y|List]):-is_element(X,List).

% base: any list is the concatenation of [] and itself.
concatenate([],List,List).
% recursive: [X|ListZ] is the concatenation of [X|ListX] and ListY if...
concatenate([X|ListX],ListY,[X|ListZ]):-
  % ListZ is the concatenation of ListX and ListY.
  concatenate(ListX,ListY,ListZ).

% base: ListY is [X|ListY] without the first occurrence of X.
remove_first(X,[X|ListY],ListY).
% recursive: [Y|ListZ] is [Y|ListY] without the first occurrence of X if...
remove_first(X,[Y|ListY],[Y|ListZ]):-
  % ListZ is ListY without the first occurrence of X.
  remove_first(X,ListY,ListZ).

% --- Sets ---

% base: [] is a subset of any List.
is_subset([],List).
% recursive: [X|ListX] is a subset of ListY if...
is_subset([X|ListX],ListY):-
  % X is an element of ListY and...
  is_element(X,ListY),
  % ListX is a subset of ListY.
  is_subset(ListX,ListY).

% base: [] is a proper subset of List if...
is_proper_subset([],List):-
  % List is not [].
  List\=[].
% recursive: [X|ListX] is a proper subset of ListY if...
is_proper_subset([X|ListX],ListY):-
  % ListZ is ListY without the first occurrence of X and...
  remove_first(X,ListY,ListZ),
  % ListX is a proper subset of ListZ.
  is_proper_subset(ListX,ListZ).

% equivalents for variables

variable_is_element(X,[Y|ListY]):-
  X==Y.
variable_is_element(X,[Y|ListY]):-
  variable_is_element(X,ListY).

variable_remove_one(X,[Y|ListY],ListY):-
  X==Y.
variable_remove_one(X,[Y|ListY],[Y|ListZ]):-
  variable_remove_one(X,ListY,ListZ).

variable_is_proper_subset([],ListY):-
  ListY\=[].
variable_is_proper_subset([X|ListX],ListY):-
  variable_remove_one(X,ListY,ListZ),
  variable_is_proper_subset(ListX,ListZ).

% conjunctions and disjunctions

% base: X is in a disjunction if...
is_in_disjunction(X,X):-
  % X is not false and...
  not X=false,
  % X is not a disjunction itself (prevent infinite recursion).
  not X=(Y;Z).
% single: X is in a disjunction if X is the first disjunct.
is_in_disjunction(X,(X;List)).
% recursive: X is in a disjunction if List is the second disjunction and...
is_in_disjunction(X,(Y;List)):-
  % X is in List.
  is_in_disjunction(X,List).

% base: List is the conjunctive concatenation of true and List.
concatenate_conjunctive(true,List,List).
% single: (X,List) is the conjunctive concatenation of X and List if...
concatenate_conjunctive(X,List,(X,List)):-
  % X is not true and...
  not X=true,
  % X is not a conjunction itself (prevent infinite recursion).
  not X=(Y,Z).
% recursive: (X,ListZ) is the conjunctive concatenation of (X,ListX) and Ys if...
concatenate_conjunctive((X,ListX),ListY,(X,ListZ)):-
  % ListZ is the conjunctive concatenation of ListX and ListY.
  concatenate_conjunctive(ListX,ListY,ListZ).

% base: List is the disjunctive concatenation of false and List.
concatenate_disjunctive(false,List,List).
% single: (X;List) is the disjunctive concatenation of X and List if...
concatenate_disjunctive(X,List,(X;List)):-
  % X is not false and...
  not X=false,
  % X is not a disjunction itself (prevent infinite recursion).
  not X=(Y;Z).
% recursive: (X;ListZ) is the disjunctive concatenation of (X;ListX) and ListY if...
concatenate_disjunctive((X;ListX),ListY,(X;ListZ)):-
  % ListZ is the disjunctive concatenation of ListX and ListY.
  concatenate_disjunctive(ListX,ListY,ListZ).

% base: true is X without the first occurrence of X if...
remove_first_conjunctive(X,X,true):-
  % X is not true and...
  not X=true,
  % X is not a conjunction itself (prevent infinite recursion).
  not X=(Y,Z).
% single: ListY is (X,ListY) without the first occurrence of X.
remove_first_conjunctive(X,(X,ListY),ListY).
% recursive: ListZ is (X,ListY) without the first occurrence of X if...
remove_first_conjunctive(X,(Y,ListY),(Y,ListZ)):-
  % ListZ is ListY without the first occurrence of X.
  remove_first_conjunctive(X,ListY,ListZ).

% copy an element from List to variable X without modifying X:
copy_element(X,List):-
  % find an element Y in List and...
  is_element(Y,List),
  % create a copy of Y and store it in X.
  copy_term(Y,X).

% equivalent to call(X)?
try(X):-not not X.

% other useful predicates

% setof/3 but succeeds with the empty list if no solutions are found
setof0(X,G,L):-
  setof(X,G,L),
  !.
setof0(X,G,[]).

% literals have the same predicate and arity
same_predicate(L1,L2):-
  functor(L1,P,N),
  functor(L2,P,N).
