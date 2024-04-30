%% grammar: Vocabulary and grammar module.
%
% This module provides the vocabulary and grammar of the system.
%
:- module(
  grammar,
  [
    noun_singular_to_plural/2,
    predicate/3,
    predicate_to_grammar/4,
    verb_plural_to_singular/2
  ]
).

% An infix operator that constructs a literal from a predicate and an argument.
% `xfy` means the operator is infix and right-associative, i.e. `a => b => c` is
% equivalent to `a => (b => c)`.
:- op(600, xfy, ' => ').

% The negation of a negation is equivalent to the original proposition.
negation(negation(X)) :- X.

% --- Predicates ---

%% predicate(-Predicate:atom, -Arity:integer, -Words:list)
%
% The predicate/3 predicate defines the logical vocabulary of the system. It relates a
% predicate, its arity, and a list of words and syntactic categories that refer to the
% logical entity.
%
% @param Predicate The predicate.
% @param Arity The number of arguments the predicate takes.
% @param Words A list of words that refer to the logical entity.
%

predicate(human, 1, [adj/human, noun/human]).
predicate(mortal, 1, [adj/mortal, noun/mortal]).
predicate(genius, 1, [adj/genius, noun/genius]).

predicate(bird, 1, [noun/bird]).
predicate(penguin, 1, [noun/penguin]).
predicate(fly, 1, [verb/fly]).

predicate(happy, 1, [adj/happy]).
predicate(teacher, 1, [noun/teacher]).

predicate(pixel, 1, [noun/pixel]).
predicate(red, 1, [adj/red]).
predicate(green, 1, [adj/green]).
predicate(blue, 1, [adj/blue]).

predicate(coin, 1, [noun/coin]).
predicate(heads, 1, [adj/heads]).
predicate(tails, 1, [adj/tails]).

predicate(prize, 1, [noun/prize]).
predicate(win, 1, [verb/win]).

predicate(penguin, 1, [noun/penguin]).

% --- Vocabulary and grammar ---

%% proper_noun(?Number:atom, ?Word:atom)//
%
% The proper_noun//2 DCG rule defines proper nouns. It relates the proper noun's
% grammatical number, its atom, and a list of atoms that refer to it.
%
% @param Number The grammatical number.
% @param Word The proper noun.
%
proper_noun(singular, alice) --> [alice].
proper_noun(singular, bob) --> [bob].
proper_noun(singular, charlie) --> [charlie].

proper_noun(singular, donald) --> [donald].

proper_noun(singular, pixie) --> [pixie].

%% verb_phrase(?Number:atom, ?Word:atom)//
%
% The verb_phrase//2 DCG rule defines verb phrases. It relates the verb phrase's
% grammatical number, its atom, and a list of atoms that refer to it.
%
% @param Number The grammatical number.
% @param Word The property or verb.
%

verb_phrase(singular, ToLiteral) --> [is], property(singular, ToLiteral).
verb_phrase(singular, ToLiteral) --> [is, not], property(singular, ToLiteral/false).


verb_phrase(plural, ToLiteral) --> [are], property(plural, ToLiteral).
verb_phrase(plural, ToLiteral) --> [are, not], property(plural, ToLiteral/false).

verb_phrase(Number, ToLiteral) --> intransitive_verb(Number, ToLiteral).

verb_phrase(singular, ToLiteral) --> [does, not], intransitive_verb(plural, ToLiteral/false).
verb_phrase(plural, ToLiteral) --> [do, not], intransitive_verb(plural, ToLiteral/false).

% Disjunction
% Here X is either:
% - a proper noun for cases like "Alice is human or a bird" as we need to pass that on to get [human(alice), bird(alice)]
% - or nothing for cases like "all pixels are red or green" as we don't need to pass pixel on to get [red(X), green(X)]
disjunction(Number, X => Literal) --> [or], verb_phrase(Number, X => Literal).
disjunction(Number, X => Literal) --> [or], intransitive_verb(Number, X => Literal).
disjunction(Number, X => Literal) --> [or], property(Number, X => Literal).
disjunction(Number, X => (Literal;Rest)) --> verb_phrase(Number, X => Literal), disjunction(Number, X => Rest).
disjunction(Number, X => (Literal;Rest)) --> intransitive_verb(Number, X => Literal), disjunction(Number, X => Rest).
disjunction(Number, X => (Literal;Rest)) --> property(Number, X => Literal), disjunction(Number, X => Rest).

% Conjunction
conjunction(Number, X => Literal) --> [and], verb_phrase(Number, X => Literal).
conjunction(Number, X => Literal) --> [and], intransitive_verb(Number, X => Literal).
conjunction(Number, X => Literal) --> [and], property(Number, X => Literal).
conjunction(Number, X => (Literal,Rest)) --> verb_phrase(Number, X => Literal), conjunction(Number, X => Rest).
conjunction(Number, X => (Literal,Rest)) --> intransitive_verb(Number, X => Literal), conjunction(Number, X => Rest).
conjunction(Number, X => (Literal,Rest)) --> property(Number, X => Literal), conjunction(Number, X => Rest).

%% property(?Number:atom, ?Word:atom)//
%
% The property//2 DCG rule defines properties. It relates the property's grammatical
% number, its atom, and a list of atoms that refer to it.
%
% @param Number The grammatical number.
% @param Word The adjective or noun.
%
property(_, Adjective) --> adjective(Adjective).

property(singular, Noun) --> [a], noun(singular, Noun).
property(singular, negation(Noun)) --> [not, a], noun(singular, Noun).

property(plural, Noun) --> noun(plural, Noun).
property(plural, negation(Noun)) --> [not], noun(plural, Noun).

%% determiner(?Number:atom, ?Body:callable, ?Head:callable, ?Rule:callable)//
%
% The determiner//4 DCG rule defines determiners. It relates the determiner's
% grammatical number, its corresponding rule, and a list of atoms that refer to it.
%
% @param ?Number The grammatical number.
% @param ?Body The relation between X and the body of the rule.
% @param ?Head The relation between X and the head of the rule.
% @param ?Rule The rule.
%

% NOTE: The first option is the one picked when generating answers
% If the determiner is like "all", then the body of the rule implies the head.
determiner(singular, 1, proper) --> [].
determiner(plural, 1, proper) --> [].
determiner(singular, 1, normal) --> [every].
determiner(plural, 1, normal) --> [all].

% If the determiner is like "most", then the body of the rule implies the head *by default*.
determiner(plural, 0.75, normal) --> [most].
determiner(plural, 0.75, normal) --> [many].
determiner(plural, 0.75, normal) --> [a, lot, of].
determiner(singular, 0.75, proper) --> [it, is, likely, that].

determiner(plural, 0.5, normal) --> [some].
determiner(singular, 0.5, proper) --> [it, could, be, that].

%% adjective(?ToLiteral:atom)//
%
% The adjective//1 DCG rule defines adjectives.
% It relates the adjective and its literal.
%
% @param ?ToLiteral The adjective and its literal in the form Noun => Adjective(Noun).
%
adjective(ToLiteral) -->
  [Adjective],
  {
      predicate_to_grammar(_Predicate, 1, adj/Adjective, ToLiteral)
  }.

%% noun(?Number:atom, ?ToLiteral:atom)//
%
% The noun//2 DCG rule defines common nouns.
% It relates the noun, its grammatical number, and its literal.
%
% @param ?Number The grammatical number.
% @param ?ToLiteral The noun and its literal.
%

% Singular nouns.
noun(singular, ToLiteral) -->
  [SingularNoun],
  {
      predicate_to_grammar(_Predicate, 1, noun/SingularNoun, ToLiteral)
  }.
% Plural nouns.
noun(plural, ToLiteral) -->
  [PluralNoun],
  {
      predicate_to_grammar(_Predicate, 1, noun/Noun, ToLiteral),
      noun_singular_to_plural(Noun, PluralNoun)
  }.

%% intransitive_verb(?Number:atom, ?ToLiteral:atom)//
%
% The intransitive_verb//2 DCG rule defines intransitive verbs.
% It relates the verb, its grammatical number, and its literal.
%
% @param ?Number The grammatical number.
% @param ?ToLiteral The intransitive verb and its literal.
%

% Singular verbs.
intransitive_verb(singular, ToLiteral) -->
  [SingularVerb],
  {
      predicate_to_grammar(_Predicate, 1, verb/Verb, ToLiteral),
      verb_plural_to_singular(Verb, SingularVerb)
  }.

% Plural verbs.
intransitive_verb(plural, ToLiteral) -->
  [Verb],
  {
      predicate_to_grammar(_Predicate, 1, verb/Verb, ToLiteral)
  }.

% --- Grammatical number ---

%% noun_singular_to_plural(?SingularNoun:atom, ?PluralNoun:atom)
%
% The noun_singular_to_plural/2 predicate converts the singular form of a noun to the
% plural form and vice versa.
%
% @param SingularNoun The singular form.
% @param PluralNoun The plural form.
%
noun_singular_to_plural(SingularNoun, PluralNoun) :-
  (
    % Irregular forms
    SingularNoun = woman -> PluralNoun = women;
    SingularNoun = man -> PluralNoun = men;
    SingularNoun = genius -> PluralNoun = geniuses;
    % Regular forms
    atom_concat(SingularNoun, s, PluralNoun)
  ).

%% verb_plural_to_singular(?PluralVerb:atom, ?SingularVerb:atom)
%
% The verb_plural_to_singular/2 predicate converts the plural form of a verb to the
% singular form and vice versa.
%
% @param PluralVerb The plural form.
% @param SingularVerb The singular form.
%
verb_plural_to_singular(PluralVerb, SingularVerb) :-
  (
    % Irregular forms
    PluralVerb = fly -> SingularVerb = flies;
    % Regular forms
    atom_concat(PluralVerb, s, SingularVerb)
  ).

% --- Logic ---

%% predicate_to_grammar(+Predicate:atom, +Arity:integer, +WordCategory:atom, -ToLiteral:callable)
%
% The predicate_to_grammar/4 predicate constructs a literal from a predicate and an
% argument.
%
% @param Predicate The predicate.
% @param Arity The number of arguments the predicate takes.
% @param WordCategory The word category.
% @param ToLiteral The word and its literal.
%
predicate_to_grammar(Predicate, 1, WordCategory/Word, X => Literal) :-
  % If predicate is a unary predicate of arity 1 and...
  predicate(Predicate, 1, Words),
  % WordCategory/Word is a member of Words...
  member(WordCategory/Word, Words),
  % Construct Literal from Predicate and X.
  Literal=..[Predicate, X].

predicate_to_grammar(Predicate, 1, WordCategory/Word, (X => negation(Literal))/false) :-
  % If predicate is a unary predicate of arity 1 and...
  predicate(Predicate, 1, Words),
  % WordCategory/Word is a member of Words...
  member(WordCategory/Word, Words),
  % Construct Literal from Predicate and X.
  Literal=..[Predicate, X].