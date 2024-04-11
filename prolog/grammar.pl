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

predicate(bird, 1, [noun/bird]).
predicate(penguin, 1, [noun/penguin]).

predicate(human, 1, [adj/human, noun/human]).
predicate(mortal, 1, [adj/mortal, noun/mortal]).

predicate(fly, 1, [verb/fly]).

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

%% verb_phrase(?Number:atom, ?Word:atom)//
%
% The verb_phrase//2 DCG rule defines verb phrases. It relates the verb phrase's
% grammatical number, its atom, and a list of atoms that refer to it.
%
% @param Number The grammatical number.
% @param Word The adjective or verb.
%

verb_phrase(singular, false, Adjective) -->
    [is, not],
    property(singular, Adjective).

verb_phrase(singular, true, Adjective) -->
    [is],
    property(singular, Adjective).

verb_phrase(plural, false, Adjective) -->
    [are, not],
    property(plural, Adjective).

verb_phrase(plural, true, Adjective) -->
    [are],
    property(plural, Adjective).

verb_phrase(singular, false, IntransitiveVerb) -->
    [does, not],
    intransitive_verb(plural, IntransitiveVerb).

verb_phrase(plural, false, IntransitiveVerb) -->
    [do, not],
    intransitive_verb(plural, IntransitiveVerb).

verb_phrase(Number, Truth, IntransitiveVerb) -->
    intransitive_verb(Number, IntransitiveVerb).

%% property(?Number:atom, ?Word:atom)//
%
% The property//2 DCG rule defines properties. It relates the property's grammatical
% number, its atom, and a list of atoms that refer to it.
%
% @param Number The grammatical number.
% @param Word The adjective or noun.
%
property(Number, Adjective) --> adjective(Number, Adjective).
property(singular, Noun) --> [a], noun(singular, Noun).
property(plural, Noun) --> noun(plural, Noun).

%% determiner(?Number:atom, ?Body:callable, ?Head:callable, ?Rule:callable)//
%
% The determiner//4 DCG rule defines determiners. It relates the determiner's
% grammatical number, its body, its head, and a list of rules that refer to it.
%
% @param ?Number The grammatical number.
% @param ?Body The body of the rule.
% @param ?Head The head of the rule.
% @param ?Rule The rule.
%

% If the determiner is like "all", then the body of the rule implies the head.
determiner(singular, true, X => Body, X => Head, [(Head :- Body)]) --> [every].
determiner(plural, true, X => Body, X => Head, [(Head :- Body)]) --> [all].
determiner(singular, false, X => Body, X => Head, [(grammar:maybe(Head) :- \+ Body)]) --> [every].
determiner(plural, false, X => Body, X => Head, [(grammar:maybe(Head) :- \+ Body)]) --> [all].

% If the determiner is like "most", then the body of the rule implies the head *by default*.
determiner(plural, true, X => Body, X => Head, [(grammar:default(Head) :- Body)]) --> [most].
determiner(plural, true, X => Body, X => Head, [(grammar:default(Head) :- Body)]) --> [many].
determiner(plural, true, X => Body, X => Head, [(grammar:default(Head) :- Body)]) --> [a, lot, of].
determiner(plural, false, X => Body, X => Head, [(grammar:default(Head) :- \+ Body)]) --> [most].
determiner(plural, false, X => Body, X => Head, [(grammar:default(Head) :- \+ Body)]) --> [many].
determiner(plural, false, X => Body, X => Head, [(grammar:default(Head) :- \+ Body)]) --> [a, lot, of].
%% adjective(?Number:atom, ?Word:atom)//
%
% The adjective//2 DCG rule defines adjectives. It relates the adjective's grammatical
% number, its atom, and a list of atoms that refer to it.
%
% @param ?Number The grammatical number.
% @param ?Word The adjective.
%
adjective(_, X) -->
  [Adjective],
  {
      predicate_to_grammar(_Predicate, 1, adj/Adjective, X)
  }.

%% noun(?Number:atom, ?Word:atom)//
%
% The noun//2 DCG rule defines nouns. It relates the noun's grammatical number and
% its literal.
%
% @param ?Number The grammatical number.
% @param ?Word The literal.
%

% Singular nouns.
noun(singular, X) -->
  [SingularNoun],
  {
      predicate_to_grammar(_Predicate, 1, noun/SingularNoun, X)
  }.
% Plural nouns.
noun(plural, X) -->
  [PluralNoun],
  {
      predicate_to_grammar(_Predicate, 1, noun/Noun, X),
      noun_singular_to_plural(Noun, PluralNoun)
  }.

%% intransitive_verb(?Number:atom, ?Word:atom)//
%
% The intransitive_verb//2 DCG rule defines intransitive verbs. It relates the verb's
% grammatical number and its literal.
%
% @param ?Number The grammatical number.
% @param ?Word The literal.
%

% Singular verbs.
intransitive_verb(singular, X) -->
  [SingularVerb],
  {
      predicate_to_grammar(_Predicate, 1, verb/Verb, X),
      verb_plural_to_singular(Verb, SingularVerb)
  }.
% Plural verbs.
intransitive_verb(plural, X) -->
  [Verb],
  {
      predicate_to_grammar(_Predicate, 1, verb/Verb, X)
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

%% predicate_to_grammar(+Predicate:atom, +Arity:integer, +WordCategory:atom, -Literal:callable)
%
% The predicate_to_grammar/4 predicate constructs a literal from a predicate and an
% argument.
%
% @param Predicate The predicate.
% @param Arity The number of arguments the predicate takes.
% @param WordCategory The word category.
% @param Literal The literal.
%
predicate_to_grammar(Predicate, 1, WordCategory/Word, X => Literal) :-
  % If predicate is a unary predicate of arity 1 and...
  predicate(Predicate, 1, Words),
  % WordCategory/Word is a member of Words...
  member(WordCategory/Word, Words),
  % Construct Literal from Predicate and X.
  Literal=..[Predicate, X].
