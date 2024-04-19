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

predicate(bird, 1, [noun/bird]).
predicate(fly, 1, [verb/fly]).
predicate(happy, 1, [adj/happy]).
predicate(human, 1, [adj/human, noun/human]).
predicate(mortal, 1, [adj/mortal, noun/mortal]).
predicate(penguin, 1, [noun/penguin]).
predicate(teacher, 1, [noun/teacher]).

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

%% verb_phrase(?Number:atom, ?Word:atom)//
%
% The verb_phrase//2 DCG rule defines verb phrases. It relates the verb phrase's
% grammatical number, its atom, and a list of atoms that refer to it.
%
% @param Number The grammatical number.
% @param Word The property or verb.
%

verb_phrase(singular, Property) --> [is], property(singular, Property).
verb_phrase(singular, negation(Property)) --> [is, not], property(singular, Property).

verb_phrase(plural, Property) --> [are], property(plural, Property).
verb_phrase(plural, negation(Property)) --> [are, not], property(plural, Property).

verb_phrase(Number, IntransitiveVerb) --> intransitive_verb(Number, IntransitiveVerb).

verb_phrase(singular, negation(IntransitiveVerb)) --> [does, not], intransitive_verb(plural, IntransitiveVerb).
verb_phrase(plural, IntransitiveVerb) --> [do, not], intransitive_verb(plural, negation(IntransitiveVerb)).

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

% If the determiner is like "all", then the body of the rule implies the head.
determiner(singular, X => Body, X => Head, [(Head :- Body)]) --> [every].
determiner(plural, X => Body, X => Head, [(Head :- Body)]) --> [all].

% If the determiner is like "no", then the body of the rule implies the negation of the head.
determiner(singular, X => Body, X => negation(Head), [(negation(Head :- Body))]) --> [every].
determiner(plural, X => Body, X => negation(Head), [(negation(Head :- Body))]) --> [all].

% If the determiner is like "most", then the body of the rule implies the head *by default*.
determiner(plural, X => Body, X => Head, [(default(Head) :- Body)]) --> [most].
determiner(plural, X => Body, X => Head, [(default(Head) :- Body)]) --> [many].
determiner(plural, X => Body, X => Head, [(default(Head) :- Body)]) --> [a, lot, of].

%% adjective(?Number:atom, ?ToLiteral:atom)//
%
% The adjective//2 DCG rule defines adjectives.
% It relates the adjective, its grammatical number, and its literal.
%
% @param ?Number The grammatical number.
% @param ?ToLiteral The adjective and its literal.
%
adjective(_, ToLiteral) -->
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
