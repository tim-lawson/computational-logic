% Vocabulary and grammar.

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
:- op(600, xfy, ' => ').

% --- Predicates ---

% Nouns
predicate(bird, 1, [noun/bird]).

% Words that are both adjectives and nouns
predicate(human, 1, [adj/human, noun/human]).
predicate(mortal, 1, [adj/mortal, noun/mortal]).

% Intransitive verbs
predicate(fly, 1, [verb/fly]).

% --- Vocabulary and grammar ---

% Proper nouns
proper_noun(singular, alice) --> [alice].
proper_noun(singular, bob) --> [bob].
proper_noun(singular, charlie) --> [charlie].

% Verb phrases
verb_phrase(singular, Word) --> [is], property(singular, Word).
verb_phrase(plural, Word) --> [are], property(plural, Word).
verb_phrase(Noun, Word) --> intransitive_verb(Noun, Word).

% Properties
property(Noun, Word) --> adjective(Noun, Word).
property(singular, Word) --> [a], noun(singular, Word).
property(plural, Word) --> noun(plural, Word).

% Determiners
determiner(singular, X => Body, X => Head, [(Head :- Body)]) --> [every].
determiner(plural, X => Body, X => Head, [(Head :- Body)]) --> [all].
determiner(plural, X => Body, X => Head, default(Head :- Body)) --> [most].

% Exceptions
exception(Noun, Word) --> [except], noun(Noun, Word).

% --- Grammatical number ---

% Convert the singular form of a noun to the plural form and vice versa.
noun_singular_to_plural(SingularNoun, PluralNoun):-
  (
    % Irregular forms
    SingularNoun = woman -> PluralNoun = women;
    SingularNoun = man -> PluralNoun = men;
    % Regular forms
    atom_concat(SingularNoun, s, PluralNoun)
  ).

% Convert the plural form of a verb to the singular form and vice versa.
verb_plural_to_singular(PluralVerb, SingularVerb):-
  (
    % Irregular forms
    PluralVerb = fly -> SingularVerb = flies;
    % Regular forms
    atom_concat(PluralVerb, s, SingularVerb)
  ).

% --- Logic ---

% Convert a predicate to a grammatical rule.
predicate_to_grammar(Predicate, 1, WordCategory/Word, X => Literal):-
  % If predicate is a unary predicate of arity 1 and...
  predicate(Predicate, 1, Words),
  % WordCategory/Word is a member of Words...
  member(WordCategory/Word, Words),
  % Construct Literal from Predicate and X.
  Literal=..[Predicate, X].

adjective(_, X) -->
  [Adjective],
  { predicate_to_grammar(_Predicate, 1, adj/Adjective, X) }.

noun(singular, X) -->
  [SingularNoun],
  { predicate_to_grammar(_Predicate, 1, noun/SingularNoun, X) }.

noun(plural, X) -->
  [PluralNoun],
  {
      predicate_to_grammar(_Predicate, 1, noun/Noun, X),
      noun_singular_to_plural(Noun, PluralNoun)
  }.

intransitive_verb(singular, X) -->
  [SingularVerb],
  {
      predicate_to_grammar(_Predicate, 1, verb/Verb, X),
      verb_plural_to_singular(Verb, SingularVerb)
  }.

intransitive_verb(plural, X) -->
  [Verb],
  { predicate_to_grammar(_Predicate, 1, verb/Verb, X) }.
