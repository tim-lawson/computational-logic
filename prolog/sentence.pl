%% sentence: Sentence parser.
%
% This module parses sentences and converts them into clauses.
%
:- module(sentence, [sentence/3, sentence_body/3]).

% --- Imports ---

:- use_module(grammar).
:- use_module(utils).

% --- Sentence parser ---

%% sentence(?Sentence:list)//
%
% The sentence//1 DCG rule parses a list of atoms into a sentence.
%
% @param Sentence The list of atoms.
%
sentence(Sentence) -->
  sentence_word,
  sentence_body(Sentence).

sentence_word --> [].
sentence_word --> [that].

%% sentence_body(?Sentence:list)//
%
% The sentence_body//1 DCG rule parses a list of atoms into a sentence body.
% The operator "=>" is used to unify two atoms in `grammar.pl`, usually ToLiteral.
% This is handy because we need to pass them both through several layers of the grammar module,
% e.g., verb_phrase -> property -> adjective -> predicate_to_grammar.
% The operator itself does not do anything special -- it could be any arithmetic operator,
% provided that it is destructured accordingly in `grammar:predicate_to_grammar`.
%
% @param Sentence The list of atoms.
%

% Here X is the variable that appears in both the head and body, e.g., `mortal(X) :- human(X).`
% It's created here and passed to both, so each side refers to the *same* variable.
%
%  TODO: Can collapse with structure like:
%   sentence_body([(Head / Applicability :- Body)]) -->
%   grammar:determiner(Number, Applicability),
%   grammar:noun(Number, _ => Body),
%   grammar:verb_phrase(Number, _ => Head).

sentence_body([(Head :- Body | Certainty)]) -->
  grammar:determiner(Number, Certainty, normal),
  grammar:noun(Number, X => Body),
  grammar:verb_phrase(Number, X => Head).

% Disjunction and conjunction are handled separately.
sentence_body([(Head :- Body | Certainty)]) -->
  grammar:determiner(Number, Certainty, normal),
  grammar:noun(Number, X => Body),
  grammar:disjunction(Number, X => Head).

sentence_body([(Head :- Body | Certainty)]) -->
  grammar:determiner(Number, Certainty, normal),
  grammar:noun(Number, X => Body),
  grammar:conjunction(Number, X => Head).

% ProperNoun is bound to a value by `proper_noun`.
% It is passed to `verb_phrase` so it can be used as the atom of Head, e.g., `human(alice) :- true.`
sentence_body([(Head :- true | Certainty)]) -->
  grammar:determiner(Number, Certainty, proper),
  grammar:proper_noun(Number, ProperNoun),
  grammar:verb_phrase(Number, ProperNoun => Head).

sentence_body([(Head :- true | Certainty)]) -->
  grammar:determiner(Number, Certainty, proper),
  grammar:proper_noun(Number, ProperNoun),
  grammar:disjunction(Number, ProperNoun => Head).

sentence_body([(Head :- true | Certainty)]) -->
  grammar:determiner(Number, Certainty, proper),
  grammar:proper_noun(Number, ProperNoun),
  grammar:conjunction(Number, ProperNoun => Head).

