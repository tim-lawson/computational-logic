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
%
% @param Sentence The list of atoms.
%

sentence_body(Sentence) -->
  grammar:determiner(Number, Truth, ToBody, ToHead, Sentence),
  grammar:noun(Number, ToBody),
  grammar:verb_phrase(Number, Truth, ToHead).

sentence_body(Sentence) -->
  grammar:determiner(Number, Truth, ToBody, X => (Head1; Head2), Sentence),
  grammar:noun(Number, ToBody),
  grammar:verb_phrase(Number, Truth, ((X => Head1); (X => Head2))).

sentence_body([implies(true, Head, always, Truth)]) -->
  grammar:proper_noun(Number, X),
  grammar:verb_phrase(Number, Truth, X => Head).

sentence_body([implies(true, (Head1; Head2), always, Truth)]) -->
  grammar:proper_noun(Number, X),
  grammar:verb_phrase(Number, Truth, ((X => Head1); (X => Head2))).
