%% sentence: Sentence parser.
%
% This module parses sentences and converts them into clauses.
%
:- module(sentence, [sentence/3, sentence_body/3]).

% --- Imports ---

:- use_module(grammar).
:- use_module(utils).

% --- Sentence parser ---

% sentence(Sentence:list)//
%
% The sentence//1 DCG rule parses a list of atoms into a sentence.
%
% @param The list of atoms.
%
sentence(Sentence) -->
  sentence_word,
  sentence_body(Sentence).

sentence_word --> [].
sentence_word --> [that].

%% sentence_body(Sentence:list)//
%
% The sentence_body//1 DCG rule parses a list of atoms into a sentence body.
%
% @param Sentence The list of atoms.
%

sentence_body(Sentence) -->
  grammar:determiner(Number, Truth, X, Y, Sentence),
  grammar:noun(Number, X),
  grammar:verb_phrase(Number, Truth, Y).

sentence_body([implies(Literal, true)]) -->
  grammar:proper_noun(Noun, X),
  grammar:verb_phrase(Noun, true, X => Literal).

sentence_body([negate(implies(Literal, true))]) -->
  grammar:proper_noun(Noun, X),
  grammar:verb_phrase(Noun, false, X => Literal).