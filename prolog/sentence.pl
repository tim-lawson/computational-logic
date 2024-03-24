%% sentence: Sentence parser.
%
% This module parses sentences and converts them into clauses.
%
:- module(sentence, [sentence/3, sentence_body/3]).

% --- Imports ---

:- use_module(grammar).

% --- Sentence parser ---

% sentence(Sentence:list)//
%
% Parses a list of atoms into the head and body of a sentence.
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
% Parses a list of atoms into the body of a sentence.
%
% @param Sentence The list of atoms.
%
% "Determiner Noun VerbPhrase" sentences.
sentence_body(Sentence) -->
  grammar:determiner(Number, X, Y, Sentence),
  grammar:noun(Number, X),
  grammar:verb_phrase(Number, Y).

% "ProperNoun VerbPhrase" sentences.
sentence_body([(Literal :- true)]) -->
  grammar:proper_noun(Noun, X),
  grammar:verb_phrase(Noun, X => Literal).
