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
  grammar:determiner(Number, XBody, XHead, Sentence),
  grammar:noun(Number, XBody),
  grammar:verb_phrase(Number, XHead).

sentence_body(Sentence) -->
  grammar:determiner(Number, XBody, X => negation(Head), Sentence),
  grammar:noun(Number, XBody),
  grammar:verb_phrase(Number, negation(X => Head)).

sentence_body(Sentence) -->
  grammar:determiner(Number, XBody, X => (Head1; Head2), Sentence),
  grammar:noun(Number, XBody),
  grammar:verb_phrase(Number, ((X => Head1); (X => Head2))).

sentence_body([Literal :- true]) -->
  grammar:proper_noun(Number, Noun),
  grammar:verb_phrase(Number, Noun => Literal).

sentence_body([negation(Literal) :- true]) -->
  grammar:proper_noun(Number, Noun),
  grammar:verb_phrase(Number, negation(Noun => Literal)).

sentence_body([(Literal1; Literal2) :- true]) -->
  grammar:proper_noun(Number, Noun),
  grammar:verb_phrase(Number, ((Noun => Literal1); (Noun => Literal2))).
