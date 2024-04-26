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

% sentence_body(Sentence) -->
%   grammar:determiner(Number, ToBody, ToHead, Sentence),
%   grammar:noun(Number, ToBody),
%   grammar:verb_phrase(Number, ToHead).

% sentence_body(Sentence) -->
%   grammar:determiner(Number, ToBody, X => negation(Head), Sentence),
%   grammar:noun(Number, ToBody),
%   grammar:verb_phrase(Number, negation(X => Head)).

% sentence_body(Sentence) -->
%   grammar:determiner(Number, ToBody, HeadList, Sentence),
%   grammar:noun(Number, ToBody),
%   grammar:verb_phrase(Number, (disjunction(HeadList))).

% sentence_body([Head :- true]) -->
%   grammar:proper_noun(Number, ProperNoun),
%   grammar:verb_phrase(Number, ProperNoun => Head).

% sentence_body([negation(Head) :- true]) -->
%   grammar:proper_noun(Number, X),
%   grammar:verb_phrase(Number, negation(X => Head)).

sentence_body([disjunction(HeadList) :- true]) -->
  grammar:proper_noun(Number, ProperNoun),
  grammar:verb_phrase(Number, ProperNoun => disjunction(HeadList)).
