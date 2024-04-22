%% question: Question parser.
%
% This module parses questions and converts them into clauses.
%
:- module(question, [question/3]).

% --- Imports ---

:- use_module(grammar).

% --- Question parser ---

%% question(?Question:list)//
%
% The question//1 DCG rule parses a list of atoms into a question.
%
% @param Question The list of atoms.
%
question(Question) -->
  question_word,
  question_body(Question).

question_word --> [].

%% question_body(?Question:list)//
%
% The question_body//1 DCG rule parses a list of atoms into the body of a question.
%
% @param Question The list of atoms.
%

% "Who VerbPhrase?" questions.
question_body(Question) -->
  [who],
  grammar:verb_phrase(singular, _ => Question).

% "Is ProperNoun Property?" questions.
question_body(Question) -->
  [is],
  grammar:proper_noun(Number, X),
  grammar:property(Number, X => Question).

% "Is ProperNoun not Property?" questions.
question_body(negation(Question)) -->
  [is],
  grammar:proper_noun(Number, X),
  [not],
  grammar:property(Number, X => Question).

% "Does ProperNoun VerbPhrase?" questions.
question_body(Question) -->
  [does],
  grammar:proper_noun(_, X),
  grammar:verb_phrase(_, X => Question).

% "Does ProperNoun not VerbPhrase?" questions.
question_body(negation(Question)) -->
  [does],
  grammar:proper_noun(_, X),
  [not],
  grammar:verb_phrase(_, X => Question).