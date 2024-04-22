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
question_body(question(Clause, _, true)) -->
  [who],
  grammar:verb_phrase(singular, true, _ => Clause).

% "Is ProperNoun Property?" questions.
question_body(question(Clause, _, true)) -->
  [is],
  grammar:proper_noun(Number, X),
  grammar:property(Number, X => Clause).

% "Is ProperNoun not Property?" questions.
question_body(question(Clause, _, false)) -->
  [is],
  grammar:proper_noun(Number, X),
  [not],
  grammar:property(Number, X => Clause).

% "Does ProperNoun VerbPhrase?" questions.
question_body(question(Clause, _, true)) -->
  [does],
  grammar:proper_noun(_, X),
  grammar:verb_phrase(_, Truth, X => Clause).

% "Does ProperNoun not VerbPhrase?" questions.
question_body(question(Clause, always, false)) -->
  [does],
  grammar:proper_noun(_, X),
  [not],
  grammar:verb_phrase(_, false, X => Clause).