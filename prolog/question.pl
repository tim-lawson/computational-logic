%% question: Question parser.
%
% This module parses questions and converts them into clauses.
%
:- module(question, [question/3]).

% --- Imports ---

:- use_module(grammar).

% --- Question parser ---

%% question(+Question:list)//
%
% The question//1 DCG rule parses a list of atoms into a question.
%
% @param Question The list of atoms.
%
question(Question) -->
  question_word,
  question_body(Question).

question_word --> [].

%% question_body(Question:list)//
%
% The question_body//1 DCG rule parses a list of atoms into the body of a question.
%
% @param Question The list of atoms.
%

% "Who VerbPhrase?" questions.
question_body(Question) -->
  [who],
  grammar:verb_phrase(singular, _X => Question).

% "Is ProperNoun (not) Property?" questions.
question_body(Question) -->
  [is],
  grammar:proper_noun(Number, Noun),
  grammar:property(Number, Noun => Question).

question_body(negate(Question)) -->
  [is],
  grammar:proper_noun(Number, Noun),
  [not],
  grammar:property(Number, Noun => Question).

% "Does ProperNoun (not) VerbPhrase?" questions.
question_body(Question) -->
  [does],
  grammar:proper_noun(_, Noun),
  grammar:verb_phrase(_, Noun => Question).

question_body(negate(Question)) -->
  [does],
  grammar:proper_noun(_, Noun),
  [not],
  grammar:verb_phrase(_, Noun => Question).