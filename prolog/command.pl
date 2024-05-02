%% command: Command parser.
%
% This module parses commands, converts them into goals, and executes them.
%
:- module(command, [command/3]).

% --- Imports ---

:- use_module(engine).
:- use_module(grammar).
:- use_module(sentence).
:- use_module(utils).

% --- Command parser ---

%% command(-Goal:atom, -Output:string, +Words:list)//
%
% The command//3 DCG rule parses a list of atoms into a goal.
% It executes the goal with the engine module, outputs the response.
%
% @param -Goal The goal to execute.
% @param -Output The generated output.
% @param +Words The command (a list of atoms).
%

% Remove a known fact.
command(goal(retractall(utils:known_fact(Fact)), 'I forgot that.')) -->
  [forget],
  sentence:sentence(Fact).

% Remove all known facts.
command(goal(retractall(utils:known_fact(_Fact)), 'I forgot everything.')) -->
  [forget, everything].

% Output all known facts.
command(goal(engine:find_known_facts(Output), Output)) -->
  [spill, the, beans].

% Output all known facts about a proper noun.
command(goal(engine:find_known_facts_noun(ProperNoun, Output), Output)) -->
  [tell, me, about],
  grammar:proper_noun(singular, ProperNoun).

% Output the proof tree for a question.
command(goal(engine:prove_question_tree(Question, Output), Output)) -->
  [explain, why],
  sentence:sentence_body([(Question :- true | _)]).
