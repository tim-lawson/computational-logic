%% command: Command parser.
%
% This module is parses commands and converts them into goals to be executed by the
% engine module.
%
:- module(command, [command/3]).

% --- Imports ---

:- use_module(engine).
:- use_module(grammar).
:- use_module(sentence).
:- use_module(utils).

% --- Command parser ---

% command(-Goal:atom, -Output:string, +Words:list)//
%
% The command//3 predicate parses a list of atoms into a goal to be executed by the
% engine module and stored the generated output.
%
% @param -Goal The goal to execute.
% @param -Output The generated output.
% @param +Words The list of atoms to parse.
%
command(goal(true, 'I can reason logically.')) -->
  [what, can, you, do].

% Remove a known fact.
command(goal(retractall(utils:known_fact(_SessionId, Fact)), 'I forgot that.')) -->
  [forget],
  sentence:sentence(Fact).

% Remove all known facts.
command(goal(retractall(utils:known_fact(_SessionId, _Fact)), 'I forgot everything.')) -->
  [forget, everything].

% Output all known facts.
command(goal(engine:find_known_facts(Output), Output)) -->
  [spill, the, beans].

% Output all known facts about a proper noun.
command(goal(engine:find_all_results(ProperNoun, Output), Output)) -->
  [tell, me, about],
  grammar:proper_noun(singular, ProperNoun).

% Output the proof tree for a question.
command(goal(engine:prove_question_tree(Question, _, Output), Output)) -->
  [explain, why],
  sentence:sentence_body([(Question :- true)]).
