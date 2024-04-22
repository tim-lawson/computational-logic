%% cli: Command-line interface module.
%
% This module provides the command-line interface for interacting with the system.
%
:- module(cli, [cli/0]).

% --- Imports ---

:- use_module(library(debug)).
:- use_module(engine).
:- use_module(utils).
:- use_module(sentence).
:- use_module(question).
:- use_module(command).

% --- Command-line interface ---

:- prompt(_Previous,'cli > ').

%% cli
%
% The cli/0 predicate is the main entry point for the command-line interface.
% It reads a line of user input, determines the corresponding output, and prints the output.
% If the input is `stop', the predicate terminates.
% Otherwise, it calls itself to read the next line of input.
%
cli :-
  % Read a line of user input.
  read_line_to_codes(user_input, Codes),
  % Convert the input to an atom.
  atom_codes(Input, Codes),
  (
    Input \== stop ->
    handle_input(Input, Output),
    writeln(Output),
    flush_output,
    % Recurse to read the next line of input.
    cli
  )
  % If the input is 'stop', terminate.
  ; true.

%% handle_input(+Input:string, -Output:string)
%
% The handle_input/2 predicate preprocesses the input and tries to parse it as a sentence, question, or command.
% If the input cannot be parsed, it outputs a default response.
%
% @param +Input The user input.
% @param -Output The generated output.
%
handle_input(Input, Output) :-
  preprocess_input(Input, InputList),
  (
      % Try to parse the input as a sentence.
      phrase(sentence:sentence(Sentence), InputList),
      debug:debug('cli', 'parsed as sentence: ~q', [Sentence]),
      handle_sentence(Sentence, Output)
  ;
      % Try to parse the input as a question.
      phrase(question:question(Question), InputList),
      debug:debug('cli', 'parsed as question: ~q', [Question]),
      handle_question(Question, Output)
  ;
      % Try to parse the input as a command.
      phrase(command:command(goal(Command, Output)), InputList),
      debug:debug('cli', 'parsed as command: ~q', [Command]),
      call(Command) -> true
  ;
      otherwise -> atomic_list_concat(['I do not understand.'], ' ', Output)
  ).

%% preprocess_input(+Input:string, -Output:list)
%
% The preprocess_input/2 predicate splits the input on whitespace into a list of words.
% It transforms each word to a lowercase atom.
%
% @param +Input The user input.
% @param -Output The list of atoms.
%
preprocess_input(Input, Output) :-
  % Split the input into words.
  split_string(Input, ' ', '', InputList),
  % Transform each word to lowercase.
  maplist(string_lower, InputList, InputListLowercase),
  % Transform each word to an atom.
  maplist(atom_string, Output, InputListLowercase).

%% handle_sentence(+Sentence:list, -Output:string)
%
% The handle_sentence/2 predicate handles a sentence input.
% If the fact that corresponds to the sentence is not known, it is added to the known facts.
%
% @param +Sentence The sentence (a list of atoms).
% @param -Output The generated output.
%
handle_sentence(Sentence, Output) :-
  (
    % If the fact is already known, respond accordingly.
    engine:is_fact_known(Sentence) ->
    atomic_list_concat(['I know that.'], ' ', Output)
  ;
    % Otherwise, add it to the known facts and respond accordingly.
    assertz(utils:known_fact(Sentence)),
    atomic_list_concat(['I will remember that.'], ' ', Output)
  ).

%% handle_question(+Question:list, -Output:string)
%
% The handle_question/2 predicate handles a question input.
% It tries to prove the question and/or its negation with the question-answering engine.
%
% @param +Question The question (a list of atoms).
% @param -Output The generated output.
%
handle_question(Question, Output) :-
  % Try to prove the question.
  engine:prove_question(Question, Output) -> true.
