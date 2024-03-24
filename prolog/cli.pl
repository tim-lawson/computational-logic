%% cli: Command-line interface module.
%
% This module provides the command-line interface for interacting with the system.
%
:- module(cli, [cli/0]).

% --- Imports ---

:- use_module(engine).
:- use_module(utils).
:- use_module(sentence).
:- use_module(question).
:- use_module(command).

% --- Configuration ---

% This predicate controls whether debug outputs are enabled.
debug_enabled(on).

% --- Command-line interface ---

:- prompt(_Previous,'cli > ').

%% cli
%
% The cli/0 predicate is the main entry point for the command-line interface. It reads
% a line of user input, determines the corresponding output, and prints the output. If
% the input is 'stop', the predicate terminates. Otherwise, it recurses to read the next
% line of input.
%
cli :-
  % Read a line of user input.
  read_line_to_codes(user_input, Codes),
  % Convert the input to an atom.
  atom_codes(Input, Codes),
  (
    Input \== stop ->
    handle_input(1, Input, Output),
    writeln(Output),
    flush_output,
    % Recurse to read the next line of input.
    cli
  )
  % If the input is 'stop', terminate.
  ; true.

%% handle_input(+SessionId:integer, +Input:string, -Output:string)
%
% The handle_input/3 predicate is the main entry point for handling user input. It
% processes the input and generates the corresponding output. The input can be a
% sentence, a question, or a command.
%
% @param +SessionId The session identifier.
% @param +Input The user input.
% @param -Output The generated output.
%
handle_input(SessionId, Input, Output) :-
  write_debug('trying to handle input...'),
  preprocess_input(Input, InputList),

  (   write_debug('trying to parse as sentence...'),
      phrase(sentence:sentence(Sentence), InputList),
      handle_sentence(Sentence, SessionId, Output)

  ;   write_debug('trying to parse as question...'),
      phrase(question:question(Question), InputList),
      handle_question(Question, SessionId, Output)

  ;   write_debug('trying to parse as command...'),
      phrase(command:command(goal(Command, Output)), InputList),
      write_debug(goal(Command, Output)),
      call(Command) -> true

  ;   otherwise ->
      write_debug('could not parse...'),
      atomic_list_concat(['I do not understand.'], ' ', Output)
  ).

%% preprocess_input(+Input:string, -Output:list)
%
% The preprocess_input/2 predicate splits the user input into a list of words, and
% transforms each word to a lowercase atom.
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

%% handle_sentence(+Sentence:list, +SessionId:integer, -Output:string)
%
% The handle_sentence/3 predicate handles a sentence input. It checks if the fact is
% already known, and if not, adds it to the known facts.
%
% @param +Sentence The sentence (a list of atoms).
% @param +SessionId The session identifier.
% @param -Output The generated output.
%
handle_sentence(Sentence, SessionId, Output) :-
  write_debug(fact(Sentence)),
  % If the fact is already known, respond accordingly.
  (   engine:is_fact_known(Sentence, SessionId) ->
      atomic_list_concat(['I know that.'], ' ', Output)
  % Otherwise, add it to the known facts and respond accordingly.
  ;   assertz(utils:known_fact(SessionId, Sentence)),
      atomic_list_concat(['I will remember that.'], ' ', Output)
  ).

%% handle_question(+Question:list, +SessionId:integer, -Output:string)
%
% The handle_question/3 predicate handles a question input. It tries to prove the
% question using the question-answering engine.
%
% @param +Question The question (a list of atoms).
% @param +SessionId The session identifier.
% @param -Output The generated output.
%
handle_question(Question, SessionId, Output) :-
  % Try to prove the question.
  engine:prove_question(Question, SessionId, Output) -> true.

%% write_debug(+X)
%
% The write_debug/1 predicate writes a debug message to the console.
%
write_debug(X) :-
    % If debug outputs are enabled...
    debug_enabled(on),
    % Set the output colour to green.
    write(user_output, '\033[92m'),
    writeln(user_output, X),
    % Reset the output colour.
    write(user_output, '\033[0m'),
    flush_output(user_output).
