%% cli: Command-line interface module.
%
% This module provides the command-line interface for interacting with the system.
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

%% cli is nondet.
%
% The cli/0 predicate is the main entry point for the command-line interface. It reads
% a line of input from the user, determines the corresponding output, and prints the
% output. If the input is "stop", the predicate terminates. Otherwise, it recurses to
% read the next line of input.
%
cli :-
  % Read a line of input from the user.
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
  % If the input is "stop", terminate.
  ; true.

%% handle_input(+SessionId, +Input, -Output) is semidet.
%
% The handle_input/3 predicate is the main entry point for handling user input. It
% processes the input and generates the corresponding output. The input can be a
% sentence, a question, or a command.
%
% @param SessionId A unique identifier for the session.
% @param Input The user input as an atom.
% @param Output The generated output.
%
handle_input(SessionId, Input, Output) :-
  write_debug("trying to handle input..."),
  preprocess_input(Input, InputList),

  (   write_debug("trying to parse as sentence..."),
      phrase(sentence:sentence(Sentence), InputList),
      handle_sentence(Sentence, SessionId, Output)

  ;   write_debug("trying to parse as question..."),
      phrase(question:question(Question), InputList),
      handle_question(Question, SessionId, Output)

  ;   write_debug("trying to parse as command..."),
      phrase(command:command(goal(Command, Output)), InputList),
      write_debug(goal(Command, Output)),
      call(Command) -> true

  ;   otherwise ->
      write_debug("could not parse..."),
      atomic_list_concat(["I do not understand."], " ", Output)
  ).

%% preprocess_input(+Input, -TokenList) is det.
%
% The preprocess_input/2 predicate preprocesses the user input by tokenizing it and
% converting it to lowercase.
%
% @param Input The user input as an atom.
% @param TokenList The tokenized and lowercased input as a list of atoms.
preprocess_input(Input, TokenList) :-
  write_debug(input(Input)),
  split_string(Input, " ", "", List),
  maplist(string_lower, List, ListLowercase),
  maplist(atom_string, TokenList, ListLowercase).

%% handle_sentence(+Sentence, +SessionId, -Output) is semidet.
%
% The handle_sentence/3 predicate handles a sentence input. It checks if the fact is
% already known, and if not, adds it to the known facts.
%
% @param Sentence The fact as a list of atoms.
% @param SessionId A unique identifier for the session.
% @param Output The generated output.
%
handle_sentence(Sentence, SessionId, Output) :-
  write_debug(fact(Sentence)),
  % If the fact is already known, respond accordingly.
  (   engine:is_fact_known(Sentence, SessionId) ->
      atomic_list_concat(["I know that."], " ", Output)
  % Otherwise, add it to the known facts and respond accordingly.
  ;   assertz(utils:known_fact(SessionId, Sentence)),
      atomic_list_concat(["I will remember that."], " ", Output)
  ).

%% handle_question(+Question, +SessionId, -Output) is semidet.
%
% The handle_question/3 predicate handles a question input. It tries to prove the
% question using the question-answering engine.
%
% @param Question The question as a list of atoms.
% @param SessionId A unique identifier for the session.
% @param Output The generated output.
%
handle_question(Question, SessionId, Output) :-
  % write_debug(question(Question)),
  % Try to prove the question.
  engine:prove_question(Question, SessionId, Output) -> true.

%% write_debug(+X) is det.
%
% The write_debug/1 predicate writes a debug message to the console.
%
write_debug(X) :-
    % If debug outputs are enabled...
    debug_enabled(on),
    % Set the output colour to green.
    write(user_output, "\033[92m"),
    writeln(user_output, X),
    % Reset the output colour.
    write(user_output, "\033[0m"),
    flush_output(user_output).
