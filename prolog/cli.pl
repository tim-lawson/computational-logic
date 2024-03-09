% Command-line interface.

:- module(cli, [cli/0]).

% --- Imports ---

:- use_module(engine).
:- use_module(utils).

% Input types.
:- use_module(sentence).
:- use_module(question).
:- use_module(command).

% Whether debug outputs are enabled.
debug_enabled(on).

% --- Command-line interface ---

:- prompt(_Previous,'cli > ').

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

% --- Input handling ---

handle_input(SessionId, Input, Output) :-
  write_debug("trying to handle input..."),
  preprocess_input(Input, InputList),

  % Try to parse the input.
  (   write_debug("trying to parse as sentence..."),
      phrase(sentence:sentence(Fact), InputList),
      handle_sentence(Fact, SessionId, Output)

  ;   write_debug("trying to parse as question..."),
      phrase(question:question(Question), InputList),
      handle_question(Question, SessionId, Output)

  ;   write_debug("trying to parse as command..."),
      phrase(command:command(g(Command, Output)), InputList),
      write_debug(g(Command, Output)),
      call(Command) -> true

  ;   otherwise ->
      write_debug("could not parse..."),
      atomic_list_concat(['I do not understand.'], ' ', Output)
  ),
  write_debug(output(Output)).

% Convert a string to a list of lowercase tokens.
preprocess_input(Input, TokenList) :-
  write_debug(input(Input)),
  split_string(Input, " ", "", StringList),
  maplist(string_lower, StringList, StringListLowercase),
  maplist(atom_string, TokenList, StringListLowercase).

% If the input is a sentence...
handle_sentence(Fact, SessionId, Output) :-
  write_debug(fact(Fact)),
  % If the fact is already known, respond accordingly.
  (   engine:is_fact_known(Fact, SessionId) ->
      atomic_list_concat(['I know that.'], ' ', Output)
  % Otherwise, add it to the known facts and respond accordingly.
  ;   assertz(utils:known_fact(SessionId, Fact)),
      atomic_list_concat(['I will remember that.'], ' ', Output)
  ).

% If the input is a question...
handle_question(Question, SessionId, Output) :-
  write_debug(question(Question)),
  % Try to prove the question.
  engine:prove_question(Question, SessionId, Output) -> true.

% --- Debugging ---

% Write a debug message to the console.
write_debug(Atom) :-
    % If debug outputs are enabled...
    debug_enabled(on),
    % Set the output colour to green.
    write(user_output, '\033[92m'),
    writeln(user_output, Atom),
    % Reset the output colour.
    write(user_output, '\033[0m'),
    flush_output(user_output).
