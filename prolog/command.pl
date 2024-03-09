% Parse commands.

:- module(command, [command/3]).

:- use_module(engine).
:- use_module(grammar).
:- use_module(sentence).
:- use_module(utils).

command(g(true, "I can reason logically.")) --> [what, can, you, do].

command(g(retractall(utils:known_fact(_SessionId, Fact)), "I forgot that.")) -->
  [forget],
  sentence:sentence(Fact).

command(g(retractall(utils:known_fact(_SessionId, _Fact)), "I forgot everything.")) --> [forget, everything].

command(g(engine:find_known_facts(Output), Output)) -->
  [spill, the, beans].

command(g(engine:find_all_results(ProperNoun, Output), Output)) -->
  [tell, me, about],
  grammar:proper_noun(singular, ProperNoun).

command(g(engine:prove_question_tree(QuestionBody, _, Output), Output)) -->
  [explain, why],
  sentence:sentence_body([(QuestionBody :- true)]).
