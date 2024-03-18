% Question-answering engine.

:- module(engine, [
    prove_question/2,
    prove_question/3,
    prove_question_tree/3
  ]).

:- use_module(grammar).
:- use_module(sentence).
:- use_module(utils).

% --- Question Answering ---

prove_question(Question, SessionId, Output) :-
  findall(Fact, utils:known_fact(SessionId, Fact), FactList),
  (   prove_from_known_facts(Question, FactList) ->
        transform(Question, Clause),
        phrase(sentence:sentence(Clause), OutputList),
        atomics_to_string(OutputList, " ", Output)
  ;   Output = 'I do not know that is true.'
  ).

% A binary version that can be used in maplist/3.
prove_question(Question, Output) :-
  findall(Fact, utils:known_fact(_SessionId, Fact), FactList),
  (   prove_from_known_facts(Question, FactList) ->
        transform(Question, Clause),
        phrase(sentence:sentence(Clause), OutputList),
        atomics_to_string(OutputList, " ", Output)
  ;   Output = ""
  ).

% A version that constructs a proof tree.
prove_question_tree(Question, SessionId, Output) :-
  findall(Fact, utils:known_fact(SessionId, Fact), FactList),
  (   prove_from_known_facts(Question, FactList, [], ProofList) ->
        maplist(proof_step_message, ProofList, OutputListTemp),
        phrase(sentence:sentence_body([(Question :- true)]), Clause),
        atomic_list_concat([therefore|Clause], " ", Last),
        append(OutputListTemp, [Last], OutputList),
        atomic_list_concat(OutputList, ", ", Output)
  ;   Output = 'I do not think that is true.'
  ).

% Covert a proof step to an message.
proof_step_message(proof(_, Fact), Message):-
  known_fact_output(Fact, Message).

proof_step_message(n(Fact), Message):-
  known_fact_output([(Fact :- true)], FactMessage),
  atomic_list_concat(['I do not know that', FactMessage], " ", Message).

% --- Facts ---

is_fact_known([Fact], SessionId) :-
  findall(Fact, utils:known_fact(SessionId, Fact), FactList1),
  utils:try(
    (
      numbervars(Fact, 0, _),
      Fact = (Head :- Body),
      engine:add_body_to_facts(Body, FactList1, FactList2),
      engine:prove_from_known_facts(Head, FactList2)
    )
  ).

add_body_to_facts((A, B), FactList1, FactList) :-
  !,
  add_body_to_facts(A, FactList1, FactList2),
  add_body_to_facts(B, FactList2, FactList).

add_body_to_facts(A, FactList1, [[(A :- true)]|FactList1]).

% --- Meta-interpreter ---

% The third argument is an accumulator for proofs.
prove_from_known_facts(true, _FactList, ProofList, ProofList) :- !.

prove_from_known_facts((A, B), FactList, ProofList, Proof) :-
  !,
  find_clause((A :- C), Fact, FactList),
  utils:concatenate_conjunctive(C, B, D),
  prove_from_known_facts(D, FactList, [proof((A, B), Fact)|ProofList], Proof).

prove_from_known_facts(A, FactList, ProofList, Proof) :-
  find_clause((A :- B), Fact, FactList),
  prove_from_known_facts(B, FactList, [proof(A, Fact)|ProofList], Proof).

% A version that does not construct proofs.
prove_from_known_facts(A, FactList):-
  prove_from_known_facts(A, FactList, [], _Proof).

% Handle negation.
prove_from_known_facts(not(Goal), FactList, ProofList, Proof) :-
  % Cut to prevent backtracking.
  !,
  % If the goal cannot be proven...
  \+ prove_from_known_facts(Goal, FactList, ProofList, Proof),
  % ...then add a proof step for negation.
  ProofList = [proof(not(Goal), n(Goal))] .

% Handle negated facts.
prove_from_known_facts(A, FactList, ProofList, Proof) :-
  find_clause((A :- B), Fact, FactList),
  prove_from_known_facts(B, FactList, [proof(A, Fact)|ProofList], Proof).

prove_from_known_facts(A, FactList, ProofList, Proof) :-
  find_clause(not(A), Fact, FactList),
  % Fact directly supports the negation.
  ProofList = [proof(A, Fact)].

% --- Utilities ---

find_clause(Clause, Fact, [Fact|_FactList]):-
  % Do not instantiate Fact.
  copy_term(Fact, [Clause]).

find_clause(Clause, Fact, [_Fact|FactList]):-
  find_clause(Clause, Fact, FactList).

% Transform an instantiated and possibly conjunctive query to a list of clauses.
transform((A, B), [(A :- true)|Rest]) :-
  !,
  transform(B, Rest).

transform(A, [(A :- true)]).

% --- Commands ---

% Find all stored facts.
find_known_facts(Output) :-
  findall(Fact, utils:known_fact(_SessionId, Fact), FactList),
  maplist(known_fact_output, FactList, OutputList),
  (   OutputList = [] -> Output = "I do not know anything."
  ;   atomic_list_concat(OutputList, ". ", Output)
  ).

% Find all results about a proper noun.
find_all_results(ProperNoun, Output) :-
  findall(Question, (grammar:predicate(Predicate, 1, _Words), Question=..[Predicate, ProperNoun]), QuestionList),
  maplist(prove_question, QuestionList, Message),
  delete(Message, "", OutputList),
  (   OutputList = [] -> atomic_list_concat(["I do not know anything about", ProperNoun], " ", Output)
  ;   otherwise -> atomic_list_concat(OutputList, ". ", Output)
  ).

% Convert a stored fact to an output string.
known_fact_output(Fact, Output):-
  phrase(sentence:sentence_body(Fact), Sentence),
  atomics_to_string(Sentence, " ", Output).
