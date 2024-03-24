%% engine: Question-answering module.
%
% This module provides the question-answering engine of the system.
%
:- module(engine, [
    prove_question/2,
    prove_question/3,
    prove_question_tree/3
  ]).

% --- Imports ---

:- use_module(grammar).
:- use_module(sentence).
:- use_module(utils).

% --- Question Answering ---

%% prove_question(+Question, +SessionId, -Output)
%
% The prove_question/3 predicate tries to prove the question based on the known facts
% for the session. If the question can be proved, it transforms it into a sentence and
% stores the sentence in the output. If the question cannot be proved, it stores a
% default response in the output.
%
% @param +Question: The question to be proven.
% @param +SessionId: The identifier for the session.
% @param -Output: The output generated based on the proof.
%
prove_question(Question, SessionId, Output) :-
  % Find all known facts for the session.
  findall(Fact, utils:known_fact(SessionId, Fact), FactList),
  % Try to prove the question based on the known facts.
  (   prove_from_known_facts(Question, FactList) ->
        % If the question can be proved, transform it into a clause.
        transform(Question, Clause),
        % Transform the clause into a sentence.
        phrase(sentence:sentence(Clause), OutputList),
        % Transform the sentence into a string.
        atomics_to_string(OutputList, ' ', Output)
  ;   % If the question cannot be proved, store a default response in the output.
      Output = 'I do not know that is true.'
  ).

%% prove_question(+Question, -Output)
%
% The prove_question/2 predicate is a simplified version of prove_question/3 that is
% suitable for maplist or similar operations. It retrieves all known facts, irrespective
% of their session, and tries to prove the question.
%
% @param +Question: The question to be proven.
% @param -Output: The output generated based on the proof.
%
prove_question(Question, Output) :-
  % Find all known facts, irrespective of their session.
  findall(Fact, utils:known_fact(_SessionId, Fact), FactList),
  % Try to prove the question based on the known facts.
  (   prove_from_known_facts(Question, FactList) ->
        % If the question can be proved, transform it into a clause.
        transform(Question, Clause),
        % Transform the clause into a sentence.
        phrase(sentence:sentence(Clause), OutputList),
        % Transform the sentence into a string.
        atomics_to_string(OutputList, ' ', Output)
  ;   % If the question cannot be proved, store a default response in the output.
      Output = ''
  ).

%% prove_question_tree(+Question, +SessionId, -Output)
%
% The prove_question_tree/3 predicate is an extended version of prove_question/3 that
% constructs a proof tree. If the question can be proved, it transforms each step of the
% proof into a sentence and stores the sentences in the output.
%
% @param +Question: The question to be proven.
% @param +SessionId: The identifier for the session.
% @param -Output: The output generated based on the proof.
%
prove_question_tree(Question, SessionId, Output) :-
  % Find all known facts for the session.
  findall(Fact, utils:known_fact(SessionId, Fact), FactList),
  % Try to prove the question based on the known facts.
  (   prove_from_known_facts(Question, FactList, [], ProofList) ->
        % If the question can be proved, transform each step of the proof into a sentence.
        maplist(proof_step_message, ProofList, OutputListTemp),
        % Transform the last step of the proof into a sentence.
        phrase(sentence:sentence_body([(Question :- true)]), Clause),
        % Transform the clause into a sentence.
        atomic_list_concat([therefore|Clause], ' ', LastClause),
        % Append the last step to the output.
        append(OutputListTemp, [LastClause], OutputList),
        % Transform the sentences into strings.
        atomic_list_concat(OutputList, ', ', Output)
  ;   % If the question cannot be proved, store a default response in the output.
      Output = 'I do not think that is true.'
  ).

%% proof_step_message(+Proof:atom, -Message)
%
% The proof_step_message/2 predicate transforms a proof step into a sentence.
%
proof_step_message(proof(_, Fact), Message):-
  known_fact_output(Fact, Message).

proof_step_message(n(Fact), Message):-
  known_fact_output([(Fact :- true)], FactMessage),
  atomic_list_concat(['I do not know that', FactMessage], ' ', Message).

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

% Base case: If the query is true, we are done.
prove_from_known_facts(true, _FactList, ProofList, ProofList) :- !.

% Recursive case: If the query is a conjunction, try to prove each conjunct.
prove_from_known_facts((ConjunctA, ConjunctB), FactList, ProofList, Proof) :-
  !,
  find_clause((ConjunctA :- C), Fact, FactList),
  utils:concatenate_conjunctive(C, ConjunctB, D),
  prove_from_known_facts(D, FactList, [proof((ConjunctA, ConjunctB), Fact)|ProofList], Proof).

prove_from_known_facts(QuestionA, FactList, ProofList, Proof) :-
  % Find a clause of the form 'if QuestionB then QuestionA'
  find_clause((QuestionA :- QuestionB), Fact, FactList),
  % Try to prove QuestionB based on the known facts.
  prove_from_known_facts(QuestionB, FactList, [proof(QuestionA, Fact)|ProofList], Proof).

prove_from_known_facts(A, FactList):-
  prove_from_known_facts(A, FactList, [], _Proof).

% --- Utilities ---

%% find_clause(+Clause:atom, +Fact:atom, +FactList:list)
%
% The find_clause/3 predicate finds a clause in the list of facts that unifies with the
% given clause and stores the fact in the output.
%
% @param +Clause: The clause to be found.
% @param +Fact: The fact to be stored in the output.
% @param +FactList: The list of facts to be searched.
%
% Base case: If the clause is found, store the fact in the output.
find_clause(Clause, Fact, [Fact|_FactList]):-
  % Avoid instantiating Fact!
  copy_term(Fact, [Clause]).
% Recursive case: If the clause is not found, search the rest of the list.
find_clause(Clause, Fact, [_Fact|FactList]):-
  find_clause(Clause, Fact, FactList).

%% transform(+A:atom, -B:list)
%
% The transform/2 predicate transforms a term into a list of clauses.
%
% @param +A: The term to be transformed.
% @param -B: The list of clauses generated based on the term.
%
% Base case: If the term is a conjunction, transform each conjunct.
transform((A, B), [(A :- true)|Rest]) :-
  !,
  transform(B, Rest).
% Recursive case: If the term is not a conjunction, transform it into a clause.
transform(A, [(A :- true)]).

% --- Commands ---

%% find_known_facts(-Output)
%
% The find_known_facts/1 predicate finds all known facts and transforms them into
% sentences.
%
find_known_facts(Output) :-
  % Find all known facts, irrespective of their session.
  findall(Fact, utils:known_fact(_SessionId, Fact), FactList),
  % Transform each fact into a response.
  maplist(known_fact_output, FactList, OutputList),
  % If no facts are known, store a default response in the output.
  (   OutputList = [] -> Output = 'I do not know anything.'
  % Otherwise, store the concatenated responses in the output.
  ;   atomic_list_concat(OutputList, '. ', Output)
  ).

%% find_all_results(+ProperNoun, -Output)
%
% The find_all_results/2 predicate finds all known facts that have a proper noun as an
% argument and transforms them into sentences.
%
% @param +ProperNoun: The proper noun to be searched for.
% @param -Output: The output generated based on the search.
%
find_all_results(ProperNoun, Output) :-
  findall(
    Question,
    (
      % Find all predicates that take a single argument.
      grammar:predicate(Predicate, 1, _Words),
      % For each predicate, construct a question with the proper noun as its argument.
      Question=..[Predicate, ProperNoun]
    ),
    QuestionList
  ),
  % Try to prove each question in the list and store the responses.
  maplist(prove_question, QuestionList, QuestionOutputList),
  % Remove questions that could not be proved from the list.
  delete(QuestionOutputList, '', OutputList),
  % If no questions could be proved, store a default response in the output.
  (   OutputList = [] ->
        atomic_list_concat(['I do not know anything about', ProperNoun], ' ', Output)
  % Otherwise, store the concatenated responses in the output.
  ;   otherwise ->
        atomic_list_concat(OutputList, '. ', Output)
  ).

%% known_fact_output(+Fact:atom, -Output)
%
% The known_fact_output/2 predicate transforms a fact into a sentence.
%
% @param +Fact: The fact to be transformed.
% @param -Output: The output generated based on the fact.
%
known_fact_output(Fact, Output):-
  % Transform the fact into a sentence.
  phrase(sentence:sentence_body(Fact), Sentence),
  % Transform the sentence into a string.
  atomics_to_string(Sentence, ' ', Output).
