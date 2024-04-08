%% engine: Question-answering module.
%
% This module provides the question-answering engine of the system.
%
:- module(engine, [
    prove_question/2,
    prove_question_list/2,
    prove_question_tree/2
  ]).

% --- Imports ---

:- use_module(grammar).
:- use_module(sentence).
:- use_module(utils).

% --- Question Answering ---

%% prove_question(+Question:atom, -Output:string)
%
% The prove_question/2 predicate tries to prove a question based on the known facts.
% If the question can be proved, it transforms it into a sentence and stores it in the
% output.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question(Question, Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
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

%% prove_question_list(+Question:atom, -Output:string)
%
% The prove_question_list/2 predicate is like prove_question/2, except it generates the
% empty string when the question cannot be proved. This is convenient to use with
% maplist, e.g. in find_all_results/2.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question_list(Question, Output) :-
  findall(Fact, utils:known_fact(Fact), FactList),
  (   prove_from_known_facts(Question, FactList) ->
        transform(Question, Clause),
        phrase(sentence:sentence(Clause), OutputList),
        atomics_to_string(OutputList, ' ', Output)
  ;   % If the question cannot be proved, store the empty string in the output.
      Output = ''
  ).

%% prove_question_tree(+Question:atom, -Output:string)
%
% The prove_question_tree/2 predicate is an extended version of prove_question/2 that
% constructs a proof tree. If the question can be proved, it transforms each step of the
% proof into a sentence and stores the sentences in the output.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question_tree(Question, Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
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

%% proof_step_message(+Proof:atom, -Output:string)
%
% The proof_step_message/2 predicate transforms a proof step into an output.
%
% @param +Proof: The proof step.
% @param -Output: The output generated.
%
proof_step_message(proof(_, Fact), Output) :-
  known_fact_output(Fact, Output).

proof_step_message(unknown(Fact), Output):-
  known_fact_output([(Fact :- true)], FactOutput),
  % If the fact is not known, store a default response in the output.
  atomic_list_concat(['I do not know that', FactOutput], ' ', Output).

% --- Facts ---

%% is_fact_known(+FactList:list)
%
% The is_fact_known/2 predicate checks if a fact is known.
%
% @param +FactList: The list of facts to check.
%
is_fact_known([Fact]) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactListOld),
  % Try to prove the fact based on the known facts.
  utils:try(
    (
      % Try to unify the free variables in the fact with anything else.
      numbervars(Fact, 0, _),
      % Construct a clause from the fact.
      Fact = (Head :- Body),
      % Add the body of the clause to the known facts.
      engine:add_clause_to_facts(Body, FactListOld, FactListNew),
      % Try to prove the head of the clause based on the known facts.
      engine:prove_from_known_facts(Head, FactListNew)
    )
  ).

%% add_clause_to_facts(+Clause:atom, +FactListOld:list, -FactListNew:list)
%
% The add_clause_to_facts/3 predicate adds a clause to the list of facts.
%
% @param +Clause: The clause.
% @param +FactListOld: The list of facts to update.
% @param -FactListNew: The updated list of facts.
%

% Recursive case: If the clause is a conjunction, add each conjunct to the list of facts.
add_clause_to_facts((Conjunct1, Conjunct2), FactListOld, FactListNew) :-
  !,
  % Add the first conjunct to the list of facts.
  add_clause_to_facts(Conjunct1, FactListOld, FactListTemp),
  % Add the second conjunct to the list of facts.
  add_clause_to_facts(Conjunct2, FactListTemp, FactListNew).

% Base case: If the body is not a conjunction, add it to the list of facts.
add_clause_to_facts(Clause, FactList, [(Clause :- true)|FactList]).

%% known_fact_output(+Fact:atom, -Output)
%
% The known_fact_output/2 predicate generates an output from a fact.
%
% @param +Fact: The fact.
% @param -Output: The generated output.
%
known_fact_output(Fact, Output) :-
  % Transform the fact into a sentence.
  phrase(sentence:sentence_body(Fact), Sentence),
  % Transform the sentence into a string.
  atomics_to_string(Sentence, ' ', Output).


% --- Meta-interpreter ---

%% prove_from_known_facts(+Clause:atom, +FactList:list, -ProofList:list, -Proof)
%
% The prove_from_known_facts/4 predicate tries to prove a clause based on a list of facts.
% If the clause can be proved, it stores the proof in the output. The proof is a list of
% steps, where each step is a fact that was used to prove the clause.
%
% @param +Clause: The clause to prove.
% @param +FactList: The list of facts to use.
% @param +ProofList: The accumulator for the proof.
% @param -Proof: The generated proof.
%

% Base case: If the clause is true, we are done.
prove_from_known_facts(true, _FactList, ProofList, ProofList) :- !.

% Recursive case: If the clause is a conjunction, try to prove each conjunct.
prove_from_known_facts((Conjunct1, Conjunct2), FactList, ProofList, Proof) :-
  !,
  % Try to prove the first conjunct (find a clause of the form 'if Body1 then Conjunct1').
  find_clause((Conjunct1 :- Body1), Fact, FactList),
  % Concatenate the body of the proof with the second conjunct.
  utils:concatenate_conjunctive(Body1, Conjunct2, Body2),
  % Try to prove the concatenated clauses.
  prove_from_known_facts(
    Body2,
    FactList,
    [proof((Conjunct1, Conjunct2), Fact)|ProofList],
    Proof
  ).

prove_from_known_facts(Clause, FactList, ProofList, Proof) :-
  (
      % Try to prove using default reasoning.
      find_clause((grammar:default(Clause) :- Body), Fact, FactList),
      % Try to prove the body of the clause.
      prove_from_known_facts(Body, FactList, [proof((Clause :- Body), Fact)|ProofList], Proof)
  )
  ;   % Try to prove the clause (find a clause of the form 'if Body then Clause').
      find_clause((Clause :- Body), Fact, FactList),
      % Try to prove the body of the clause.
      prove_from_known_facts(Body, FactList, [proof(Clause, Fact)|ProofList], Proof).

prove_from_known_facts(Clause, FactList) :-
  prove_from_known_facts(Clause, FactList, [], _Proof).

% --- Utilities ---

%% find_clause(+Clause:atom, +Fact:atom, +FactList:list)
%
% The find_clause/3 predicate finds a clause in the list of facts that unifies with the
% given clause and stores the fact in the output.
%
% @param +Clause: The clause to find.
% @param +Fact: The fact to store in the output.
% @param +FactList: The list of facts to search.
%

% Base case: If the clause is found, store the fact in the output.
find_clause(Clause, Fact, [Fact|_FactList]) :-
  % Avoid instantiating Fact!
  copy_term(Fact, [Clause]).

% Recursive case: If the clause is not found, search the rest of the list.
find_clause(Clause, Fact, [_Fact|FactList]) :-
  find_clause(Clause, Fact, FactList).

%% transform(+Term:atom, -ClauseList:list)
%
% The transform/2 predicate transforms a term into a list of clauses.
%
% @param +Term: The term to transform.
% @param -ClauseList: The list of clauses generated based on the term.
%

% Base case: If the term is a conjunction, transform each conjunct.
transform((Term1, Term2), [(Term1 :- true)|Rest]) :-
  !,
  transform(Term2, Rest).

% Recursive case: If the term is not a conjunction, transform it into a clause.
transform(Term, [(Term :- true)]).

% --- Commands ---

%% find_known_facts(-Output)
%
% The find_known_facts/1 predicate finds all known facts and transforms them into
% sentences.
%
find_known_facts(Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
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
% @param +ProperNoun: The proper noun to search for.
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
  % Try to prove each question in the list and store the responses (or empty strings).
  maplist(prove_question_list, QuestionList, QuestionOutputList),
  % Remove questions that could not be proved from the list.
  delete(QuestionOutputList, '', OutputList),
  % If no questions could be proved, store a default response in the output.
  (   OutputList = [] ->
        atomic_list_concat(['I do not know anything about', ProperNoun], ' ', Output)
  % Otherwise, store the concatenated responses in the output.
  ;   otherwise ->
        atomic_list_concat(OutputList, '. ', Output)
  ).