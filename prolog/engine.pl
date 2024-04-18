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
% The prove_question/2 predicate tries to prove a question from the known facts.
% If the question can be proved, it is transformed into a sentence output.
% Otherwise, a default response is output.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question(Question, Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
  % Try to prove the question based on the known facts.
  (   engine:prove_from_known_facts(Question, FactList, Truth) ->
        % If the question can be proved, transform it into a clause.
        utils:transform(Question, Truth, Clause),
        % Transform the clause into a sentence.
        phrase(sentence:sentence(Clause), OutputList),
        % Transform the sentence into a string.
        atomics_to_string(OutputList, ' ', Output)
  ;   % If the question cannot be proved, store a default response in the output.
      Output = 'I do not know that is true.'
  ).

%% prove_question_list(+Question:atom, -Output:string)
%
% The prove_question_list/2 predicate is equivalent to prove_question/2, except it outputs the empty string when the question cannot be proved.
% This predicate is suitable for use with maplist, such as in find_known_facts_noun/2.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question_list(Question, Output) :-
  findall(Fact, utils:known_fact(Fact), FactList),
  (   engine:prove_from_known_facts(Question, FactList, Truth) ->
        utils:transform(Question, Truth, Clause),
        phrase(sentence:sentence(Clause), OutputList),
        atomics_to_string(OutputList, ' ', Output)
  ;   % If the question cannot be proved, store the empty string in the output.
      Output = ''
  ).

%% prove_question_tree(+Question:atom, -Output:string)
%
% The prove_question_tree/2 predicate is an extended version of prove_question/2 that constructs a proof tree.
% If the question can be proved, it transforms each step of the proof into a sentence and concatenates the sentences into the output.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question_tree(Question, Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
  % Try to prove the question based on the known facts.
  (   prove_from_known_facts(Question, FactList, [], ProofList, Truth) ->
        % If the question can be proved, transform each step of the proof into a sentence.
        maplist(output_proof, ProofList, OutputListTemp),
        % Transform the last step of the proof into a sentence.
        phrase(sentence:sentence_body([(Question :- Truth)]), Clause),
        % Transform the clause into a sentence.
        atomic_list_concat([therefore|Clause], ' ', LastClause),
        % Append the last step to the output.
        append(OutputListTemp, [LastClause], OutputList),
        % Transform the sentences into strings.
        atomic_list_concat(OutputList, ', ', Output)
  ;   % If the question cannot be proved, store a default response in the output.
      Output = 'I do not think that is true.'
  ).

% --- Meta-interpreter ---

%% prove_from_known_facts(+Clause:atom, +FactList:list, -ProofList:list, -Proof:atom, -Truth:atom)
%
% The prove_from_known_facts/5 predicate tries to prove a clause based on a list of facts.
% If the clause can be proved, it stores the proof in the output. The proof is a list of
% steps, where each step is a fact that was used to prove the clause.
%
% @param +Clause: The clause to prove.
% @param +FactList: The list of facts to use.
% @param +ProofList: The accumulator for the proof.
% @param -Proof: The generated proof.
% @param -Truth: The truth-value of the clause.
%

% Base case: If the clause is true, we are done.
prove_from_known_facts(true, _FactList, ProofList, ProofList, true) :- !.

% Recursive case: If the clause is a conjunction, try to prove each conjunct.
prove_from_known_facts((Conjunct1, Conjunct2), FactList, ProofList, Proof, Truth) :-
  !,
  % Try to prove the first conjunct (find a clause of the form 'if Body1 then Conjunct1').
  utils:find_clause((Conjunct1 :- Body1), Fact, FactList),
  % Concatenate the body of the proof with the second conjunct.
  utils:concatenate_conjunctive(Body1, Conjunct2, Body2),
  % Try to prove the concatenated clauses.
  prove_from_known_facts(
    Body2,
    FactList,
    [proof((Conjunct1, Conjunct2), Fact)|ProofList],
    Proof,
    Truth
  ).

% Try to prove using default reasoning.
prove_from_known_facts(Clause, FactList, ProofList, Proof, Truth) :-
  utils:find_clause((default(Clause) :- Body), Fact, FactList),
  utils:write_debug("Found default clause"), utils:write_debug((default(Clause) :- Body)),
  prove_from_known_facts(Body, FactList, [proof((Clause :- Body), Fact)|ProofList], Proof, Truth).

% Try to prove using negation.
prove_from_known_facts(Clause, FactList, ProofList, Proof, false) :-
  utils:find_clause(negate(Clause :- Body), Fact, FactList),
  utils:write_debug("Found negate clause"), utils:write_debug((negate(Clause :- Body))),
  prove_from_known_facts(Body, FactList, [proof(negate(Clause, Body), Fact)|ProofList], Proof, true).

prove_from_known_facts(Clause, FactList, ProofList, Proof, Truth) :-
  % Try to prove the clause (find a clause of the form 'if Body then Clause').
  utils:find_clause((Clause :- Body), Fact, FactList),
  utils:write_debug("Found clause"), utils:write_debug((Clause :- Body)),
  % Try to prove the body of the clause.
  prove_from_known_facts(Body, FactList, [proof(Clause, Fact)|ProofList], Proof, Truth).

prove_from_known_facts(Clause, FactList, Truth) :-
  prove_from_known_facts(Clause, FactList, [], _Proof, Truth).

% --- Commands ---

%% find_known_facts(-Output)
%
% The find_known_facts/1 predicate finds all known facts and transforms them into sentences.
%
% @param -Output: The known facts or default response.
%
find_known_facts(Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
  % Transform each fact into a response.
  maplist(engine:output_known_fact, FactList, OutputList),
  % If no facts are known, store a default response in the output.
  (   OutputList = [] -> Output = 'I do not know anything.'
  % Otherwise, store the concatenated responses in the output.
  ;   atomic_list_concat(OutputList, '. ', Output)
  ).

%% find_known_facts_noun(+ProperNoun, -Output)
%
% The find_known_facts_noun/2 predicate finds all known facts with a proper-noun argument and transforms them into sentences.
%
% @param +ProperNoun: The proper noun.
% @param -Output: The known facts or default response.
%
find_known_facts_noun(ProperNoun, Output) :-
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
  maplist(engine:prove_question_list, QuestionList, QuestionOutputList),
  % Remove questions that could not be proved from the list.
  delete(QuestionOutputList, '', OutputList),
  % If no questions could be proved, store a default response in the output.
  (   OutputList = [] ->
        atomic_list_concat(['I do not know anything about', ProperNoun], ' ', Output)
  % Otherwise, store the concatenated responses in the output.
  ;   otherwise ->
        atomic_list_concat(OutputList, '. ', Output)
  ).

proof_step_message(unknown(Fact), Output):-
  known_fact_output([(Fact :- true)], FactOutput),
  % If the fact is not known, store a default response in the output.
  atomic_list_concat(['I do not know that', FactOutput], ' ', Output).

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
      engine:prove_from_known_facts(Head, FactListNew, _Truth)
    )
  ).

%% add_clause_to_facts(+Clause:atom, +FactListOld:list, -FactListNew:list)
%
% The add_clause_to_facts/3 predicate adds a clause to a list of facts.
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

%% output_known_fact(+Fact:atom, -Output:string)
%
% The output_known_fact/2 predicate transforms a known fact into a string output.
%
% @param +Fact: The fact.
% @param -Output: The generated output.
%
output_known_fact(Fact, Output) :-
  % Transform the fact into a sentence.
  phrase(sentence:sentence_body(Fact), Sentence),
  % Transform the sentence into a string.
  atomics_to_string(Sentence, ' ', Output).

%% output_proof(+Proof:atom, -Output:string)
%
% The output_proof/2 predicate transforms a proof step into a string output.
%
% @param +Proof: The proof step.
% @param -Output: The generated output.
%
output_proof(proof(_, Fact), Output) :-
  engine:output_known_fact(Fact, Output).

output_proof(unknown(Fact), Output):-
  engine:output_known_fact([(Fact :- true)], FactOutput),
  % If the fact is not known, store a default response in the output.
  atomic_list_concat([FactOutput, 'I do not know that is true.'], ' ', Output).
