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

negation(negation(X)) :- X.

% --- Question Answering ---

%% prove_question(+Question:atom, -Output:string)
%
% The prove_question/2 predicate tries to prove a question from the known facts.
% If the question can be proved either way, it outputs the answer.
% Otherwise, it outputs a default response.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question(Question, Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
  ( % Try to prove the question based on the known facts.
    engine:prove_from_known_facts(Question, FactList) ->
      engine:output_answer(Question, Output)
  ; % Try to prove the negation of the question.
    engine:prove_from_known_facts(negation(Question), FactList) ->
      engine:output_answer(negation(Question), Output)
  ; % If the question cannot be proved either way, output a default response.
    Output = 'I do not know that is true.'
  ).

%% prove_question_list(+Question:atom, -Output:string)
%
% The prove_question_list/2 predicate is equivalent to prove_question/2, except it
% outputs the empty string when question cannot be proved either way.
% This predicate is suitable for use with maplist, such as in find_known_facts_noun/2.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question_list(Question, Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
  ( % Try to prove the question based on the known facts.
    engine:prove_from_known_facts(Question, FactList) ->
      engine:output_answer(Question, Output)
  ; % Try to prove the negation of the question.
    engine:prove_from_known_facts(negation(Question), FactList) ->
      engine:output_answer(negation(Question), Output)
  ; % If the question cannot be proved either way, output the empty string.
    Output = ''
  ).

%% prove_question_tree(+Question:atom, -Output:string)
%
% The prove_question_tree/2 predicate is an extended version of prove_question/2 that
% constructs a proof tree.
% If the question can be proved either way, it transforms each step of the proof into a
% sentence and concatenates the sentences into the output.
%
% @param +Question: The question to prove.
% @param -Output: The generated output.
%
prove_question_tree(Question, Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
  ( % Try to prove the question based on the known facts.
    engine:prove_from_known_facts(Question, FactList, [], ProofList) ->
      output_proof_list(Question, ProofList, Output)
  ; % Try to prove the negation of the question.
    engine:prove_from_known_facts(negation(Question), FactList, [], ProofList) ->
      output_proof_list(negation(Question), ProofList, Output)
  ; % If the question cannot be proved, output a default response.
    Output = 'I do not think that is true.'
  ).

% --- Meta-interpreter ---

%% prove_from_known_facts(+Clause:atom, +FactList:list, -ProofList:list, -Proof:atom)
%
% The prove_from_known_facts/4 predicate tries to prove a clause based on a list of facts.
% If the clause can be proved, it stores the proof in the output.
% The proof is a list of steps, where each step is a fact that was used to prove the clause.
%
% @param +Clause: The clause to prove.
% @param +FactList: The list of facts to use.
% @param +ProofList: The accumulator for the proof.
% @param -Proof: The generated proof.
%

% If the clause is true, we are done.
prove_from_known_facts(true, _FactList, ProofList, ProofList) :- !.

% -- Conjunction
prove_from_known_facts((Conjunct1, Conjunct2), FactList, ProofList, Proof) :-
  !,
  % Find a clause of the form 'if Body1 then Conjunct1'.
  utils:find_clause((Conjunct1 :- Body1), Fact, FactList),
  % Concatenate Body1 and Conjunct2 into Body2.
  utils:concatenate_conjunctive(Body1, Conjunct2, Body2),
  % Try to prove Body2. If the proof succeeds, then we have proven (Conjunct1, Conjunct2).
  prove_from_known_facts(Body2, FactList, [proof((Conjunct1, Conjunct2), Fact)|ProofList], Proof).

% -- Default reasoning
prove_from_known_facts(Clause, FactList, ProofList, Proof) :-
  % Find a clause of the form 'if Body then default(Clause)'.
  utils:find_clause((default(Clause) :- Body), Fact, FactList),
  % Try to prove Body. If the proof succeeds, then we have proven default(Clause).
  prove_from_known_facts(Body, FactList, [proof(default(Clause), Fact)|ProofList], Proof).

% -- Implication
prove_from_known_facts(Clause, FactList, ProofList, Proof) :-
  % Find a clause of the form 'if Body then Clause'.
  utils:find_clause((Clause :- Body), Fact, FactList),
  % Try to prove Body. If the proof succeeds, then we have proven Clause.
  prove_from_known_facts(Body, FactList, [proof(Clause, Fact)|ProofList], Proof).

% -- Negation (modus tollens)
prove_from_known_facts(negation(Clause), FactList, ProofList, Proof) :-
  % Find a clause of the form 'if Clause then Body'.
  utils:find_clause((Body :- Clause), Fact, FactList),
  % Try to prove the negation of Body. If the proof succeeds, then we have proven the negation of Clause.
  prove_from_known_facts(negation(Body), FactList, [proof(negation(Clause), Fact)|ProofList], Proof).

% -- Disjunction (positive case)
prove_from_known_facts(Clause, FactList, ProofList, Proof) :-
  ( % Find a disjunctive rule of the form 'if Body then (Clause; Other)'.
    utils:find_clause(((Clause; Other) :- Body), Fact, FactList)
    % Find a disjunctive rule of the form 'if Body then (Other; Clause)'.
  ; utils:find_clause(((Other; Clause) :- Body), Fact, FactList)
  ),
  % Try to prove Body. If the proof succeeds, then we have proven (Clause; Other).
  prove_from_known_facts(Body, FactList, [], A),
  % Try to prove negation(Other). If the proof succeeds, then we have proven Clause.
  prove_from_known_facts(negation(Other), FactList, [], B),
  !,
  % Concatenate the proofs of the Body, negation(Other), and Clause itself.
  append(A, B, C),
  append(C, ProofList, D),
  Proof = [proof(Clause, Fact)|D].

%% prove_from_known_facts(+Clause:atom, +FactList:list)
%
% The prove_from_known_facts/2 predicate also tries to prove a clause, but it does not
% store the proof, i.e. only the answer is needed.
%
% @param +Clause: The clause to prove.
% @param +FactList: The list of facts to use.
%
prove_from_known_facts(Clause, FactList) :-
  prove_from_known_facts(Clause, FactList, [], _Proof).

% --- Commands ---

%% find_known_facts(-Output:string)
%
% The find_known_facts/1 predicate finds all known facts and transforms the sentences
% into a string output.
%
% @param -Output: The known facts or default response.
%
find_known_facts(Output) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactList),
  % Transform each fact into a response.
  maplist(engine:output_known_fact, FactList, OutputList),
  ( % If no facts are known, output a default response.
    OutputList = [] -> Output = 'I do not know anything.'
    % Otherwise, store the concatenated responses in the output.
  ; atomic_list_concat(OutputList, '. ', Output)
  ).

%% find_known_facts_noun(+ProperNoun:atom, -Output:string)
%
% The find_known_facts_noun/2 predicate finds all known facts with a proper-noun argument,
% and transforms the sentences into a string output.
%
% @param +ProperNoun: The proper noun.
% @param -Output: The known facts or default response.
%
find_known_facts_noun(ProperNoun, Output) :-
  findall(
    Question,
    ( % Find all predicates that take a single argument.
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
  ( % If no questions could be proved, output a default response.
    OutputList = [] ->
      atomic_list_concat(['I do not know anything about', ProperNoun], ' ', Output)
  ; % Otherwise, store the concatenated responses in the output.
    otherwise ->
      atomic_list_concat(OutputList, '. ', Output)
  ).

% --- Facts ---

%% is_fact_known(+FactList:list)
%
% The is_fact_known/2 predicate checks if a fact is known.
% If the fact itself is not known, it tries to prove it based on the known facts.
%
% @param +FactList: The list of facts to check.
%
is_fact_known([Fact]) :-
  % Find all known facts.
  findall(Fact, utils:known_fact(Fact), FactListOld),
  % Try to prove the fact based on the known facts.
  utils:try((
      % Try to unify the free variables in the fact with anything else.
      numbervars(Fact, 0, _),
      % Construct a clause from the fact.
      Fact = (Head :- Body),
      % Add the body of the clause to the known facts.
      engine:add_clause_to_facts(Body, FactListOld, FactListNew),
      % Try to prove the head of the clause based on the known facts.
      engine:prove_from_known_facts(Head, FactListNew)
  )).

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

%% output_answer(+Question:atom, -Output:string)
%
% The output_answer/2 predicate transforms a question answer into a string output.
%
% @param +Result: The question.
% @param -Output: The generated output.
%
output_answer(Question, Output) :-
  % Transform the question into a clause.
  utils:transform(Question, Clause),
  % Transform the clause into a sentence.
  phrase(sentence:sentence(Clause), OutputList),
  % Transform the sentence into a string.
  atomics_to_string(OutputList, ' ', Output).

%% output_known_fact(+Fact:atom, -Output:string)
%
% The output_known_fact/2 predicate transforms a known fact into a string output.
%
% @param +Fact: The known fact.
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
output_proof(proof(_Clause, Fact), Output) :-
  engine:output_known_fact(Fact, Output).

% TODO: I don't know if this is used.
output_proof(proof(_Clause, Fact), Output):-
  engine:output_known_fact(Fact, FactOutput),
  % If the fact is not known, output a default response.
  atomic_list_concat([FactOutput, 'I do not know that is true.'], ' ', Output).

%% output_proof_list(+Question:atom, +ProofList:list, -Output:string)
%
% The output_proof_list/2 predicate transforms a question and a proof list into a string output.
%
% @param +Question: The question.
% @param +ProofList: The proof list.
% @param -Output: The generated output.
%
output_proof_list(Question, ProofList, Output) :-
  % Transform each step of the proof into a sentence.
  maplist(engine:output_proof, ProofList, OutputListProof),
  % Transform the question into a clause.
  phrase(sentence:sentence_body([(Question :- true)]), Clause),
  % Transform the clause into a sentence.
  atomic_list_concat([therefore|Clause], ' ', Sentence),
  % Append the sentence to the output.
  append(OutputListProof, [Sentence], OutputList),
  % Transform the sentences into strings.
  atomic_list_concat(OutputList, ', ', Output).