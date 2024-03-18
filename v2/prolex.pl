% --- Operators ---

:- op(500, xfy, &).
:- op(510, xfy, =>).
:- op(100, fx, ~).

% --- Command-line interface ---

/**
 * prolex is nondet
 *
 * The prolex/0 predicate starts the control loop of the program. It reads input,
 * determines the corresponding output, prints the output, and recurses.
 */
prolex :-
  write('>> '),
  read_input(Input),
  handle_input(Input, Output),
  print_output(Output),
  prolex.

/**
 * read_input(+Input:str) is nondet
 *
 * The read_input/1 predicate reads the input character by character.
 *
 * @param WordList The list of words in the input.
 */
read_input(WordList) :-
  % Read the first character code of the input.
  get0(CharacterCode),
  % Read the rest of the input.
  read_input(CharacterCode, WordList).

/**
 * read_input(+CharacterCode:int,-WordList:str) is nondet
 *
 * The read_input/2 predicate splits the input into a list of words.
 *
 * @param CharacterCode The next character code.
 * @param WordList The list of words in the input.
 */
% The newline character code ends the input.
read_input(CharacterCodeNewline, []) :-
  newline(CharacterCodeNewline),
  !.
% The space character code is ignored.
read_input(CharacterCodeSpace, WordList) :-
  space(CharacterCodeSpace),
  !,
  get0(CharacterCode),
  read_input(CharacterCode, WordList).
% Otherwise, the character code starts a new word.
read_input(CharacterCode, [Word|WordList]) :-
  read_word(CharacterCode, CharacterCodeList, CharacterCodeNext),
  % Convert between an atom and a list of character codes.
  atom_codes(Word, CharacterCodeList),
  read_input(CharacterCodeNext, WordList).

/**
 * read_word(+CharacterCode:int, -Word:str, -CharacterCodeNext:int) is nondet
 *
 * The read_word/3 predicate reads a word character by character.
 */
% The space character code ends a word.
read_word(CharacterCode, [], CharacterCode) :-
  space(CharacterCode),
  !.
% The newline character code also ends a word.
read_word(CharacterCode, [], CharacterCode) :-
  newline(CharacterCode),
  !.
% Otherwise, the character code is part of the word.
read_word(CharacterCode, [CharacterCode|CharacterCodeList], CharacterCodeLast) :-
  get0(CharacterCodeNext),
  read_word(CharacterCodeNext, CharacterCodeList, CharacterCodeLast).

/**
 * space(+CharacterCode:int) is det
 *
 * The space/1 predicate is true if the given character code is a space.
 *
 * @param CharacterCode The character code.
 */
space(32).

/**
 * newline(+CharacterCode:int) is det
 *
 * The newline/1 predicate is true if the given character code is a newline.
 *
 * @param CharacterCode The character code.
 */
newline(10).

/**
 * handle_input(+Input:str, -Output) is det
 *
 * The handle_input/2 predicate parses the input, determines the corresponding output,
 * and returns it.
 *
 * @param Input The input to process.
 * @param Output The output to return.
 */
handle_input(Input, Output) :-
  % Parse the input.
  grammar:parse(Input, LogicalForm, Type),
  % Transform the first-order logical form of the input into a clausal form.
  clausify:clausify(LogicalForm, ClausalForm, FreeVariables),
  !,
  % Determine the corresponding output.
  output(Type, FreeVariables, ClausalForm, Output).
handle_input(_Input, error("could not parse")).

/**
 * output(+Type:str, +FreeVariables:list, +ClausalForm, -Output) is det
 *
 * The output/4 predicate replies to a given input.
 *
 * @param Type The literal "question" or "assertion".
 * @param FreeVariables The free variables in the input, interpreted existentially.
 * @param ClausalForm The clausal form of the input.
 * @param Output The output to return.
 */
% If the input is an assertion, assert it.
output(type_assertion, _FreeVariables, Assertion, asserted(Assertion)) :-
  assert(Assertion),
  !.
% If the input is a question, output with the answer.
output(type_question, FreeVariables, (answer(Answer) :- Condition), Output) :-
  % Find the answers to the question.
  (
    setof(Answer, FreeVariables^Condition, Answers)
    % If there are answers, they are the output.
    -> Output = answer(Answers)
    ; (
        % Otherwise, the output is "none" or "no".
        Answer = []
        -> Output = answer([none])
        ;  Output = answer([no])
      )
  ),
  !.
% Otherwise, the output is an error.
output(_Type, _FreeVariables, _ClausalForm, error("unknown type")).


/**
 * print_output(+Output) is det
 *
 * The print_output/1 predicate prints the output.
 *
 * @param Output The output to print.
 */
print_output(error(ErrorType)) :-
  write("Error: '"),
  write(ErrorType),
  write("'."),
  nl.
print_output(asserted(Assertion)) :-
  write("Asserted: '"),
  write(Assertion),
  write("'."),
  nl.
print_output(answer(Answers)) :-
  print_answers(Answers).

/**
 * print_answers(+Answers:list) is det
 *
 * The print_answers/1 predicate prints a list of answers.
 *
 * @param Answers The list of answers to print.
 */
% Base case: if there is a single answer, print it.
print_answers([Answer]) :-
  !,
  write(Answer),
  write("."),
  nl.
% Otherwise, print the first answer and recurse.
print_answers([Answer|AnswerList]) :-
  write(Answer),
  write(", "),
  print_answers(AnswerList).

% --- Clausify ---

% First-order logic expression, clausal form, free variables.
% Universals.
clausify(all(X, F0), F, [X|V]) :-
  !,
  clausify(F0, F, V).
% Implications.
clausify((Antecedent1 => Consequent1), (Consequent :- Antecedent), V) :-
  !,
  clausify_literal(Consequent1, Consequent),
  clausify_antecedent(Antecedent1, Antecedent, V).
% Literals.
clausify(Consequent1, Consequent, []) :-
  clausify_literal(Consequent1, Consequent).

% First-order logic expression, clausal form, free variables.
% Literals.
clausify_antecedent(Literal0, Literal, []) :-
  clausify_literal(Literal0, Literal).
% Conjunctions.
clausify_antecedent(A0 & B0, (A, B), V) :- !,
  clausify_antecedent(A0, A, V0),
  clausify_antecedent(B0, B, V1),
  conc(V0, V1, V).
% Existentials.
clausify_antecedent(exists(X, F0), F, [X|V]) :-
  clausify_antecedent(F0, F, V).

% First-order logic literal, clausal form.
clausify_literal(~A, A).

conc([], List, List).
conc([Head|Tail], List, [Head|Rest]) :-
  conc(Tail, List, Rest).

% --- Grammar ---

% An assertion is a finite sentence without gaps.
parse(Sentence, LogicalForm, type_assertion) :-
  sentence(LogicalForm, nogap, Sentence, []).

% A question.
parse(Sentence, LogicalForm, type_question) :-
  question(LogicalForm, Sentence, []).

% Questions

question(Sentence => ~(answer(X))) -->
  dictionary:wh_pronoun,
  verb_phrase(finite, X^Sentence, nogap).
question(Sentence => ~(answer(X))) -->
  dictionary:wh_pronoun,
  inverted_sentence(Sentence, gap(noun_phrase, X)).
question(Sentence => ~(answer(yes))) -->
  inverted_sentence(Sentence, nogap).
question(Sentence => ~(answer(yes))) -->
  [is],
  noun_phrase((X^Sentence0)^Sentence, nogap),
  noun_phrase((X^true)^exists(X, Sentence0 & true), nogap).

% Declarative sentences

sentence(Sentence, GapInfo) -->
  noun_phrase(VerbPhrase^Sentence, nogap),
  verb_phrase(finite, VerbPhrase, GapInfo).

% Inverted sentences

inverted_sentence(Sentence, GapInfo) -->
  dictionary:auxiliary_verb(finite/Form, VerbPhrase1^VerbPhrase2),
  noun_phrase(VerbPhrase2^Sentence, nogap),
  verb_phrase(Form, VerbPhrase1, GapInfo).

% Noun phrases

noun_phrase(NounPhrase, nogap) -->
  dictionary:determiner(Noun2^NounPhrase),
  dictionary:noun(Noun1),
  optional_relative_pronoun(Noun1^Noun2).
noun_phrase(NounPhrase, nogap) -->
  dictionary:proper_noun(NounPhrase).
noun_phrase((X^Sentence)^Sentence, gap(noun_phrase, X)) -->
  [].

% Verb phrases

verb_phrase(Form, X^Sentence, GapInfo) -->
  dictionary:transitive_verb(Form, X^VerbPhrase),
  noun_phrase(VerbPhrase^Sentence, GapInfo).
verb_phrase(Form, VerbPhrase, nogap) -->
  dictionary:intransitive_verb(Form, VerbPhrase).
verb_phrase(Form1, VerbPhrase2, GapInfo) -->
  dictionary:auxiliary_verb(Form1/Form2, VerbPhrase1^VerbPhrase2),
  verb_phrase(Form2, VerbPhrase1, GapInfo).
verb_phrase(Form1, VerbPhrase2, GapInfo) -->
  dictionary:subject_object_raising_verb(Form1/Form2, NounPhrase^VerbPhrase1^VerbPhrase2),
  noun_phrase(NounPhrase, GapInfo),
  verb_phrase(Form2, VerbPhrase1, nogap).
verb_phrase(Form2, VerbPhrase2, GapInfo) -->
  dictionary:subject_object_raising_verb(Form1/Form2, NounPhrase^VerbPhrase1^VerbPhrase2),
  noun_phrase(NounPhrase, nogap),
  verb_phrase(Form1, VerbPhrase1, GapInfo).
verb_phrase(finite, X^Sentence, GapInfo) -->
  [is],
  noun_phrase((X^Sentence1)^exists(X, Sentence & Sentence1), GapInfo).

% Relative clauses

optional_relative_pronoun((X^Sentence1)^(X^(Sentence1 & Sentence2))) -->
  dictionary:relative_pronoun,
  verb_phrase(finite, X^Sentence2, nogap).
optional_relative_pronoun((X^Sentence1)^(X^(Sentence1 & Sentence2))) -->
  dictionary:relative_pronoun,
  sentence(Sentence2, gap(noun_phrase, X)).
optional_relative_pronoun(Noun^Noun) -->
  [].

% --- Dictionary ---

% Preterminals

determiner(LogicalForm) -->
  [Determiner],
  {determiner(Determiner, LogicalForm)}.

noun(LogicalForm) -->
  [Noun],
  {noun(Noun, LogicalForm)}.

proper_noun((X^Sentence)^Sentence) -->
  [ProperNoun],
  {proper_noun(ProperNoun, X)}.

auxiliary_verb(Form, LogicalForm) -->
  [AuxiliaryVerb],
  {auxiliary_verb(AuxiliaryVerb, Form, LogicalForm)}.

relative_pronoun -->
  [RelativePronoun],
  {relative_pronoun(RelativePronoun)}.

wh_pronoun -->
  [WhPronoun],
  {wh_pronoun(WhPronoun)}.

intransitive_verb(nonfinite, LogicalForm) -->
  [IntransitiveVerb],
  {intransitive_verb(IntransitiveVerb, _, _, _, _, LogicalForm)}.
intransitive_verb(finite, LogicalForm) -->
  [IntransitiveVerb],
  {intransitive_verb(_, IntransitiveVerb, _, _, _, LogicalForm)}.
intransitive_verb(finite, LogicalForm) -->
  [IntransitiveVerb],
  {intransitive_verb(_, _, IntransitiveVerb, _, _, LogicalForm)}.
intransitive_verb(past_participle, LogicalForm) -->
  [IntransitiveVerb],
  {intransitive_verb( _, _, _, IntransitiveVerb, _, LogicalForm)}.
intransitive_verb(present_participle, LogicalForm) -->
  [IntransitiveVerb],
  {intransitive_verb( _, _, _, _, IntransitiveVerb, LogicalForm)}.

transitive_verb(nonfinite, LogicalForm) -->
  [TransitiveVerb],
  {transitive_verb(TransitiveVerb, _, _, _, _, LogicalForm)}.
transitive_verb(finite, LogicalForm) -->
  [TransitiveVerb],
  {transitive_verb(_, TransitiveVerb, _, _, _, LogicalForm)}.
transitive_verb(finite, LogicalForm) -->
  [TransitiveVerb],
  {transitive_verb(_, _, TransitiveVerb, _, _, LogicalForm)}.
transitive_verb(past_participle, LogicalForm) -->
  [TransitiveVerb],
  {transitive_verb( _, _, _, TransitiveVerb, _, LogicalForm)}.
transitive_verb(present_participle, LogicalForm) -->
  [TransitiveVerb],
  {transitive_verb( _, _, _, _, TransitiveVerb, LogicalForm)}.

subject_object_raising_verb(nonfinite/Requires, LogicalForm) -->
  [SORVerb],
  {subject_object_raising_verb(SORVerb, _, _, _, _, Requires, LogicalForm)}.
subject_object_raising_verb(finite/Requires, LogicalForm) -->
  [SORVerb],
  {subject_object_raising_verb(_, SORVerb, _, _, _, Requires, LogicalForm)}.
subject_object_raising_verb(finite/Requires, LogicalForm) -->
  [SORVerb],
  {subject_object_raising_verb(_, _, SORVerb, _, _, Requires, LogicalForm)}.
subject_object_raising_verb(past_participle/Requires, LogicalForm) -->
  [SORVerb],
  {subject_object_raising_verb(_, _, _, SORVerb,  _, Requires, LogicalForm)}.
subject_object_raising_verb(present_participle/Requires, LogicalForm) -->
  [SORVerb],
  {subject_object_raising_verb(_, _,  _, _, SORVerb, Requires, LogicalForm)}.

% Lexical items

relative_pronoun(that).
relative_pronoun(who).
relative_pronoun(whom).

wh_pronoun(who).
wh_pronoun(whom).
wh_pronoun(what).

determiner(every, (X^P)^(X^Q)^all(X, P => Q)).
determiner(a, (X^P)^(X^Q)^exists(X, P & Q)).
determiner(an, (X^P)^(X^Q)^exists(X, P & Q)).
determiner(some, (X^P)^(X^Q)^exists(X, P & Q)).

noun(author, X^(~author(X))).
noun(book, X^(~book(X))).
noun(professor, X^(~professor(X))).
noun(program, X^(~program(X))).
noun(programmer, X^(~programmer(X))).
noun(student, X^(~student(X))).

proper_noun(alice, alice).
proper_noun(bob, bob).
proper_noun(charlie, charlie).
proper_noun(prolog, prolog).
proper_noun(principia, principia).

intransitive_verb(halt, halts, halted, halted, halting, X^(~verb_halt(X))).

transitive_verb(write, writes, wrote, written, writing, X^Y^(~verb_write(X, Y))).
transitive_verb(run, runs, ran, run, running, X^Y^(~verb_run(X, Y))).
transitive_verb(like, likes, liked, liked, liking, X^Y^(~verb_like(X, Y))).

subject_object_raising_verb(want, wants, wanted, wanted, wanting, (X^(~verb_want(X, Y, Comp))^S)^(X^Comp)^Y^S, infinitival).

auxiliary_verb(to, infinitival/nonfinite, VerbPhrase^VerbPhrase).
auxiliary_verb(does, finite/nonfinite, VerbPhrase^VerbPhrase).
auxiliary_verb(did, finite/nonfinite, VerbPhrase^VerbPhrase).
