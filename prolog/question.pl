% Parse questions.

:- module(question, [question/3]).

:- use_module(grammar).

question(QuestionBody) -->
  question_word,
  question_body(QuestionBody).

question_word --> [].

question_body(QuestionBody) -->
  [who],
  grammar:verb_phrase(singular, _ => QuestionBody).

question_body(QuestionBody) -->
  [is],
  grammar:proper_noun(Noun, X),
  grammar:property(Noun, X => QuestionBody).

question_body(QuestionBody) -->
  [does],
  grammar:proper_noun(_, X),
  grammar:verb_phrase(_, X => QuestionBody).
