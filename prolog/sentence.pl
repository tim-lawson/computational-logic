% Parse sentences.

:- module(
  sentence,
  [
      sentence/3,
      sentence_body/3
  ]
).

:- use_module(grammar).

sentence(Clause) -->
  sentence_word,
  sentence_body(Clause).

sentence_word --> [].
sentence_word --> [that].

sentence_body(Clause) -->
  grammar:determiner(Number, X, Y, Clause),
  grammar:noun(Number, X),
  grammar:verb_phrase(Number, Y).

sentence_body([(Literal :- true)]) -->
  grammar:proper_noun(Noun, X),
  grammar:verb_phrase(Noun, X => Literal).
