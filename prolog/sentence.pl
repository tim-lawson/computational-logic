%% sentence: Sentence parser.
%
% This module parses sentences and converts them into clauses.
%
:- module(sentence, [sentence/3, sentence_body/3]).

% --- Imports ---

:- use_module(grammar).
:- use_module(utils).

% --- Sentence parser ---

%% sentence(?Sentence:list)//
%
% The sentence//1 DCG rule parses a list of atoms into a sentence.
%
% @param Sentence The list of atoms.
%
sentence(Sentence) -->
  sentence_word,
  sentence_body(Sentence).

sentence_word --> [].
sentence_word --> [that].

%% sentence_body(?Sentence:list)//
%
% The sentence_body//1 DCG rule parses a list of atoms into a sentence body.
%
% @param Sentence The list of atoms.
%
% Note "=>" is used here two allow two atoms to be unified to one in grammar.pl, ususally "ToLiteral"
% This is useful as the two need to be passed through several layers of grammar together
% e.g. verb_phrase->property->adjective->predicate_to_grammar
% It does not perform any special functionality. Any arithmetic operator can be used (+/-; etc) as long
% as grammar:predicate_to_grammar is updated accordingly to destructure it again. 


% Here X is the variable that appears in both the head and body
% E.g. mortal(X) :- human(X). It's created here and passed to both
% So each side refers to the *same* variable.
%  TODO: Can collapse with structure like:
%   sentence_body([(Head / Applicability :- Body)]) -->
%   grammar:determiner(Number, Applicability),
%   grammar:noun(Number, _ => Body),
%   grammar:verb_phrase(Number, _ => Head).

sentence_body([(Head :- Body)]) -->
  grammar:determiner(Number, all),
  grammar:noun(Number, X => Body),
  grammar:verb_phrase(Number, X => Head).

sentence_body([(default(Head) :- Body)]) -->
  grammar:determiner(Number, default),
  grammar:noun(Number, X => Body),
  grammar:verb_phrase(Number, X => Head).

sentence_body([(some(Head) :- Body)]) -->
  grammar:determiner(Number, some),
  grammar:noun(Number, X => Body),
  grammar:verb_phrase(Number, X => Head).

% Here ProperNoun is bound to a value by proper_noun
% It is passed to verb_phrase so it can be used as the atom of Head
% E.g. human(alice) :- true. 
sentence_body([Head :- true]) -->
  grammar:proper_noun(Number, ProperNoun),
  grammar:verb_phrase(Number, ProperNoun => Head).


