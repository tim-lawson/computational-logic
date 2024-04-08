% TODO: handle contradictions. has_property(Name, Property, Values) and lacks_property(Name, Property, Values) at the same time.

:- use_module(library(clpb)).

% Probably going overboard, but we can know a few different types of information
% 1. A thing has a property with a specific value e.g. "dave the pixel is red"
% 2. A thing has a property and we know for sure it's one of a subset of values e.g. "pixie the pixel is either red or green"
% 3. A thing has a property and we know it could be one of a subset of values. As Prolog treats everything as true until proven otherwise
%   I will use this as a default. I.e. adding a fact for this does nothing.
% 4. A thing hass a property that definitely isn't a specific value or range of values e.g. "pixie the pixel is not blue" 

% Our things
:- dynamic proper_noun/1.
:- retractall(proper_noun(_)).
:- asserta(proper_noun(dave)).
:- asserta(proper_noun(pixie)).
:- asserta(proper_noun(george)).

% Knowledge. has_property(Name, PropertyName, is/is_not, Value).
:- dynamic predicate/3.
:- retractall(predicate(_, _, _)).
:- asserta(predicate(colour/red, dave, true)).
:- asserta(predicate(colour/red, archie, true)).
:- asserta(predicate(colour/green, george, true)).
:- asserta(predicate(colour/blue, pixie, false)).

% Contradictions. Never Ends. Use findall/bagof to fix. 
contradiction():-
    has_property(Property/Value, Thing),
    has_property(Property/Value, Thing).

% A predicate is true of a Thing if the functor exists and it is not known that it doesn't apply to the Thing   
has_property(Property/Value, Thing):-
    predicate(Property/Value, Thing, true). 
has_property(Property/Value, Thing):-
    predicate(Property/Value, _, true),
    \+ predicate(Property/Value, Thing, false).

% Some more computationally intensive queries that return exhaustive sets of possible values for a property.
% Return a list of possible values for a property by search through all the things we know about
unique_values(Property, UniqueValues):-
    findall(Value,  predicate(Property/Value, _, _), Values), % forcing true misses knowlege from counter examples but prevents nonsense
    list_to_set(Values, UniqueValues).
% Easy case where we know the possible values for a property
unique_values(Thing, Property, UniqueValues):-
    findall(Value,  predicate(Property/Value, Thing, true), Values),
    list_to_set(Values, UniqueValues),
    length(UniqueValues, Int), Int > 0. % Check not empty.      
% Disjunction.
unique_values(Thing ,Property, UniqueValues):-
    unique_values(Property, UniqueKnownOptions),
    findall(Value,  predicate(Property/Value, Thing, false), KnownNot),
    list_to_set(KnownNot, UniqueNot),
    subtract(UniqueKnownOptions, UniqueNot, UniqueValues).

% For listing what we know about a thing
describe(Name):-
    proper_noun(Name),
    write(Name), nl, 
    predicate(Property/Value, Name, Bool),
    (Bool -> Text = "is" ; Text = "is not"),
    tab(2), write(Property), tab(1), write(Text), tab(1), write(Value), nl, fail.
describe(_).