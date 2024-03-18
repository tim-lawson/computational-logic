% TODO: handle contradictions. has_property(Name, Property, Values) and lacks_property(Name, Property, Values) at the same time.

% Probably going overboard, but we can know a few different types of information
% 1. A thing has a property with a specific value e.g. "dave the pixel is red"
% 2. A thing has a property and we know for sure it's one of a subset of values e.g. "pixie the pixel is either red or green"
% 3. A thing has a property and we know it could be one of a subset of values. As Prolog treats everything as true until proven otherwise
%   I will use this as a default. I.e. adding a fact for this does nothing.
% 4. A thing hass a property that definitely isn't a specific value or range of values e.g. "pixie the pixel is not blue" 

% Our things
:- dynamic pixel/1.
:- retractall(pixel(_)).
:- asserta(pixel(dave)).
:- asserta(pixel(pixie)).
:- asserta(pixel(george)).

% Properties. property(PropertyName, Value).
% Properties have to be a keypair format because predicates can't be added at runtime with asserta/1.
% E.g. can't have property(colour(red))

% Can't do this
% :- dynamic property/2.
% :- retractall(property(_, _)).
% :- asserta(property(colour, red)).
% :- asserta(property(colour, green)).
% :- asserta(property(colour, blue)).

% because of some weirdness
% ?- has_property(_, property(colour, Colour)).
% Colour = green ;
% Colour = red.
% ?- property(colour, Colour).
% false.

% Knowledge. has_property(Name, PropertyName, is/is_not, Value).
:- dynamic has_property/4.
:- retractall(has_property(_, _, _, _)).
:- asserta(has_property(dave, colour, is, red)).
:- asserta(has_property(george, colour, is, green)).
:- asserta(has_property(george, colour, is, red)).
:- asserta(has_property(pixie, colour, is_not, blue)).

% Shorthand for accessing properties
% A property name and value are related if
% there is any thing that has that property and its specified the property is or is not that value. 
property(PropertyName, Value):-
    has_property(_, PropertyName, _, Value).
% Contradictions. 
property(Thing, PropertyName, Value):-
    has_property(Thing, PropertyName, is, Value),
    has_property(Thing, PropertyName, is_not, Value),
    !, 
    write("Contradiction: "), write(Thing), tab(1), write(PropertyName), write(" is and is_not "), write(Value),
    fail.
% A thing, property name, and value are related if
% that thing has that property and its specified the property is that value.
property(Thing, PropertyName, Value):-
    has_property(Thing, PropertyName, is, Value).
% A thing, property name, and value are related if
% the property and value are related anywhere, and it's not provable that the property is not the value for the thing in question.
property(Thing, PropertyName, Value):-
    property(PropertyName, Value),
    \+ has_property(Thing, PropertyName, is_not, Value).

% Some more computationally intensive queries that return exhaustive sets of possible values for a property.
% Return a list of possible values for a property by search through all the things we know about
unique_values(Property, UniqueValues):-
    findall(Value,  has_property(_, Property, _, Value), Values),
    list_to_set(Values, UniqueValues).
% Easy case where we know the possible values for a property
unique_values(Thing, Property, UniqueValues):-
    findall(Value,  has_property(Thing, Property, is, Value), Values),
    list_to_set(Values, UniqueValues),
    length(UniqueValues, Int), Int > 0. % Check not empty.      
% Disjunction.
unique_values(Thing ,Property, UniqueValues):-
    unique_values(Property, UniqueKnownOptions),
    findall(Value,  has_property(Thing, Property, is_not, Value), KnownNot),
    list_to_set(KnownNot, UniqueNot),
    subtract(UniqueKnownOptions, UniqueNot, UniqueValues).

% For listing what we know about a thing
describe(Name):-
    pixel(Name),
    write(Name), nl, 
    has_property(Name, Property, Bool, Value),
    tab(2), write(Property), tab(1), write(Bool), tab(1), write(Value), nl, fail.
describe(_).