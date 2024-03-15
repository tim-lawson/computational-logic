% Our things
:- dynamic pixel/1.
:- retractall(pixel(_)).
:- asserta(pixel(dave)).
:- asserta(pixel(pixie)).
:- asserta(pixel(george)).

% Properties. property(PropertyName, Options).
:- dynamic property/2.
:- retractall(property(_, _)).
:- asserta(property(colour, [red, green, blue])).

% Knowledge. has_property(Name, PropertyName, PossibleValues).
:- dynamic has_property/3.
:- retractall(has_property(_, _, _)).
:- asserta(has_property(dave, colour, [red])).
:- dynamic lacks_property/3.
:- retractall(lacks_property(_, _, _)).
:- asserta(lacks_property(pixie, colour, [blue])).

% Disjunction
% :- discontiguous has_property/3.
has_property(Name, Property, Values):-
    pixel(Name),
    property(Property, Options),
    lacks_property(Name, Property, Negations),
    subtract(Options, Negations, Values).

% Functions to add knowledge easily
% TODO: handle the case where the property already exists. 

% Add knowledge that a thing has a property with certain values
property_is(Name, Values):-
    % Check if a thing exists
    pixel(Name),
    % Look through all properties to find one with values that match
    property(Property, Options),
    subset(Values, Options),
    % Add the knowledge that the thing has the property values
    asserta(has_property(Name, Property, Values)).

% Add knowledge that a thing has a property that is not certain values
property_is_not(Name, Values):-
    % Check if a thing exists
    pixel(Name),
    % Look through all properties to find one with values that match
    property(Property, Options),
    subset(Values, Options),
    % Add the knowledge that the thing lacks values of the property
    asserta(lacks_property(Name, Property, Values)).