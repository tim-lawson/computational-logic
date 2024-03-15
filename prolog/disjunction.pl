% Our things
pixel(dave).
pixel(pixie).

% Properties
property(colour, [red, green, blue]).

% Knowledge
has_property(dave, colour, [red]).
lacks_property(pixie, colour, [blue]).

% Disjunction
:- discontiguous has_property/3.
has_property(Name, Property, Values):-
    pixel(Name),
    property(Property, Options),
    lacks_property(Name, Property, Negations),
    subtract(Options, Negations, Values).