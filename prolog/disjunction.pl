% Need some way to specfiy a pixel without a specific colour
% Also need a way to say "not a colour"

% pixel(Name, [Colours], [NotColours])
pixel(dave, [red], []).
pixel(pixie, [], [blue]).

% Define the possible options for colour. 
colours([red, green, blue]).

what_colour_is(Name, Colour):- pixel(Name, Colour, _), length(Colour, Int), Int > 0, !.
what_colour_is(Name, Colour):- pixel(Name, _, NotColours), colours(Options), subtract(Options, NotColours, Colour).

