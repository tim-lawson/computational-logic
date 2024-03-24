%% debug: Documentation server module.
%
% This module runs the documentation server and shows the comments for the given modules.

% Run the documentation server.
:- doc_server(1234).
:- portray_text(true).

% The modules to show comments for.
:- [cli, command, debug, engine, grammar, question, sentence, utils].