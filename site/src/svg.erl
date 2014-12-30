-module(svg).
-export([line/2, line/3, box/2, box/3, halfRect/2, halfRect/3, link2/3]).
-export([text/2, text/3, compileSVG/2]).

% line({X1,Y1}, {X2,Y2}, Option) -> Draw a line between (X1,Y1) and (X2,Y2).
% X1,Y1   : Integer, position of the starting point.
% X2,Y2   : Integer, position of the ending point of the line.
% Option  : Tuple, [{optionsName, "option's value"}].
% Exemple : line({0,0}, {10,10},[{stroke, red}, {'stroke-width', 3}]).
line({X1, Y1}, {X2, Y2}) ->
	{path, [{d, "M "
		++erlang:integer_to_list(X1)++" "
		++erlang:integer_to_list(Y1)++" L "
		++erlang:integer_to_list(X2)++" "
		++erlang:integer_to_list(Y2)}], []}.

line({X1,Y1}, {X2,Y2}, []) ->
	line({X1,Y1}, {X2,Y2});

line({X1,Y1}, {X2,Y2}, Option) ->
	{path, [{d, "M "
		++erlang:integer_to_list(X1)++" "
		++erlang:integer_to_list(Y1)++" L "
		++erlang:integer_to_list(X2)++" "
		++erlang:integer_to_list(Y2)}]++Option, []}.


% box({X,Y}, {W,H}, Option) -> Draw a box at the position (X,Y), with the size (W,H).
% X,Y     : Integer, position of the box.
% W,H     : Integer, size of the box.
% Option  : Tuple, [{optionsName, "option's value"}].
% Exemple : box({0,0}, {10,10}, [{fill, blue}, {stroke, red}]).
box({X,Y}, {W,H}) ->
	{rect, [{x, X}, {y, Y}, {width, W}, {height, H}], []}.

box({X,Y}, {W,H}, []) ->
	box({X,Y}, {W,H});
	
box({X,Y}, {W,H}, Option) ->
	{rect, [{x, X}, {y, Y}, {width, W}, {height, H}]++Option, []}.

% halfRect({X,Y}, {A,B}, Option) -> Create half rectangle between (X,Y) and (A,B).
% X,Y     : Integer, Starting point.
% A,B     : Integer, Ending point.
% Option  : Tuple, [{optionName, "option's value"}]
% Exemple : halfRect({0,0}, {10,10}, [{stroke, red}, {'stroke-width', 3}]).
halfRect({X,Y}, {A,B})  ->
	[line({X,Y}, {A,Y}),
	line({A,Y}, {A,B})].
halfRect({X,Y}, {A,B}, Option) ->
	[line({X,Y}, {A,Y}, Option),
	line({A,Y}, {A,B}, Option)].

% link2({X,Y}, {A,B}, Option) -> Create a squared link between (X,Y) and (A,B).
% X,Y     : Integer, Starting point.
% A,B     : Integer, Ending point.
% Option  : Tuple, [{optionName, "option's value"}]
% Exemple : link2({0,0}, {10,10}, [{stroke, red}, {'stroke-width', 3}]).
link2({X,Y}, {A,B}, Option) ->
	Inter = {(X+A) div 2, (Y+B) div 2},
	lists:flatten([halfRect({X,Y}, Inter, Option),
	halfRect({A,B}, Inter, Option)]).

% Draw a text.
% X,Y  : Integer, Position of the text.
% Text : String, The text to draw.
% Option : Tuple, [{optionName, "option's value"}]
% Exemple : svg:text(10,10, "Hello World !", [{'font-family', "Verdana"}, {'font-size', 18}, {fill, blue}]").
text({X,Y}, Text) ->
	{text, [{x, X}, {y, Y}], [Text]}.
text({X,Y}, Text, Option) ->
	{text, [{x, X}, {y, Y}]++Option, [Text]}.

% compileSVG(Object) -> Create a valid SVG file with the specified object in argument.
% Object -> a Array of tuple. Simply said, the best way to do something
% with this is to do something like :
% 	svg:createSVG( [ svg:line(args), svg:box(args, ... ] ).
compileSVG(Objects, {Width, Height}) ->
	Data = {svg, [{xmlns, "http://www.w3.org/2000/svg"}, {version, "1.1"}, {width, Width}, {height, Height}], lists:flatten(Objects)},
	xmerl:export_simple([Data], xmerl_xml).
