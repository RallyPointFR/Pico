-module(metierSVG).
-export([matchBox/3, getDim/2, getOriginMatchBox/1, linkMatch/2, getTotalDim/2, createSVG/2, createStyledSVG/3, generateSVG/1]).

% textBox({X,Y},{W,H},Text,Options) -> 
%       Draws a box with text inside !
%       The box doesn't fit the text inside.
% {X,Y}     : Coordinates, X and Y are Integers. Position of the box.
% {W,H}     : Dimensions of the box, W and H are Integers.
% Text      : String, text to display in the box.
% Options   : List of tuples 
textBox({X,Y},{W,H},Text) ->
        [svg:box({X,Y},{W,H},
                [{fill,white},
                {stroke,black}])] ++
	    [svg:text({X+5,Y+H-5}, Text,
                [{'font-family',helvetica},
                {'font-size',12}]
        )].

nameBox(Coord,Text) -> textBox(Coord,{100,18},Text).

scoreBox(Coord,null) -> textBox(Coord,{25,18},"");
scoreBox(Coord,Text) -> textBox(Coord,{25,18},erlang:integer_to_list(Text)).

% matchBox({X,Y}, Match, [Equipes]) ->
%       Creates a box in which a match is displayed.
%       It includes all the teams involved and their respective scores
% {X,Y}     : Coordinates, X and Y are Integers. Position of the box from the upper left corner.
% Match     : The match to be displayed.
%           It should look like this : {match, Equipes, Classement, Suivants, Id}
% Equipes   : The list of the teams involved in the match.
matchBoxRec(_, {match, [], _, _, _}, _, Acc, _) -> Acc;
matchBoxRec({X,Y}, {match, [TE|Equipe], Classement, A, B}, Equipes, Acc, Offset) ->
        matchBoxRec({X,Y}, {match, Equipe, Classement, A, B}, Equipes,
		Acc++[nameBox({X,Y+Offset}, tools:getEquipeName(TE, Equipes)),
        	scoreBox({X+100,Y+Offset}, tools:getScoreRec(TE, Classement))], Offset+18).

matchBox(Coord, Match, Equipes) -> matchBoxRec(Coord, Match, Equipes, [], 0).

% Get all the rectangle in a SVG ( tuple list in fact ).
getRectRec([], Acc) -> Acc;
getRectRec([{rect, X, Y}|Q], Acc) -> getRectRec(Q, Acc++[{rect, X, Y}]);
getRectRec([_|Q], Acc) -> getRectRec(Q, Acc).
getRect(Objects) -> getRectRec(lists:flatten(Objects), []).

% Get the value associated to a key in a list of 2 dimensionnal tuple ( [{key1, value1},{key2,value2}] ).
getAtupleValueRec(_, []) -> throw(no_value);
getAtupleValueRec(Key, [{Cle, Value}|_]) when Cle =:= Key ->
	Value;
getAtupleValueRec(Key, [_|Q]) ->
	getAtupleValueRec(Key, Q).

% Get all the rect align on the same X position.
getXalignRec([], _, Acc) -> Acc;
getXalignRec([{rect, List, Opt}|Q], X, Acc) ->
	A = getAtupleValueRec(x, List),
	if A =:= X -> 
		getXalignRec(Q, X, Acc++[{rect, List, Opt}]);
		true -> getXalignRec(Q,X,Acc)
	end.
getXalign(Rekt, X) -> getXalignRec(Rekt, X, []).

% Get all the rect align on the same Y position.
getYalignRec([], _, Acc) -> Acc;
getYalignRec([{rect, List, Opt}|Q], X, Acc) ->
	A = getAtupleValueRec(y, List),
	if A =:= X -> 
		getYalignRec(Q, X, Acc++[{rect, List, Opt}]);
		true -> getYalignRec(Q,X,Acc)
	end.
getYalign(Rekt, Y) -> getYalignRec(Rekt, Y, []).

% Check if the rect are aligned on Y.
isYalignedRec([], _) -> true;
isYalignedRec([{rect, List, _}|Q], OrigY) ->
	Y = getAtupleValueRec(y, List),
	if Y =:= OrigY ->
		isYalignedRec(Q, OrigY);
		true -> false
	end.
isYaligned([]) ->
	throw(empty_list);
isYaligned([{rect,List,_}|Rekt]) ->
	Orig = getAtupleValueRec(y,List),
	isYalignedRec(Rekt, Orig).
	
% Check id the rect are aligned on X.
isXalignedRec([], _) -> true;
isXalignedRec([{rect, List, _}|Q], OrigX) ->
	X = getAtupleValueRec(x, List),
	if X =:= OrigX ->
		isYalignedRec(Q, OrigX);
		true -> false
	end.
isXaligned([]) ->
	throw(empty_list);
isXaligned([{rect,List,_}|Rekt]) ->
	Orig = getAtupleValueRec(x,List),
	isXalignedRec(Rekt, Orig).

% Get the max width of a list of rect.
maxWRec([], Width) -> Width;
maxWRec([{rect,List,_}|Q], PrecW) -> 
	Width = getAtupleValueRec(width,List),
	if Width > PrecW ->
		maxWRec(Q,Width);
		true -> maxWRec(Q, PrecW)
	end.

maxW([]) ->
	throw(empty_list);
maxW([{rect,List,_}|Q]) ->
	Width = getAtupleValueRec(width,List),
	maxWRec(Q,Width).

% get the max height of a list of rect.
maxHRec([], Height) -> Height;
maxHRec([{rect,List,_}|Q], PrecH) -> 
	Height = getAtupleValueRec(height,List),
	if Height > PrecH ->
		maxHRec(Q,Height);
		true -> maxHRec(Q, PrecH)
	end.

maxH([]) ->
	throw(empty_list);
maxH([{rect,List,_}|Q]) ->
	Height = getAtupleValueRec(height,List),
	maxHRec(Q,Height).

% Get the total width of a list of rect.
dimWRec([], Acc) -> Acc;
dimWRec([{rect, List, _}|Q], Acc) ->
	W = getAtupleValueRec(width, List),
	dimWRec(Q, Acc+W).
dimW(Rekt) ->
	Aligned = isXaligned(Rekt),
	if Aligned =:= true ->
		maxW(Rekt);
		true -> dimWRec(Rekt, 0)
	end.

% Get the total height of a list of rect.
dimHRec([], Acc) -> Acc;
dimHRec([{rect, List, _}|Q], Acc) ->
	H = getAtupleValueRec(height, List),
	dimHRec(Q, Acc+H).

dimH(Rekt) ->
	Aligned = isYaligned(Rekt),
	if Aligned =:= true ->
		maxH(Rekt);
		true -> dimHRec(Rekt, 0)
	end.

% Get the total dimension (height & width) of a list of rect.
dim(RectList) -> {dimW(RectList),dimH(RectList)}.

% Sort the rect by their X position. return a double nested array.
sortRectByXRec([], Acc) -> Acc;
sortRectByXRec([{rect, List, Data}|Rekt], Acc) ->
	X = getAtupleValueRec(x, List),
	Array = getXalign(Rekt, X)++[{rect,List,Data}],
	sortRectByXRec(Rekt--Array, Acc++[Array]).

sortRectByX(Rekt) ->
	sortRectByXRec(Rekt, []).

% Sort the rect by their Y position. return a double nested array.
sortRectByYRec([], Acc) -> Acc;
sortRectByYRec([{rect, List, Data}|Rekt], Acc) ->
	Y = getAtupleValueRec(y, List),
	Array = getYalign(Rekt, Y)++[{rect,List,Data}],
	sortRectByYRec(Rekt--Array, Acc++[Array]).

sortRectByY(Rekt) ->
	sortRectByYRec(Rekt, []).

% X offset between two rect.
offsetX({rect, List1, _}, {rect, List2, _}) ->
	X1 = getAtupleValueRec(x, List1),
	W1 = getAtupleValueRec(width, List1),
	X2 = getAtupleValueRec(x, List2),
	W2 = getAtupleValueRec(width, List2),
	if 	X2 > X1 	-> (X2 - (X1+W1))
	; 	X2 < X1 	-> (X1 - (X2+W2))
	; 	X1 =:= X2 	-> 0
	end.

% Y offset between two rect.
offsetY({rect, List1, _}, {rect, List2, _}) ->
	Y1 = getAtupleValueRec(y, List1),
	H1 = getAtupleValueRec(height, List1),
	Y2 = getAtupleValueRec(y, List2),
	H2 = getAtupleValueRec(height, List2),
	if 	Y2 > Y1 	-> (Y2 - (Y1+H1))
	; 	Y2 < Y1 	-> (Y1 - (Y2+H2))
	; 	Y1 =:= Y2 	-> 0
	end.

% Donne les dimensions totale d'une liste de rect issue d'une matchbox en fonction de son orientation.
matchBoxDimRec([], _, Acc) -> Acc;
matchBoxDimRec([T|[]], Orientation, Acc) when Orientation =:= horizontal ->
	{W1, H1} = dim(T),
	{_ , HO} = Acc,
	{W1, H1+HO};

matchBoxDimRec([T|[]], Orientation, Acc) when Orientation =:= vertical ->
	{W1, H1} = dim(T),
	{WO,  _} = Acc,
	{W1+WO, H1};

matchBoxDimRec([T1|[T2|Q]], Orientation, Acc) when Orientation =:= horizontal ->
	Offset = offsetY(hd(T1), hd(T2)),
	{W1, H1} = dim(T1),
	{_ , H2} = dim(T2),
	{_ , HO} = Acc,
	matchBoxDimRec(Q, Orientation, {W1, H1+H2+HO-Offset});

matchBoxDimRec([T1|[T2|Q]], Orientation, Acc) when Orientation =:= vertical ->
	Offset = offsetX(hd(T1), hd(T2)),
	{W1, H1} = dim(T1),
	{W2,  _} = dim(T2),
	{WO,  _} = Acc,
	matchBoxDimRec(Q, Orientation, {W1+W2+WO-Offset, H1}).


% get the dimension of a match box in function of her orientation.
getDim(MatchBox, Orientation) when Orientation =:= horizontal ->
	Rekt = getRect(MatchBox),
	Sorted = sortRectByY(Rekt),
	matchBoxDimRec(Sorted, Orientation, {0,0});
getDim(MatchBox, Orientation) when Orientation =:= vertical ->
	Rekt = getRect(MatchBox),
	Sorted = sortRectByX(Rekt),
	matchBoxDimRec(Sorted, Orientation, {0,0}).

% Get the origin of a matchbox. It is supposed that the origin is the nearest point to {0,0}.
% So we get all the couple {x,y} in the objects of a matchbox and return the smallest one.
getOriginObj({_, List, _}) ->
	X = getAtupleValueRec(x, List),
	Y = getAtupleValueRec(y, List),
	{X,Y}.
getDist({X,Y}) ->
	math:sqrt(X*X+Y*Y).

getOriginMatchBoxRec([], Min) -> Min;
getOriginMatchBoxRec([T|Q], Min) ->
	OrigT = getOriginObj(T),
	DistT = getDist(OrigT),
	DistMin = getDist(Min),
	if DistT < DistMin -> 
		getOriginMatchBoxRec(Q,OrigT)
	;	true -> getOriginMatchBoxRec(Q,Min)
	end.

getOriginMatchBox(Matchbox) ->
	A = lists:flatten(Matchbox),
	[T|Q] = A,
	Min = getOriginObj(T),
	getOriginMatchBoxRec(Q, Min).

% Créer un lien entre 2 matchs.
linkMatch(MatchBox1, MatchBox2) ->
	{W1,H1} = getDim(MatchBox1, horizontal),
	{W2,H2} = getDim(MatchBox2, horizontal),
	{XO1,YO1} = getOriginMatchBox(MatchBox1),
	{XO2,YO2} = getOriginMatchBox(MatchBox2),
	svg:link2({(W1 div 2)+XO1, (H1 div 2)+YO1}, {(W2 div 2)+XO2, (H2 div 2)+YO2}, 
		[{stroke, black}, {'stroke-width', 1}]).

% Donne la taille totale d'une liste d'objets.
getTotalDim([], Acc) -> Acc;
getTotalDim([{_, Liste, _}|Q], {Width, Height}) ->
	WidthCalc 	= getAtupleValueRec(x, Liste) + getAtupleValueRec(width, Liste),
	HeightCalc 	= getAtupleValueRec(y, Liste) + getAtupleValueRec(height, Liste),
	if 	WidthCalc 	> 	Width, HeightCalc	> 	Height -> getTotalDim(Q, {WidthCalc,HeightCalc});
		WidthCalc 	> 	Width, HeightCalc	=<	Height -> getTotalDim(Q, {WidthCalc,Height});
		WidthCalc 	=< 	Width, HeightCalc	> 	Height -> getTotalDim(Q, {Width,HeightCalc});
		WidthCalc 	=< 	Width, HeightCalc	=<	Height -> getTotalDim(Q, {Width,Height})
	end.

% 
generateSVG(Objects) ->
	svg:compileSVG(Objects, getTotalDim(getRect(Objects), {0,0})).

% createSVG(FileName, Objects) -> Create a file containing the svg code for the given objects.
createSVG(FileName, Objects) ->
        file:write_file(FileName, svg:compileSVG(Objects, getTotalDim(getRect(Objects), {0,0}))).
	
createStyledSVG(Filename, Stylesheet, Objects) ->
	{ok, Str} = file:read_file(Stylesheet),
	Style = [{style, [{type, 'text/css'}], [erlang:bitstring_to_list(Str)]}],
	createSVG(Filename, Style++Objects).
