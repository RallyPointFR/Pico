%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (show).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

% Salut Maël du futur !
% Pense a gérer le cas ou on donne un mauvais ID de tournois !
% Bon courage.

main() -> #template { file="./site/templates/fundation.html" }.

title() ->
	case wf:q(id) of
			undefined ->
				"Wrong ID !";
			A ->
				"Tournois " ++ A
	end.

menu() ->
	menu:fundationMenu().

body() -> 
	case wf:q(id) of
		undefined ->
			#panel{body=[
				#p{text="You must provide a tournament id !"}
				]};
		ID ->
			wf:comet_global(fun() -> update(ID) end, tournament_pool),
			{ok, {_,Type,T}} = tools:unserialize("res/" ++ ID ++ ".pico"),
			{ok, Rep} = interfaceTournoi:representation(Type, T),
			SVG = metierSVG:generateSVG(Rep),
			#panel{id=placeholder, body=[
				#h3{text="Tournois " ++ ID},
				SVG
				]}
	end.
	
update(TID) ->
	receive
		{update, ID} ->
			if TID =:= ID ->
					{ok, {_,Type,T}} = tools:unserialize("res/" ++ ID ++ ".pico"),
					{ok, Rep} = interfaceTournoi:representation(Type, T),
					SVG = metierSVG:generateSVG(Rep),
					wf:update(placeholder, [#h3{text="Tournois " ++ ID}, SVG]);
				true ->
					ok
			end
	end,
	wf:flush(),
	update.
	

