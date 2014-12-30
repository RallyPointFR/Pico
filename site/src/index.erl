%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/fundation.html" }.

title() -> "Bienvenue Sur Pico !".

menu() ->
	menu:fundationMenu().

body() ->
    content().
		
content() ->
	DB = saveManager:list("res/"),
	if is_tuple(DB) ->
			#panel{body=[
				#p{text="Bienvenue sur Pico !"},
				#p{text="Aucun tournois n'est en cours."}
			]};
		true ->
			[
				#panel{body=[
					#p{text="Bienvenue sur Pico !"},
					#p{text="Tournois en cours :"}
				]}
			] ++ genList(DB)
	end.

genList([], Acc) -> 
	Acc;
genList([{Nom, _, _}|Q], Acc) ->
	genList(Q, Acc++[#panel{body=[#link{text=Nom, url="/show?id="++Nom}]}]).
genList(DB) ->
	genList(DB, []).
