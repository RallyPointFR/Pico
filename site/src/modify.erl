%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (modify).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("type.hrl").

main() ->
	case wf:role(admin) of
		true ->
			#template { file="./site/templates/fundation.html" };
		false ->
			wf:redirect_to_login("/login")
	end.

title() -> "Update Tournament".

menu() ->
	menu:fundationMenu().


body() -> 
    case wf:q(tid) of
		undefined ->
			DB = saveManager:list("res/"),
			if is_tuple(DB) ->
					#p{text="Pas de tournoi en cours."};
				true ->
					genList(DB)
			end;
		TID ->
			case wf:q(mid) of
				undefined ->
					{ok, {_, Type, T}} = saveManager:load(TID, "res/"),
					{ok, ListeMatch} = interfaceTournoi:matchJouables(Type, T),
					Matchs = genIds(ListeMatch, TID, T),
					{ok, Rep} = interfaceTournoi:representation(Type, T),
					SVG = metierSVG:generateSVG(Rep),
					[Matchs, SVG];
				MID ->
					case wf:q(nbeq) of
						undefined ->
							{ok, {_, _, T}} = saveManager:load(TID, "res/"),
							{match, Equipes, _, _, _} = tools:getMatch(list_to_integer(MID), T),
							"<form action=\"/modify?tid=" ++ TID ++ "&mid=" ++ MID ++ "\" method=\"post\">"
							++ "<input type=\"hidden\" name=\"nbeq\" value=\"" ++ bitstring_to_list(wf:pickle(Equipes)) ++ "\">"
							++ generateForm(MID,T)
							++ "<input type=\"submit\" /></form>";
						Equipes ->
							ListeEquipe = wf:depickle(list_to_bitstring(Equipes)),
							Resultat = createResult(ListeEquipe, []),
							{ok, {_, Type, T}} = saveManager:load(TID, "res/"),
							{ok, Dept} = interfaceTournoi:resultat(Type, T, list_to_integer(MID), Resultat),
							ok = saveManager:save(TID, Type, Dept, "res/"),
							wf:send_global(tournament_pool, {update, TID}),
							wf:redirect("/show?id="++TID)
					end
			end
	end.
    
createResult([], Res) ->
	Res;
createResult([T|Q], Res) ->
	Score = wf:q(list_to_atom(integer_to_list(T))),
	createResult(Q, Res++[#resultat{equipe=T, score=list_to_integer(Score)}]).
	
genList([], Acc) -> 
	Acc;
genList([{Nom, _, _}|Q], Acc) ->
	genList(Q, Acc++[#panel{body=[#link{text=Nom, url="/modify?tid="++Nom}]}]).
genList(DB) ->
	genList(DB, []).

genIds([], _, _, Acc) ->
	Acc;
genIds([{match, Equipes, _,_, ID}|Q], Nom, Tournoi, Acc) ->
	Eq = listeEqName(Equipes, Tournoi),
	T = erlang:integer_to_list(ID),
	genIds(Q, Nom, Tournoi, Acc++["<p><a href='/modify?tid="++Nom++"&mid="++T++"'>Match n°"++T++" "++vsEq(Eq)++"</a></p>"]).
genIds(Match, Name, Tournoi) ->
	genIds(Match, Name, Tournoi, ["<p>Liste des matchs modifiables :</p>"]).

vsEq([Q], Acc) ->
	Acc ++ [Q];
vsEq([T|Q], Acc) ->
	vsEq(Q, Acc++[T++" vs. "]).
vsEq(Names) ->
	vsEq(Names, []).

listeEqName([], _, Acc) ->
	Acc;
listeEqName([ID|Q], T, Acc) ->
	{equipe, Nom, _, _} = tools:getEquipe(ID,T),
	listeEqName(Q, T, Acc++[Nom]).
listeEqName(IdEq, T) ->
	listeEqName(IdEq, T, []).

%createResult([], Acc) -> Acc;
%createResult([T|Q], Acc) ->
%	{Bid,Bsc} = T,
%	{Id , Sc} = {bitstr_to_int(Bid),bitstr_to_int(Bsc)},
%	Res = #resultat{equipe=Id, score=Sc},
%	createResult(Q, Acc++[Res]).
%	
%createResult(Res) ->
%	createResult(Res, []).
%
genEquipe([], _, Acc) -> Acc;
genEquipe([Id|Q], T, Acc) ->
	{equipe, Nom, _, _} = tools:getEquipe(Id,T),
	genEquipe(Q, T, Acc++"<p><label>"++Nom++" : </label><input type=\"number\" name=\""++erlang:integer_to_list(Id)++"\"/>").

generateForm(MatchId, Tournoi) ->
	{match, Equipes, _, _, _} = tools:getMatch(list_to_integer(MatchId), Tournoi),
	genEquipe(Equipes,Tournoi,["<p>Entrez les scores :</p>"]).
	

