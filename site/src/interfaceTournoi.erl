-module(interfaceTournoi).
%-compile(export_all).
-export([createTournoi/2, representation/3, representation/2, matchJouables/2, resultat/4, isCorectNbTeam/2]).
-include("type.hrl").

% Fonction constante qui renvoie les textes des erreurs
errorTooManyTeams() ->
    "Trop d'équipes !".
errorNotATeamList() ->
    "La liste d'équipe entrée est invalide ! (au moins une équipe n'est pas une équipes valide)".
errorNotAType(TypeTournoi) ->
    "Paramètre incorect ! '"++ atom_to_list(TypeTournoi)++"' n'est pas un type de tournois ou "++
	"n'implémente pas cette fonctionalité".
errorNbEquipeInvalid() ->
    "Nombre d'équipe invalid ! Reporter vous au règles du tournoi pour plus d'information.".
errorNotATournoi() ->
    "Paramètre incorect ! Le tournoi est invalid.".
errorNotAResult() ->
    "Paramètre incorect ! Le résultat est invavild.".
errorInvalidId() ->
    "Paramètre incorect ! L'identifiant est invalid.".
errorNotAnInteger() ->
    "Paramètre incorect ! Ce n'est pas un entier.".


% Création d'un tournoi avec la liste Equipes contenant les équipes qui doivent s'affronter.
createTournoi(TypeTournoi, Equipes) ->
    case erlang:function_exported(TypeTournoi, createTournoi, 1) of
	true -> 
	    case lists:all((fun(X) -> is_record(X, equipe) end), Equipes) of
		true -> 
		    try
			{ok, TypeTournoi:createTournoi(Equipes)}
		    catch
			tropDEquipes -> {fail, errorTooManyTeams()};
			nbEquipeInvalid-> {fail, errorNbEquipeInvalid()}
		    end;
		false ->
		    {fail, errorNotATeamList()}
	    end;
	false ->
	    {fail, errorNotAType(TypeTournoi)}
    end.

% Récupération svg du tournoi.
representation(TypeTournoi, Tournoi, Filename) ->
    case erlang:function_exported(TypeTournoi, representation, 1) of
	true ->
	    case is_record(Tournoi, tournoi) of
		true  -> {ok, TypeTournoi:representation(Tournoi, Filename)};
	        false -> {fail, errorNotATournoi()}
	    end;
	false ->
	    {fail, errorNotAType(TypeTournoi)}
    end.
    
representation(TypeTournoi, Tournoi) ->
    case erlang:function_exported(TypeTournoi, representation, 1) of
	true ->
	    case is_record(Tournoi, tournoi) of
		true  -> {ok, TypeTournoi:representation(Tournoi)};
	        false -> {fail, errorNotATournoi()}
	    end;
	false ->
	    {fail, errorNotAType(TypeTournoi)}
    end.

% Renvoie la liste des matchs actuellement jouables (Ayant toutes les équipes participante remplis).
matchJouables(TypeTournoi, Tournoi) ->
    case erlang:function_exported(TypeTournoi, matchJouables, 1) of
	true  ->
	    case is_record(Tournoi, tournoi) of
		true  -> {ok, TypeTournoi:matchJouables(Tournoi)};
		false -> {fail, errorNotATournoi()}
	    end;
	false ->
	    {fail, errorNotAType(TypeTournoi)}
    end.

% Permet de définir le résultat d'un match.
resultat(TypeTournoi, Tournoi, IdMatch, Resultats) ->
    case erlang:function_exported(TypeTournoi, resultat, 3) of
	true -> 
	    case is_integer(IdMatch) of
		true -> 
		    case is_record(Tournoi, tournoi) of
			true -> 
			    case lists:all((fun(X)-> is_record(X,resultat)end), Resultats) of
				true  -> 
				    {ok, TypeTournoi:resultat(Tournoi, IdMatch, Resultats)};
				false -> 
				    {fail, errorNotAResult()}
			    end;
			false -> 
			    {fail, errorNotATournoi()}
		    end;
		false -> 
		    {fail, errorInvalidId()}
	    end;
	false -> 
	    {fail, errorNotAType(TypeTournoi)}
    end.

isCorectNbTeam(TypeTournoi, NbEquipe) ->
    case erlang:function_exported(TypeTournoi, isCorectNbTeam, 1) of
	false -> {fail, errorNotAType(TypeTournoi)};
	true  ->
	    case is_integer(NbEquipe) of
		false -> {fail, errorNotAnInteger()};
		true  -> {ok, TypeTournoi:isCorectNbTeam(NbEquipe)}
	    end
    end.
