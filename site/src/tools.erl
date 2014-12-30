-module(tools).
-export([durstenfeld/1, createJoueur/2, createEquipe/3, createEquipe/2, createResultat/2, createMatch/1, createMatch/4]).
-export([serialize/2, unserialize/1, getMatchID/1, getMatch/2]).
-export([getEquipeName/1, getAllEquipeName/1, getEquipeName/2, getEquipe/2, getScoreRec/2]).
-export([addEquipe/2, setResult/2, ordonnerResultats/1, setEquipeMatch/2]).
-export([version/0, parPaquets/2, isPowOf/2]).

-include("type.hrl").

% Serialize ( sauvegarde ) des données erlang dans un fichier.
serialize(Filename, Data) ->
	FData = erlang:term_to_binary(Data),
	file:write_file(Filename, FData).

% Unserialize.
unserialize(Filename) ->
    case file:read_file(Filename) of
		{error, Reason} -> {error, Reason};
		{ok, Data} -> {ok, erlang:binary_to_term(Data)}
    end.

% Implémentation de l'algorithme de shuffle de Durstenfeld :
% http://en.wikipedia.org/wiki/Fisher-Yates_shuffle
% Cette méthode à l'avantage d'avoir dans tout les cas
% une complexité O(n), ce qui ne serait pas le cas avec
% une méthode par liste en compréhension.

% Renvoie un nombre aléatoire entre 0..N :
rand(N) ->
        (random:uniform(N+1)-1).

% Echange 2 valeurs dans un array.
switch(P1, P2, Array) ->
        N2 = array:get(P2, Array),
        N1 = array:get(P1, Array),
        array:set(P2, N1, array:set(P1,N2,Array)).

% Durstenfeld's shuffle :
durstenfeld_acc(Array, Result, 0) -> [array:get(0,Array)]++Result;
durstenfeld_acc(Array, Result, Range) ->
        N = rand(Range),
        R = [array:get(N, Array)] ++ Result,
        A = switch(N, Range, Array),
        durstenfeld_acc(A, R, (Range-1)).

durstenfeld(Liste) ->
        durstenfeld_acc(array:from_list(Liste),[],length(Liste)-1).

% Fonctions permettant de créer des Records Joueur, Equipe, Resultat et Match.
% Ces fonctions sont présente afin d'aider lors de l'implementation de tournois a créer des records valide au spécifications.

% Création de record joueur.
% Nom 	: String, nom du joueur.
% Id  	: Integer, Id unique du joueur.
createJoueur(Nom, Id) ->
	#joueur{nom=Nom, id=Id}.

% Création de record Equipe.
% Nom		: Nom de l'equipe.
% Joueur	: Liste de joueur.
% Id		: Integer, id unique de l'equipe.
createEquipe(ListeJoueur, NameBase) ->
    createEquipe(ListeJoueur, NameBase, 0, []).
createEquipe([], _NameBase, _Id,  Acc) -> Acc;
createEquipe([Joueur|ListeJoueur], NameBase, Id, Acc) ->
    createEquipe(ListeJoueur, NameBase, Id+1,
		 Acc++[#equipe{nom=NameBase++integer_to_list(Id),
				joueur=Joueur,
				id=Id}]).
createEquipe(Nom, Joueur, Id) ->
	#equipe{nom=Nom, joueur=Joueur, id=Id}.
% Création de record Résultat.
% {_,_,X}	: Une équipe : on ne récupére que l'id.
% Score		: Integer : score de l'équipe.
createResultat({equipe, _, _, X}, Score) ->
	#resultat{equipe=X, score=Score};

createResultat(Id, Score) ->
    #resultat{equipe=Id, score=Score}.

% Création de record Match.
% Equipes	: Liste d'Equipe : les équipes qui s'affronte.
% Classement	: Liste de Resultat : Résultat du match, par equipe, ordonner en ordre décroissant de classement.
% MatchSucc	: Liste des match suivant ou iront les équipes dans l'ordre du classement.
% id		: Integer : id unique du match.
createMatch(Equipes, Classement, MatchSucc, Id) ->
	#match{equipes=Equipes, classement=Classement, matchSuiv=[X || {match, _, _, _, X} <- MatchSucc], id=Id}.

createMatch(Id) ->
    #match{equipes=[], classement=[], matchSuiv=[], id=Id}.

% Permet de récupérer l'identifiant d'un match
getMatchID([], Acc) ->
	Acc;
getMatchID([{match, _, _, _, ID}|Q], Acc) ->
	getMatchID(Q, Acc++[ID]).
getMatchID(ListeMatch) when is_list(ListeMatch) ->
	getMatchID(ListeMatch, []);
getMatchID({match, _, _, _, ID}) -> ID.

% Permet d'extraire un match d'une liste de matchs à partir de son id
getMatch(Id, {tournoi,Matchs, _Equipes}) -> getMatch(Id, Matchs);
getMatch(_, []) -> throw(match_introuvable);
getMatch(ID, [{match,A,B,C,IDm}|_]) when ID == IDm -> {match,A,B,C,IDm};
getMatch(ID, [_|H]) -> getMatch(ID, H).

% Retourne le nom d'une Equipe.
getEquipeName({equipe, Nom, _, _}) -> Nom.
getEquipeName(_, []) -> null;
getEquipeName(IdSearch, [{equipe, Nom, _, Id}|Equipes]) ->
    if IdSearch =:= Id ->
	    Nom;
       true ->
	    getEquipeName(IdSearch, Equipes)
    end.

% Retroune les noms d'une liste d'Equipe.
getAllEquipeName([], Acc) -> Acc;
getAllEquipeName([{equipe, Nom, _, _}|Q], Acc) ->
	getAllEquipeName(Q,Acc++[Nom]).

getAllEquipeName(ListeEquipe) ->
	getAllEquipeName(ListeEquipe,[]).

% Permet d'extraire une equipe d'une liste d'equipes à partir de son id
getEquipe(Id, {tournoi, _Matchs, Equipes}) -> getEquipe(Id, Equipes);
getEquipe(_, []) -> null;
getEquipe(ID, [{equipe,A,B,IDe}|_]) when ID == IDe -> {equipe,A,B,IDe};
getEquipe(ID, [_|T]) -> getEquipe(ID, T).

% Retourne le scrore d'une équipe dans une liste de résultat.
getScoreRec({equipe, _, _, _}, []) -> null;
getScoreRec(Id, []) when is_integer(Id) -> null;
getScoreRec({equipe, _, _, ID}, [{resultat, X, Score}|_]) when ID==X -> Score;
getScoreRec({equipe, N, J, ID}, [{resultat, X, _}|Q]) when ID=/=X ->
        getScoreRec({equipe, N,J,ID}, Q);
getScoreRec(Id, [{resultat, X, Score}|Resultats]) when is_integer(Id) ->
    if Id == X ->
	    Score;
       true ->
	    getScoreRec(Id, Resultats)
    end.

% Ajoute une équipe si possible dans le match donné
% - Match : le match à mettre à jour
% - Equipe : l'équipe à ajoutter
addEquipe({match, Equipes, Resultats, Suivants, Id}, Equipe)->
    {match, addEquipeRec(Equipe, Equipes), Resultats, Suivants, Id}.

addEquipeRec(Equipe, Equipes)->
    addEquipeRec(Equipe, Equipes, []).

addEquipeRec(_,[],_) -> 
    throw(matchPlein);
addEquipeRec(Equipe, [-1|Equipes], Acc)->
    Acc++[Equipe]++Equipes;
addEquipeRec(Equipe, [E|Equipes], Acc) ->
    addEquipeRec(Equipe, Equipes, Acc++[E]).

% Change les résultats du match donné
% - Match : Le match que l'on veux mettre à jour
% - Resultats : Le tableau de résultats que l'on veux mettre dans le match. Ce tableau doit faire la même taille
% 		que le nombre de joueur et les joueurs identifier dans les résultats doivent corespondre aux joueurs
%		présent dans ce match.
setResult({match, Equipes, _, Suivants, Id}, Resultats) ->
    {match, Equipes, setResultRec(Equipes, Resultats), Suivants, Id}.

setResultRec(Equipes, Resultats) ->
    if length(Equipes) =:= length(Resultats) ->
	    setResultRec(Equipes, Resultats, []);
       length(Equipes) =/= length(Resultats) ->
	    throw(nbResultatsIncorect)
    end.
setResultRec(_, [], Acc) -> Acc;
setResultRec(Equipes, [Resultat|Resultats], Acc) ->
    {resultat, E, _} = Resultat,
    IsCorect = lists:any(fun(Elem) -> Elem =:= E end, Equipes),
    if IsCorect =:= true ->
	    setResultRec(Equipes, Resultats, Acc++[Resultat]);
       IsCorect =:= false ->
	    throw(equipeResultatIncorect)
    end.

% ordonnerResultats(Resultats) ->
%       Ordonne une liste de Resultat par ordre de Score décroissant.
% Resultats : Liste de Resultat à ordonner
%             Un Resultat devrait être de la forme {resultat, Equipe, Score}
ordonnerResultats(Resultats) ->
	lists:sort(
		fun({resultat, EA, ScoreA}, {resultat, EB, ScoreB}) 
		-> {ScoreA, resultat, EA} >= {ScoreB, resultat, EB} end
		,Resultats).

setEquipeMatch({match, _, _, Id}, Equipes) ->
	{match, Equipes, [], Id}.

parPaquets(Length, List) ->
    parPaquets(Length, List, [],[]).
parPaquets(_Length, [], [], Acc) -> Acc;
parPaquets(_Length, [], _Bloc, _Acc) -> throw(invalidNbElements);
parPaquets(Length, [Elem|List], Bloc, Acc) ->
    NewBloc = Bloc++[Elem],
    if length(NewBloc) == Length ->
	    parPaquets(Length, List, [], Acc++[NewBloc]);
       length(NewBloc) < Length ->
	    parPaquets(Length, List, NewBloc, Acc)
    end.

isPowOf(Base, N) ->
    if (N rem Base) =:= 0 ->
	    isPowOf(Base, N div Base);
       N =:= 1 -> true;
       (N rem Base) =/= 0 -> false
    end.

version() ->
    "0.0.1".
