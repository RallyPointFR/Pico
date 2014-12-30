-module(representationTournoiSimple).
-export([representation/1]).

%Renvoie la liste des éléments SVG représentant le tournois donné.
% - Tournois : Le tournois que l'on veut représenter
representation({tournoi, Matchs, Equipes}) ->
    MatchBoxs = repRec(tri(Matchs), Equipes, [], null),
    cleanUp(MatchBoxs)++link(Matchs, MatchBoxs).

repRec([], _, Acc, _) ->
    Acc;
repRec([Match|Matchs], Equipes, [], _) ->
    {match,_,_,_,Id} = Match,
    MatchBox = metierSVG:matchBox({10,10}, Match, Equipes),
    repRec(Matchs, Equipes, [{Id,MatchBox}], {Id,MatchBox});
repRec([Match|Matchs], Equipes, Acc, {IdPrecedent,Precedent}) ->
    {Largeur, Hauteur} = metierSVG:getDim(Precedent, horizontal),
    {X,Y} = metierSVG:getOriginMatchBox(Precedent),
    {match,_,_,_,Id} = Match,
    if Id div 100 =:= 0 ->
			MatchBox = metierSVG:matchBox({X, Y + Hauteur + 20}, Match, Equipes),
			repRec(Matchs, Equipes, Acc ++ [{Id,MatchBox}], {Id,MatchBox});
       (Id div 100) =/= (IdPrecedent div 100) ->
			MatchBox = metierSVG:matchBox({X + Largeur + 200, Y + decallage(Id, Hauteur)}, Match, Equipes),
			repRec(Matchs, Equipes, Acc ++ [{Id,MatchBox}], {Id,MatchBox});
       (Id div 100 =/= 0) and ((Id div 100) =:= (IdPrecedent div 100)) ->
			MatchBox = metierSVG:matchBox({X, Y + espacement(Id,Hauteur)}, Match, Equipes),
			repRec(Matchs, Equipes, Acc ++ [{Id,MatchBox}], {Id,MatchBox})
    end.

%Eleve un nombre à une puissance.
% - Base : le nombre à élever
% - Puissance : La puissance à laquelle élever
exp(_,0) -> 1;
exp(Base, Puissance) ->
    exp(Base, Puissance-1) * Base.

%Calcul l'espacement vertical entre les origines des MatchBoxs
% - Id : L'ID du match représenté par la MatchBox
% - Hauteur : La hauteur d'une MatchBox
espacement(0,_) -> 0;
espacement(Id,Hauteur) ->
    exp(-1,Id div 100)*exp(2,Id div 100)*(Hauteur + 20).

%Caclul le décallage vertical de la première MatchBox d'un niveau de l'arbre par rapport
% à la dernière MatchBox du niveau précédent.
% - Id : L'ID du match représenté par la MatchBox
% - Hauteur : La hauteur des MatchBoxs
decallage(0,_) -> 0;
decallage(Id, Hauteur) ->
    exp(-1,Id div 100)*exp(2,(Id div 100)-1)*(10 + (Hauteur div 2)).

% Renvoie la liste des éléments SVG permettant de lié les MatchBoxs entre elles.
% - Matchs : Les matchs du tournois
% - MatchBoxs : Les MatchBoxs qui ont été générer pour ce tournois.
link(Matchs, MatchBoxs) ->
    linkRec(Matchs, MatchBoxs, []).

linkRec([_],_,Acc) -> Acc;
linkRec([{match,_,_,Suivants, Id}|Matchs], MatchBoxs, Acc) ->
    linkRec(Matchs, MatchBoxs, linkSuiv(Suivants, Id, MatchBoxs)++Acc).

% Lie la MatchBox représentant un match à toutes les MatchBoxs corespondant au successeurs
% de ce match.
% - Suivants : La liste des ID des matchs à lier
% - Source : L'ID du match que l'On est en train de traiter
% - MatchBoxs : La liste de toutes les MatchBoxs générée pour ce tournois
linkSuiv(Suivants, Source, MatchBoxs) ->
    linkSuivRec(Suivants, Source, MatchBoxs, []).

linkSuivRec([],_,_,Acc) -> Acc;
linkSuivRec([M|Suivants], Source, MatchBoxs, Acc) ->
    linkSuivRec(Suivants, Source, MatchBoxs, 
		[metierSVG:linkMatch(get(Source, MatchBoxs), get(M,MatchBoxs))|Acc]).

% Renvoie la MatchBox représentant un match celon son ID.
% - Id : L'ID du match dont on cherche la MatchBox
% - MatchBoxs : La liste de toutes les MAtchsBoxs générée pour ce tournois
get(_,[]) -> throw(matchboxIntrouvable);
get(Id, [{Id,MatchBox}|_]) -> MatchBox;
get(Id, [_|List]) -> get(Id, List).

% Permet de nettoyé la liste de Matchbox pour en enlever les ID et la rendre utilisable.
% - MatchBoxs : La liste des MatchBox générer pour ce tournois.
cleanUp(MatchBoxs) -> 
    cleanUpRec(MatchBoxs, []).

cleanUpRec([], Acc) -> 
    Acc;
cleanUpRec([{_,MatchBox}|MatchBoxs], Acc) ->
    cleanUpRec(MatchBoxs, [MatchBox|Acc]).

% Permet de trié la liste de matchs pour que l'ordre coreponde à l'ordre de traitement par
% repRec()
% - Matchs : La liste des matchs du tournois
tri([Match|Matchs]) ->
    triRec(Matchs, [Match], []).

triRec([], Tmp, Acc) ->
    Acc ++ Tmp;
triRec([Match|Matchs], Tmp, Acc) ->
    [{match,_,_,_,Precedent}|_] = Tmp,
    {match,_,_,_,Id} = Match,
    if (Id div 100) =:= (Precedent div 100) ->
	    if ((Id div 100) rem 2) =:= 1 ->
		    triRec(Matchs, [Match|Tmp], Acc);
	       ((Id div 100) rem 2) =:= 0 ->
		    triRec(Matchs, Tmp++[Match], Acc)
	    end;
       (Id div 100) =/= (Precedent div 100) ->
	    triRec(Matchs, [Match], Acc++Tmp)
    end.


