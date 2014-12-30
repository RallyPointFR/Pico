-module(tournoiSimple).
-export([creerTournoiTest/0, creerEquipesTest/1, creerTournoiTest/1]).
-export([createTournoi/1, resultat/3, matchJouables/1]).
-export([representation/1, representation/2, isCorectNbTeam/1]).
%-compile(export_all).
-include("type.hrl").

%==================================================================================================
%% FONCTIONS DE TEST - DEBUT
% Permet de créer le tournoi de test
creerTournoiTest() -> 
    J1 = tools:createJoueur("Derp",1),
    J2 = tools:createJoueur("Herp",2),
    E1 = tools:createEquipe("DaTeam",[J1],1),
    E2 = tools:createEquipe("DaChiz",[J2],2),
    E3 = tools:createEquipe("DaLulz",[J1],3),
    E4 = tools:createEquipe("DaSwag",[J2],4),
    R1 = tools:createResultat(E1,1),
    R2 = tools:createResultat(E2,0),
    R3 = tools:createResultat(E3,0),
    R4 = tools:createResultat(E4,1),
    C1 = tools:ordonnerResultats([R1,R2]),
    C2 = tools:ordonnerResultats([R3,R4]),
	
    V = tools:createMatch([null],[null],[null],0),
    M3 = tools:createMatch([null,null],[null,null],[V,null],3),
    M2 = tools:createMatch([E1,E2],C1,[M3,null],2),
    M1 = tools:createMatch([E3,E4],C2,[M3,null],1),
    [M1,M2,M3,V].   

creerJoueurTest() -> tools:createJoueur("Derp",1).

creerEquipesTest(0) -> [];
creerEquipesTest(N) -> [tools:createEquipe(erlang:integer_to_list(N),[creerJoueurTest()],N)]++creerEquipesTest(N-1).

creerTournoiTest(N) ->
    createTournoi(creerEquipesTest(N)).

%% FONCTIONS DE TEST - FIN
%==================================================================================================

offsetIDMatch() -> 100.

%%%%%%%% Fontion Valentin %%%%%%%%%%%%
resultat({tournoi, Matchs, Equipes}, IdMatch,  Resultats) ->
    NewMatchs = resultatRec(Matchs, IdMatch, Resultats, []),
    {tournoi, NewMatchs, Equipes}.

resultatRec([], _, _, Acc) -> Acc;
resultatRec([Match|Matchs], IdMatch, Resultats, Acc) ->
    {match, _, ResultatsMatch, Suivants, Id} = Match,
    Res = tools:ordonnerResultats(Resultats),
    if Id =:= IdMatch ->
	    if ResultatsMatch =:= []->
		    Acc++[tools:setResult(Match, Res)]++actualiser(Res, Suivants, Matchs);
	       ResultatsMatch =/= [] ->
		    throw(resultatDejaPresents)
	    end;
       Id =/= IdMatch ->
	    resultatRec(Matchs, IdMatch, Resultats, Acc++[Match])
    end.  

actualiser(_Resultats, [fin], Matchs) ->
    Matchs;
actualiser(Resultats, Suivants, Matchs) ->
    actualiserRec(Resultats, Suivants, Matchs, []).

actualiserRec(_, [], Matchs, []) -> Matchs;
actualiserRec([Resultat|Resultats], [IdMatch|Suivants], [Match|Matchs], Acc)->
    {resultat, Equipe, _} = Resultat,
    if IdMatch =:= fin -> [Match|Matchs];
       IdMatch =/= fin ->
	    case Match of
		{match,_,_,_, IdMatch} ->
		    actualiserRec(Resultats, Suivants,
				  Acc++[tools:addEquipe(Match, Equipe)]++Matchs, []);
		{match,_,_,_,_} ->
		    actualiserRec([Resultat|Resultats], [IdMatch|Suivants], Matchs, Acc++[Match])
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% representation(T, FileName) ->
%       Permet de générer un fichier svg FileName représentant le tournoi T.
%       La méthode de représentation utilisée est celle de representationTournoiSimple
% T         : Le tournoi à représenter
% FileName  : Le nom du fichier qui sera créé
representation(T) -> 
    representationTournoiSimple:representation(T).
representation(T, Filename) ->
    metierSVG:createSVG(Filename, representationTournoiSimple:representation(T)).

% createTournoi(Equipes) ->
%       Crée un tournoi dans lequel seul le premier tour est rempli.
%       Necessite un nombre d'Equipe qui soit une puissance de 2.
% Equipes   : Liste d'Equipe à utiliser pour générer le tournoi

% Cas terminal
createTournoiRec([_|[]],_,_,_) -> [];

% Lorsque toutes les équipes ont été intégrées au tournoi
% Commence alors à créer les matchs vides
createTournoiRec([],0,_,Acc) ->
    Acc++createTournoiRec(Acc,1,0,[]);

% Lorsque tous les matchs d'un niveau ont été traités
% Passe au niveau supérieur du tournoi
createTournoiRec([],J,_,Acc) when J =/= 0 -> 
    Acc++createTournoiRec(Acc,J+1,0,[]);

% Traitement sur les matchs
% Elimine les matchs deux à deux en ajoutant à chaque fois un nouveau match
createTournoiRec(Matchs,J,I,Acc) when J =/= 0 ->
    [_|[_|M]] = Matchs,
    if (M == []) and (I == 0) ->
            Suivant = tools:createMatch(fin),
            Courant = tools:createMatch([-1,-1],[],[Suivant, null],J*offsetIDMatch()+I),
            createTournoiRec(M,J,I+1,Acc++[Courant]);
       (M /= []) or (I /= 0) ->
            IdSuivant = ((J+1)*offsetIDMatch())+(I div 2),
            Suivant = tools:createMatch(IdSuivant),
            Courant = tools:createMatch([-1,-1],[],[Suivant, null],J*offsetIDMatch()+I),
            createTournoiRec(M,J,I+1,Acc++[Courant])
    end;

% Première passe, travaille sur un liste d'équipes et crée les premiers matchs du tournoi
createTournoiRec([{equipe,_,_,E1}|[{equipe,_,_,E2}|T]],0,I,Acc) ->
    if (T == []) and (I == 0) ->
           Suivant = tools:createMatch(fin),
           Courant = tools:createMatch([E1,E2],[],[Suivant, null],I),
           createTournoiRec(T,0,I+1,Acc++[Courant]);
       (T /= []) or (I /= 0) ->
           Suivant = tools:createMatch(offsetIDMatch()+I div 2),
           Courant = tools:createMatch([E1,E2],[],[Suivant, null],I),
           createTournoiRec(T,0,I+1,Acc++[Courant])
    end.

log2(X) -> math:log(X) / math:log(2).

createTournoi(Equipes) -> 
    Log = log2(length(Equipes)),
    NbEquipesMax = offsetIDMatch()*2,
    if length(Equipes) > NbEquipesMax ->
           throw(tropDEquipes);
       length(Equipes) =< NbEquipesMax ->
        if round(Log) == Log ->
            E = tools:durstenfeld(Equipes),
            #tournoi{matchs=createTournoiRec(E, 0, 0, []),equipes=Equipes};
           round(Log) /= Log ->
            throw(nombreEquipesInvalide)
        end
    end.

% matchJouables(Tournoi) ->
%       Retourne la liste des matchs jouables, c'est à dire ceux qui ont toutes les équipes participantes renseignées.
matchJouablesRec([],Acc) -> Acc;
matchJouablesRec([H|T],Acc) -> 
    {match,Equipes,Resultats,_,_} = H,
    case Equipes of
        [-1,-1] -> matchJouablesRec(T,Acc);
        [_,-1] -> matchJouablesRec(T,Acc);
        [-1,_] -> matchJouablesRec(T,Acc);
        [_,_] -> 
            case Resultats of
                [] -> matchJouablesRec(T,Acc++[H]);
                _ -> matchJouablesRec(T,Acc)
            end
    end.

matchJouables(Tournoi) -> 
    {tournoi, Matchs, _} = Tournoi,
    matchJouablesRec(Matchs,[]).

isCorectNbTeam(NbTeam) ->
    (NbTeam < offsetIDMatch()*2) and tools:isPowOf(2, NbTeam).
