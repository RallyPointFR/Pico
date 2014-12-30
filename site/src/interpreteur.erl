-module(interpreteur).
-export([start/0]).
-include("type.hrl").

% Ce fichier est l'interpretteur de commande permettant de gérer les tournois. %


% Petit sucre pour afficher une chaine de caractère suivis d'un retour ligne
println(String) when is_list(String) ->
    io:format(<<"~s~n">>, [String]);
println(Something) ->
    io:format(<<"##### Debug : ~w #####~n">>, [Something]).

% Fonctions permettant de définir les mot clef utiliser dans les commandes.
% Les actions
tokenCreate() -> "create".
tokenNewResult() -> "newResult".
tokenLoad() -> "load".
tokenSave() -> "save".
tokenQuit() -> "quit".
tokenModify() -> "modify".
tokenReset() -> "reset".
tokenShow() -> "show".
tokenHelp() -> "help".
tokenExport() -> "export".
tokenList() -> "list".
tokenPlayablesMatchs() -> "playablesMatchs".

% Les modes de création du tournoi
tokenModeJoueur() -> "byPlayer".
tokenModeEquipe() -> "byTeam".

% Les mode de modifications du tournoi
%tokenModifyMode() -> "mode".
tokenModifyType() -> "type".
tokenModifyNom() -> "name".

% Les symbole d'accpetation ou de refus d'un proposition
tokenAccept() -> "y".
tokenDenied() -> "n".
tokenCancel() -> "c".

%Le dossier où seront enregistrer les SVGs et les sauvegardes
resFolder() ->
    "../res/".

% Les messages et textes
welcome() -> 
    io:format(<<"Bienvenue dans PICO !~nTapez help pour afficher l'aide.~n">>).
by() -> 
    io:format(<<"A bientôt !~n">>).
prompt() -> 
    <<">>> ">>.
errorInvalidAction(Action) -> 
    io:format("Erreur ! L'action ~s n'existe pas. Tapez help pour plus "++
		"d'informations.~n", [Action]).
errorInvalidCreationMode(Mode) -> 
    io:format("Erreur ! Le mode de création ~s n'existe pas. Tapez help pour "++
		"plus d'information.~n", [Mode]).
errorInvalidNbJoueurs() -> 
    io:format("Erreur ! Le nombre de joueurs n'est pas un multiple du nombre d'équipe.~n"++
		  "Veuillez entrer un nombre de joueurs corect ou changer nombre"++
		  "d'équipes.~n").
errorNotANumber() ->
    io:format(<<"Erreur ! Veuillez entrer un nombre corect.~n">>).
errorInvalidNbEquipes() ->
    io:format("Erreur ! Le nombre d'équipe n'est pas valide.~n"++
		  "Veuillez vous reporté aux règles du tournois pour plus d'informations.~n").
errorInvalidModificationMode(Mode) ->
    io:format("Erreur ! Le mode ~s n'est pas un mode de modification. Tapez help "++
		"pour plus d'information~n", [Mode]).
errorNoLoadedTournament() ->
    io:format(<<"Aucun tournoi n'est actuellement charger.~n">>).
errorTooManyArguments() ->
    io:format("Erreur : Vous avez entré trop d'arguments ! Tapez help pour plus"++
		  "d'inforamtion.~n").
errorTooFewArguments() ->
    io:format("Erreur : Vous n'avez pas entré asser d'arguements ! Tapez help pour plus"++
		  "d'inforamtions.~n").
infoNewTeam() -> 
    io:format(<<"    Création d'une nouvelle équipe :~n">>).
infoNewPlayer() -> 
    io:format(<<"        Création d'un nouveau joueur :~n">>).
infoNewTournoi() -> 
    io:format(<<"Création d'un nouveau tournoi : ~n">>).
infoAppendingTeams() -> 
    io:format(<<"    Ajout des équipes :~n">>).
infoAppendingPlayers() -> 
    io:format(<<"        Ajout des joueurs :~n">>).
infoCreationFailed() -> 
    io:format(<<"La création du tournoi à échoué.~n">>).
infoCreationSucces() -> 
    io:format(<<"Création du tournoi réussis !~n">>).
infoLoadingFailed() -> 
    io:format(<<"Le chargement à lamentablement échouer.~n">>).
infoLoadingSucces() -> 
    io:format(<<"Chargement réussis !~n">>).
infoSavingFailed() -> 
    io:format(<<"La sauvegarde à lamentablement échouer.~n">>).
infoSavingSucces() -> 
    io:format(<<"Sauvegarde réussi !~n">>).
infoSaving() -> 
    io:format(<<"Sauvegarde en cours...~n">>).
infoExportingSVG() -> 
    io:format(<<"Exportation en image SVG...~n">>).
infoSVGExportSucces() -> 
    io:format(<<"Exportation réussie !~n">>).
infoSVGExportFailed() -> 
    io:format(<<"Echec de l'exportation.~n">>).
infoResetSucces() -> 
    io:format(<<"Réinitialisation résussie !~n">>).
infoResetFailed() ->
    io:format(<<"La réinitialisation à échouer.~n">>).
infoResultAdded() ->
    io:format(<<"Résultat ajouter avec succès !~n">>).
infoResultAddFailed() ->
    io:format(<<"Echec de l'ajout du résultat.~n">>).
infoModificationAborted() ->
    io:format(<<"La modification à été annulée.~n">>).
infoModificationSucces() ->
    io:format(<<"La modification à été effectuer avec succès !~n">>).


% Lancement de l'interpreteur
start() ->
    welcome(),
    main({"", null}),
    by().

% Boucle principale de l'interpreteur
main({Nom, Tournoi}) ->
    Line = io:get_line(Nom++prompt()),
    case commande(Nom, Tournoi, string:tokens(Line,"\n\t ")) of
        quit -> ok;
	fail -> main({Nom, Tournoi});
        NewData  -> main(NewData)
    end.


% Parsing du premier token
commande(Nom, Tournoi, [Action|Expression]) ->
    TokenCreate = tokenCreate(),
    TokenLoad = tokenLoad(),
    TokenSave = tokenSave(),
    TokenNewResult = tokenNewResult(),
    TokenShow = tokenShow(),
    TokenModify = tokenModify(),
    TokenReset = tokenReset(),
    TokenQuit = tokenQuit(),
    TokenHelp = tokenHelp(),
    TokenExport = tokenExport(),
    TokenList = tokenList(),
    TokenPlayablesMatchs = tokenPlayablesMatchs(),
    Commande = case Action of
		   TokenCreate          -> fun create/3;
		   TokenLoad            -> fun load/3;
		   TokenSave            -> fun save/3;
		   TokenNewResult       -> fun newResult/3;
		   TokenShow            -> fun show/3;
		   TokenModify          -> fun modify/3;
		   TokenReset           -> fun reset/3;
		   TokenQuit            -> fun quit/3;
		   TokenHelp            -> fun help/3;
		   TokenExport          -> fun export/3;
		   TokenList            -> fun list/3;
		   TokenPlayablesMatchs -> fun playablesMatchs/3;
		   _                    -> invalidAction
	       end,
    if Commande =:= invalidAction -> 
	    errorInvalidAction(Action),
	    fail;
       is_function(Commande, 3) -> 
	    Commande(Nom, Tournoi, Expression)
    end;
commande(_Nom, _Tournoi, []) ->
    fail.

    


% La Commande de création
create(_Nom , _Tournoi, [Mode, T, Nom]) ->
    TokenModeJoueur = tokenModeJoueur(),
    TokenModeEquipe = tokenModeEquipe(),
    Creation =  case Mode of
		    TokenModeJoueur -> fun createByPlayer/3;
		    TokenModeEquipe -> fun createByTeam/3;
		    _               -> invalidCreationMode
		end,
    if Creation =:= invalidCreationMode -> 
	    errorInvalidCreationMode(Mode),
	    infoCreationFailed,
	    fail;
       is_function(Creation, 3) -> 
	    infoNewTournoi(),
	    case io:fread("    Nombre de joueurs "++prompt(),"~u")of
		{error, _} ->
		    errorNotANumber(),
		    infoCreationFailed(),
		    fail;
		{ok, [NbJoueurs]} ->
		    Type = list_to_atom(T),
		    case io:fread("    Nombre d'équipes "++prompt(),"~u") of
			{error, _} ->
			    errorNotANumber(),
			    infoCreationFailed(),
			    fail;
			{ok, [NbEquipes]} ->
			    case interfaceTournoi:isCorectNbTeam(Type, NbEquipes) of
				{fail, Reason} -> 
				    println(Reason),
				    infoCreationFailed(),
				    fail;
				{ok, false} ->
				    errorInvalidNbEquipes(),
				    infoCreationFailed(),
				    fail;
				{ok, true} ->
				    if (NbJoueurs rem NbEquipes) =/= 0 -> 
					    errorInvalidNbJoueurs(),
					    fail;
				       (NbJoueurs rem NbEquipes) =:= 0 ->
					    case Creation(Type, NbJoueurs, NbEquipes) of
						fail -> fail;
						Tournoi -> {Nom, {Type, Tournoi}}
					    end
				    end
			    end
		    end
	    end
    end;
create(_Nom, _Tournoi, [_,_,_,_|_]) ->
    errorTooManyArguments(),
    infoCreationFailed(),
    fail;
create(_Nom, _Tournoi, _) ->
    errorTooFewArguments(),
    infoCreationFailed(),
    fail.



% On ne demande que les joueurs puis il sont aléatoirement placer dans des équipes.
createByPlayer(Type, NbJoueurs, NbEquipes) ->
    Players = tools:durstenfeld(createJoueurs(NbJoueurs)),
    Teams = tools:createEquipe(tools:parPaquets(NbJoueurs div NbEquipes, Players), ""),
    case interfaceTournoi:createTournoi(Type, Teams) of
	{fail, Reason} ->
	    println(Reason),
	    infoCreationFailed(),
	    fail;
	{ok, Tournoi} ->
	    infoCreationSucces(),
	    Tournoi
    end.


% On demande à la fois les joueurs et la composition des équipes
createByTeam(Type, NbJoueurs, NbEquipes) ->
    case 
	interfaceTournoi:createTournoi(Type,createTeams(NbEquipes,NbJoueurs div NbEquipes))
    of
	{fail, Reason} -> 
	    println(Reason),
	    infoCreationFailed(),
	    fail;
	{ok,Tournoi} -> 
	    infoCreationSucces(),
	    Tournoi
    end.

% Creation de NbTeam équipes de NbJoueurs dans chacune d'entre elle.
createTeams(NbEquipes, NbJoueurs) ->
    infoAppendingTeams(),
    createTeams(NbEquipes, NbJoueurs, 0, []).
createTeams(0, _NbJouers, _Id , Acc) -> Acc;
createTeams(NbEquipes, NbJoueurs, Id, Acc) ->
    infoNewTeam(),
    {ok, [Nom]} = io:fread("        Nom de l'équipe "++prompt(), "~s"),
    createTeams(NbEquipes-1, NbJoueurs, Id+1, 
		[tools:createEquipe(Nom, createJoueurs(NbJoueurs), Id)|Acc]).

% Création de NbJoueurs  joueurs qui seront renvoyer dans un tableau.
createJoueurs(NbJoueurs) ->
    infoAppendingPlayers(),
    createJoueurs(NbJoueurs, 0, []).
createJoueurs(0, _Id, Acc) -> Acc;
createJoueurs(NbJoueurs, Id, Acc) -> 
    infoNewPlayer(),
    {ok, [Nom]} = io:fread("            Nom du joueur "++prompt(), "~s"),
    createJoueurs(NbJoueurs-1, Id+1, [tools:createJoueur(Nom, Id)|Acc]).

% La commande de chargement de tournoi
load(_Nom, _Tournoi, [Nom]) ->
    load(null, null, [Nom, resFolder()]);
load(_Nom, _Tournoi, [Nom, Dir]) ->
    case saveManager:load(Nom, Dir) of
	{error, Reason} ->
	    println(Reason),
	    infoLoadingFailed(),
	    fail;
	{ok, {Nom, Type, Tournoi}} ->
	    infoLoadingSucces(),
	    {Nom, {Type, Tournoi}}
    end;
load(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    infoLoadingFailed(),
    fail.

% La commande de sauvegarde de tournoi qui sauvegarde le tournoi courant
save(Nom, Tournoi, []) ->
    save(Nom, Tournoi, [resFolder()]);
save(Nom, {Type, Tournoi}, [Chemin]) ->
    infoSaving(),
    case saveManager:save(Nom, Type, Tournoi, Chemin) of
	{error, Reason} ->
	    println(Reason),
	    infoSavingFailed(),
	    fail;
	ok ->
	    infoSavingSucces()
    end,
    {Nom, {Type, Tournoi}};
save(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    infoSavingFailed(),
    fail.




export(Nom, {Type, Tournoi}, []) ->		    
    export(Nom, {Type, Tournoi}, [resFolder()]);
export(Nom, {Type, Tournoi}, [Chemin]) ->	
    infoExportingSVG(),
    case interfaceTournoi:representation(Type, Tournoi, Chemin++Nom++".svg") of
	{ok, ok} -> 
	    infoSVGExportSucces(),
	    {Nom, {Type, Tournoi}};
	{ok, {error, Error}} ->
		println(Error),
		infoSVGExportFailed(),
		fail;
	{fail, Error} ->
	    println(Error),
	    infoSVGExportFailed(),
	    fail
    end;
export(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    infoSVGExportFailed(),
    fail.

% La commande de mise à jour des résultats du tournoi.
newResult(Nom, {Type, Tournoi}, [Id]) when Tournoi =/= null ->
    IdMatch = try list_to_integer(Id)
	      catch error:badarg -> null
	      end,
    if
	IdMatch == null ->
	    errorNotANumber(),
	    infoResultAddFailed(),
	    fail;
	is_integer(IdMatch) ->
	    {match, EquipeIds, _, _, _} = tools:getMatch(IdMatch, Tournoi),
	    case interfaceTournoi:resultat(Type,Tournoi,IdMatch,
					   getResults(Tournoi, EquipeIds)) of
		{ok, NewTournoi} ->
		    infoResultAdded(),
		    {Nom, {Type, NewTournoi}};
		{fail, Reason} ->
		    println(Reason),
		    infoResultAddFailed(),
		    fail
	    end
    end;
newResult(_Nom, null, _Expression) ->
    errorNoLoadedTournament(),
    infoResultAddFailed(),
    fail;
newResult(_Nom, _Tournoi, []) ->
    errorTooFewArguments(),
    infoResultAddFailed(),
    fail;
newResult(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    infoResultAddFailed(),
    fail.


getResults(Tournoi, EquipeIds) ->
    getResults(Tournoi, EquipeIds, []).
getResults(_Tournoi, [], Acc) -> Acc;
getResults(Tournoi, [Id|EquipeIds], Acc) ->
    {ok, [Score]} = io:fread("    Score de "++
			    tools:getEquipeName(tools:getEquipe(Id, Tournoi))++
			    " (Id : "++integer_to_list(Id)++") "++prompt(), "~u"),
    Resultat = tools:createResultat(Id, Score),
    getResults(Tournoi, EquipeIds, [Resultat|Acc]).
    
    
    

% La commande de modification du tournoi. On parle ici de modifier soit le type, nom ou 
% le mode de création de équipes (aléatoire ou manuelle).
modify(Nom, Tournoi, [Mode|Expression]) ->
    TokenModifyType = tokenModifyType(),
    TokenModifyNom = tokenModifyNom(),
    Modify = case Mode of
		 TokenModifyType -> fun modifyType/3;
		 TokenModifyNom  -> fun modifyNom/3;
		 _               -> invalidModifyMode
	     end,
    if Modify =:= invalidModifyMode ->
	    errorInvalidModificationMode(Mode),
	    infoModificationAborted(),
	    fail;
       is_function(Modify, 3) ->
	    Modify(Nom, Tournoi, Expression)
    end.

modifyType(Nom, {_Type, Tournoi}, [T]) ->
    Type = list_to_atom(T),
    TokenAccept = tokenAccept(),
    TokenDenied = tokenDenied(),
    println("Attention, cette action entrainera le reset du tournoi."),
    {ok, [Confirm]} = io:fread("Êtes-vous sûre de vouloir continuer ? ("++
				   tokenAccept()++" ou "++tokenDenied()++") "++ prompt(), "~s"),
    case Confirm of
	TokenAccept ->
	    case reset(Nom, {Type, Tournoi}, []) of
		fail ->
		    infoModificationAborted(),
		    fail;
		New  ->
		    infoModificationSucces(),
		    New
	    end;
	TokenDenied ->
	    infoModificationAborted(),
	    fail
    end;
modifyType(_Nom, _Tournoi, []) ->
    errorTooFewArguments(),
    infoModificationAborted(),
    fail;
modifyType(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    infoModificationAborted(),
    fail.



modifyNom(_Nom, Tournoi, [Nom]) ->
    infoModificationSucces(),
    {Nom, Tournoi};
modifyNom(_Nom, _Tournoi, []) ->
    errorTooFewArguments(),
    infoModificationAborted(),
    fail;
modifyNom(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    infoModificationAborted(),
    fail.

% La commande de réintialisation du tournoi
reset(Nom, {Type, Tournoi}, []) ->
    case interfaceTournoi:createTournoi(Type, Tournoi#tournoi.equipes) of
	{fail, Reason} ->
	    println(Reason),
	    infoResetFailed(),
	    fail;
	{ok, NewTournoi} ->
	    infoResetSucces(),
	    {Nom, {Type, NewTournoi}}
    end;
reset(_Nom, null, []) ->
    errorNoLoadedTournament(),
    infoResetFailed(),
    fail;
reset(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    infoResetFailed(),
    fail.


% La commande de sortie de l'interpreuteur.
quit(Nom, Tournoi, []) ->
    TokenAccept = tokenAccept(),
    TokenDenied = tokenDenied(),
    TokenCancel  = tokenCancel(),
    case Tournoi of
	null -> quit;
	{_,_} ->
	    println("Attention, tout changement non sauvergardé seront perdu !"),
	    {ok, [Confirm]} = io:fread("Voulez-vous sauvergarder avant de quitter ? ("++
					   tokenAccept()++" ou "++tokenDenied()++" ou "++
					   tokenCancel()++") "++prompt(),"~s"),
	    case Confirm of
		TokenAccept -> save(Nom, Tournoi, []), quit;
		TokenDenied -> quit;
		TokenCancel  -> fail;
		_           -> println("Veuillez entrer "++tokenAccept()++" pour oui ou "++
					   tokenDenied()++" pour non ou "++tokenCancel()++
					   "pour annuler."),
			       quit(Nom, Tournoi, [])
	    end
    end;
quit(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    fail.

% La commande permettant d'avoir la liste des tournoi actuellement présent dans la table.
list(Nom, Tournoi, []) ->
    showList(saveManager:list()),
    {Nom, Tournoi};
list(Nom, Tournoi, [Dir]) ->
    showList(saveManager:list(Dir)),
    {Nom, Tournoi};
list(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    fail.

showList([]) ->
    println("");
showList([{Nom, _Dir, _FileName}|Lignes]) ->
    println(Nom),
    showList(Lignes).

playablesMatchs(_Nom, null, _Expression) ->
    errorNoLoadedTournament(),
    fail;
playablesMatchs(Nom, {Type, Tournoi}, []) ->
    case interfaceTournoi:matchJouables(Type, Tournoi) of
	{error, Reason} ->
	    println(Reason);
	{ok, Data} ->
	    showPlayablesMatchs(Data, Tournoi#tournoi.equipes)
    end,
    {Nom, {Type, Tournoi}};
playablesMatchs(_Nom, _Tournoi, _Exression) ->
    errorTooManyArguments(),
    fail.

showPlayablesMatchs([], _Equipes) ->
    println("");
showPlayablesMatchs([{match, [IdE|Ids], [], _MatchsSuivants, IdM}|Matchs], Equipes) ->
    println("Match "++integer_to_list(IdM)++" : "++tools:getEquipeName(IdE, Equipes)++
		"("++integer_to_list(IdE)++")"++listEquipesToString(Ids, Equipes)),
    showPlayablesMatchs(Matchs, Equipes).

listEquipesToString([], _Equipes) ->
    "";
listEquipesToString([IdE|Ids], Equipes) ->
    " vs "++tools:getEquipeName(IdE, Equipes)++"("++integer_to_list(IdE)++")"++listEquipesToString(Ids, Equipes).


% La commande permettant d'afficher en brut le tournoi dans l'interpreteur. 
% C'est principalement une fonction de débug qui seras sans doute rtirer ultérieurement.
show(Nom, Tournoi, []) ->
    io:format("~p~n", [Tournoi]),
    {Nom, Tournoi};
show(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    fail.



% La commande qui afficher l'aide. Peut être placer l'aide dans un fichier
% extèrne ?
help(Nom, Tournoi, []) ->
    io:format("~nPico v."++ tools:version() ++".~n~n"++
		  "-> "++tokenCreate()++" : Permet de créer un tournoi.~n"++
		  "    Utilisation : "++tokenCreate()++" Mode Type Nom~n"++
		  "        Mode : ~n"++
		  "            -> "++tokenModeJoueur()++" : Tous les joueurs du tournois serons créer~n"++
		  "                          puis aléatoirement placer dans un nombre ~n"++
		  "                          d'équipe indiquer par l'utilisateur.~n"++
		  "            -> "++tokenModeEquipe()++"   : Tous les jouerurs et les équipes serons ~n"++
		  "                          créer manuellement.~n"++
		  "        Type : Le type de tournoi à créer (exemple: tournoiSimple).~n"++
		  "        Nom : Le nom du tournoi. Il ne doit pas contenir d'espace.~n"++
		  "~n"++
		  "-> "++tokenSave()++" : Permet de sauvegarder le tournoi courant.~n"++
		  "    Utilisation : "++tokenSave()++"~n"++
		  "~n"++
		  "-> "++tokenExport()++" : Permet d'exporter le tournoi en un image format SVG.~n"++
		  "    Utilisation : "++tokenExport()++"~n"++
		  "~n"++
		  "-> "++tokenLoad()++" : Permet de charger un tournoi déjà existant.~n"++
		  "    Utilisation : "++tokenLoad()++" Nom~n"++
		  "        Nom : Le nom du tournoi à charger.~n"++
		  "~n"++
		  "-> "++tokenNewResult()++" : Permet de mettre à jour le tournoi en y ajoutant le ~n"++
		  "               résultat d'un match~n"++
		  "    Utilisation : "++tokenNewResult()++" IdMatch~n"++
		  "        IdMatch : L'id du match dont on souhaite entrer le résultat~n"++
		  "~n"++
		  "-> "++tokenModify()++" : Permet de modifier le type ou le nom du tournoi courant.~n"++
		  "            Attention, changer le type du tournoi le réinitialisera !~n"++
		  "    Utilisation : "++tokenModify()++" Mode Paramètre~n"++
		  "         Mode :~n"++
		  "             -> "++tokenModifyType()++" : Change le type du tournoi courant.~n"++
		  "                       Attention, cela réinitialise le tournoi.~n"++
		  "                 Paramètre : Le nom du nouveau type de tournoi.~n"++
		  "             -> "++tokenModifyNom()++" : Change le nom du tournoi courant.~n"++
		  "                 Paramètre : Le nouveau nom du tournoi.~n"++
		  "~n"++
		  "-> "++tokenReset()++" : Permet de réinitialiser le tournoi courant.~n"++
		  "           Attention, cette action est définitive une fois la ~n"++
		  "           commande save éxécuter !~n"++
		  "    Utilisation : "++tokenReset()++"~n"++
		  "~n"++
		  "-> "++tokenShow()++" : Affiche la structure du tournoi courant. Cette commande~n"++
		  "          est une commande de débug pour les dévelopeur.~n"++
		  "    Utilisation : "++tokenShow()++"~n"++
		  "~n"++
		  "-> "++tokenQuit()++" : Permet de quitter l'application~n"++
		  "    Utilisation : "++tokenQuit()++"~n"++
		  "~n"++
		  "-> "++tokenHelp()++" : affiche cette page d'information.~n"++
		  "    Utilisation : "++tokenHelp()++"~n~n"),
    {Nom, Tournoi};
help(_Nom, _Tournoi, _Expression) ->
    errorTooManyArguments(),
    fail.

