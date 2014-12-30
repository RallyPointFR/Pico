-module(saveManager).
-include("type.hrl").
-export([save/3,save/4, load/1, load/2, list/0, list/1]).

%possibleChars() ->
%    <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>.
fileExtansion() ->
    ".pico".
dbFile() ->
    "res/saveManager.db".
defaultDir() ->
    "res/".
errorCoruptedDB() ->
    "La table de noms des sauvegardes est corompue !".
errorUnableToModifyDB(Reason) ->
    "Impossible de mettre à jour la base de donnée : "++Reason++".".
errorNoSuchDBFile() ->
    "Table de nom introuvable.".
errorUnableToReadDBFile(Reason) ->
    "Impossible de lire la table de noms : "++Reason++".".
errorNoSuchTournament() ->
    "Tournoi introuvable.".
errorUnableToLoadTournament(Reason) ->
    "Impossible de charger le tournoi : "++Reason++".".
errorCoruptedSave() ->
    "La sauvegarde est corompue".

save(Nom, Type, Tournoi) ->
    save(Nom, Type, Tournoi, defaultDir()).
save(Nom, Type, Tournoi, Dir) ->
    case tools:unserialize(dbFile()) of
	{error, enoent} ->
	    save(Nom, Type, Tournoi, Dir, [], []);
	{ok, DB} when is_list(DB) ->
	    save(Nom, Type, Tournoi, Dir, DB, []);
	{error, Reason} ->
	    {error, Reason};
	{ok, _} ->
	    {error, errorCoruptedDB()}
    end.
save(Nom, Type, Tournoi, Dir, [{Nom, Dir, FileName}|_DB], _Acc) ->
    tools:serialize(Dir++FileName, {Nom, Type, Tournoi});
save(Nom, Type, Tournoi, Dir, [Save|DB], Acc) ->
    save(Nom, Type, Tournoi, Dir, DB, Acc++[Save]);
save(Nom, Type, Tournoi, Dir, [], DB) ->
    case tools:serialize(dbFile(), DB++[{Nom, Dir, Nom++fileExtansion()}]) of
    {error, enoent} ->
		file:write_file(dbFile(), "lol"),
		save(Nom, Type, Tournoi, Dir, [], DB);
	{error, Reason} ->
	    {error, errorUnableToModifyDB(Reason)};
	ok ->
	    tools:serialize(Dir++Nom++fileExtansion(), {Nom, Type, Tournoi})
    end.

load(Nom) ->
    load(Nom, defaultDir()).
load(Nom, Dir) ->
    case tools:unserialize(dbFile()) of
	{error, enoent} ->
	    {error, errorNoSuchDBFile()};
	{error, Reason} ->
	    {error, errorUnableToReadDBFile(Reason)};
	{ok, DB} when is_list(DB) ->
	    load(Nom, Dir, DB);
	{ok, _} ->
	    {error, errorCoruptedDB()}
    end.
load(_Nom, _Dir, []) ->
    {error, errorNoSuchTournament()};
load(Nom, Dir, [{Nom, Dir, FileName}|_DB]) ->
    case tools:unserialize(Dir++FileName) of
	{error, enoent} ->
	    {error, errorNoSuchTournament()};
	{error, Reason} ->
	    {error, errorUnableToLoadTournament(Reason)};
	{ok, Data} ->
	    case Data of
		{Nom, Type, Tournoi} when is_record(Tournoi, tournoi) and is_atom(Type) ->
		    {ok, Data};
		_ ->
		    {error, errorCoruptedSave()}
	    end
    end;
load(Nom, Dir, [_Save|DB]) ->
    load(Nom, Dir, DB).

list() ->
    case tools:unserialize(dbFile()) of
	{error, enoent} ->
	    {error, errorNoSuchDBFile()};
	{error, Reason} ->
	    {error, errorUnableToReadDBFile(Reason)};
	{ok, DB} when is_list(DB) ->
	    DB;
	{ok, _} ->
	    {error, errorCoruptedDB()}
    end.
list(Dir) ->
    case tools:unserialize(dbFile()) of
	{error, enoent} ->
	    {error, errorNoSuchDBFile()};
	{error, Reason} ->
	    {error, errorUnableToReadDBFile(Reason)};
	{ok, DB} when is_list(DB) ->
	    list(Dir, DB, []);
	{ok, _} ->
	    {error, errorCoruptedDB()}
    end.
list(_Dir, [], Acc) ->
    Acc;
list(Dir, [{Nom, Dir, Filename}|DB], Acc) ->
    list(Dir, DB, [{Nom, Dir, Filename}|Acc]);
list(Dir, [_Save|DB], Acc) ->
    list(Dir, DB, Acc).
