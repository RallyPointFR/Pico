-module(dummyPlug).
-export([createTournoi/1,representation/1,matchJouables/1,resultat/3]).

%Module pour les tests de interfaceTournoi, simule les fonctions d'un tournoi sans traiter les données

createTournoi(_) -> true.

representation(_) -> true.

matchJouables(_) -> true.

resultat(_, _, _) -> true.
