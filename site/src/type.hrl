% Type Joueur :
% nom 	= Nom du joueur.
% id 	= Id unique du joueur.
-record(joueur, {nom="null", id=-1}).

% Type Equipe :
% joueur	= Liste de Joueur ( ID ).
% id		= Id unique de l'equipe.
-record(equipe, {nom="null", joueur=[], id=-1}).

% Type Resultat :
% equipe	= Id de l'équipe.
% score		= Score de l'équipe.
-record(resultat, {equipe=-1, score=0}).

% Type Match :
% equipes	= Liste d'id des equipes s'affrontant.
% classement	= Liste de resultat de même taille que les équipes contenant un record score par équipe, ordonner en classement decroissant.
% matchSuiv	= Liste des matchs suivants, dans l'ordre du classement.
% id		= Id unique du match.
-record(match, {equipes=[], classement=[], matchSuiv=[], id=-1}). 

% Type Tournois :
% matchs        = Liste des match de départ du tournois
% equipes       = Equipes qui participe au tournois.
-record(tournoi, {matchs=[], equipes=[]}).
