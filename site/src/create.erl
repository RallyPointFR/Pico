%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (create).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("type.hrl").

% Bonjour maël du futur !
% Si tu lit ça, je te pris de remplacer l'HTML en dur
% par un RESTfulform (regarde la doc nitrogen) et le js
% par des évenements Nitrogen.
% Pense aussi à gérer le cas ou nbPlayer mod 2 != 0.
% Bisous et bon courage.

main() -> 
	% Work around degeulasse pour la lazy compilation étrange de la console nitrogen.
	tournoiSimple:creerTournoiTest(),
	% Random
	{X, Y, Z} = now(),
	random:seed(X, Y, Z),
	case wf:role(admin) of
		true ->
			#template { file="./site/templates/fundation.html" };
		false ->
			wf:redirect_to_login("/login")
	end.

title() -> "Create tournament".

menu() ->
	menu:fundationMenu().
    
create(A) ->
	Nbj = erlang:list_to_integer(A),
	Joueur = getAllTeam([], Nbj),
	{ok, T} = interfaceTournoi:createTournoi(tournoiSimple, Joueur),
	ID = erlang:bitstring_to_list(new_paste_id()),
	ok = saveManager:save(ID, tournoiSimple, T, "res/"),
	wf:redirect("/show?id="++ID).
			

body() ->
	case wf:q(nbj) of
		undefined ->
			[
				javascript(),
				form()
			];
		A ->
			create(A)
	end.

form() ->
	"
	<form action=\"/create\" method=\"post\">
		Nombre de joueur : <input type=\"number\" name=\"nbj\" id=\"nbj\" onkeyup=\"addFields();\" onchange=\"addFields();\">
		<div id=\"container\">
		</div>
		<input type=\"submit\" name=\"submit\">
	</form>
	".
	
javascript() ->
	"
	<script type='text/javascript'>
        function addFields(){
            var number = document.getElementById(\"nbj\").value;
            var container = document.getElementById(\"container\");
            
            while (container.hasChildNodes()) {
                container.removeChild(container.lastChild);
            }
            
            for (i=0;i<number;i++){
                container.appendChild(document.createTextNode(\"Nom equipe \" + (i+1) + \" : \"));
                var input = document.createElement(\"input\");
                input.type = \"text\";
                input.name = \"equipe\" + (i+1);
                container.appendChild(input);
                
                container.appendChild(document.createElement(\"br\"));
            }
        }
    </script>
    ".

getAllTeam(Teams, 0) ->
	Teams;

getAllTeam(Teams, Nb) ->
	getAllTeam(Teams++[#equipe{nom=(wf:q(erlang:list_to_atom("equipe"++erlang:integer_to_list(Nb)))), joueur=[], id=Nb}], Nb-1).

new_paste_id() ->
	Initial = random:uniform(62) - 1,
	new_paste_id(<<Initial>>, 7).
	
new_paste_id(Bin, 0) ->
	Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
	<< <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
	
new_paste_id(Bin, Rem) ->
	Next = random:uniform(62) - 1,
	new_paste_id(<<Bin/binary, Next>>, Rem - 1).


