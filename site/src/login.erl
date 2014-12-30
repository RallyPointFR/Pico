%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (login).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> 
	#template { file="./site/templates/fundation.html" }.

title() -> "Login - Pico".

menu() ->
	case wf:role(admin) of
		true ->
			[
				#listitem{body=[#link{text="Create", url="/create"}]},
				#listitem{body=[#link{text="Modify", url="/modify"}]},
				#listitem{body=[#link{text="Logout", postback=logout}]}
			];
		false ->
			[
				#listitem{body=[#link{text="Login", url="/login"}]}
			]
	end.
	
event(logout) ->
	wf:logout(),
	wf:redirect("/");

event(login) ->
	SubmitedLogin = wf:q(username),
	SubmitedPassword = wf:q(password),
	case tools:unserialize("res/login") of
		{error, _} ->
			wf:flash("Error 1 while getting credential.");
		{ok, Credential} ->
			case Credential of
				{Login, Password} ->
					if SubmitedLogin =:= Login, SubmitedPassword =:= Password ->
							wf:role(admin, true),
							wf:redirect_from_login("/");
						true ->
							wf:flash("Wrong IDs.")
					end;
				_ ->
					wf:flash("Error 2 while getting credential.")
			end
	end.

body() -> 
    [
        #panel{body=[
			#flash{},
			#label{text="User:"},
			#textbox{id=username, next=password},
			#br{},
			#label{text="Password:"},
			#password{id=password, next=submit},
			#br{},
			#button{text="Login", id=submit, postback=login}
			]}
    ].
	
