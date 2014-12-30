-module(menu).

-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-export([fundationMenu/0, nitrogenMenu/0]).

fundationMenu() ->
	case wf:role(admin) of
		true ->
			[
				#listitem{body=[#link{text="Create", url="/create"}]},
				#listitem{body=[#link{text="Modify", url="/modify"}]},
				#logoutButton{}
			];
		false ->
			[
				#listitem{body=[#link{text="Login", url="/login"}]}
			]
	end.
	
nitrogenMenu() ->
	#mobile_panel { 
	  display_mode=push,
	  body=[
		#h3{text="Menu"},
			"Here is a slide-out side menu"
	  ]
}.
	

