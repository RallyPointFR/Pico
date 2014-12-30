%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_logoutButton).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, logoutButton).

-spec render_element(#logoutButton{}) -> body().
render_element(_Record = #logoutButton{}) ->
	LogoutID = wf:temp_id(),
	wf:wire(LogoutID, #event{
		type=click,
		delegate=?MODULE,
		postback={click, logout ,LogoutID}
	}),
    #listitem{body=[#link{id=LogoutID, text="Logout"}]}.
    
event({click, logout, _}) ->
	wf:logout(),
	wf:redirect("/").
