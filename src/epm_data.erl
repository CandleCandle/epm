-module(epm_data).



-export([start_link/0]).
-export([init/1]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init(_Args) ->
	{ok, state}.