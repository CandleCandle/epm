-module(epm_data).


-include("../include/epm_data_rec.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init(_Args) ->
	{ok, #state{}}.

handle_call(state, _From, State) ->
	{reply, State, State}.