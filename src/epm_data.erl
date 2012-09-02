-module(epm_data).


-include("../include/epm_data_rec.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, terminate/2]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init(_Args) ->
	{ok, #state{}}.

handle_call(state, _From, State) ->
	{reply, State, State};
handle_call({stub, Func, Args, Result}, _From, State) ->
	{reply, ok,
		State#state{
			stubs = State#state.stubs ++ [#stub{func = Func, args = Args, result = Result }]
		}
	}.


terminate(_Reason, _State) -> ok.