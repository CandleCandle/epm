-module(epm_data).


-include("../include/epm_data_rec.hrl").

-export([start_link/0, stub/4, call/3]).
-export([init/1, handle_call/3, terminate/2]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

stub(Pid, Func, Args, Result) ->
	gen_server:call(Pid, {stub, Func, Args, Result}).

call(Pid, Func, Args) ->
	gen_server:call(Pid, {call, Func, Args}).


%% %% %% %% gen_server %% %% %% %%
init(_Args) ->
	{ok, #state{}}.

handle_call(state, _From, State) ->
	{reply, State, State};
handle_call({stub, Func, Args, Result}, _From, State) ->
	{reply, ok,
		State#state{
			stubs = State#state.stubs ++ [#stub{func = Func, args = Args, result = Result }]
		}
	};
handle_call({call, Func, Args}, _From, State) ->
	case find_stub(Func, Args, State) of
		E = {error, _} -> {reply, E, State};
		{Stub, Stubs} ->
			find_stub(Func, Args, State),
			{reply, do_result(Stub#stub.result), State#state{stubs=Stubs}}
	end.

terminate(_Reason, _State) -> ok.


%% %% %% %% internal %% %% %% %%
find_stub(Func, Args, State) ->
	Stubs = State#state.stubs,
	case find_relevent_stubs(Func, Args, Stubs) of
		[] -> {error, {not_stubbed, Func, Args}};
		[Elem] -> {Elem, Stubs};
		[Elem, _Rest] -> {Elem, lists:delete(Elem, Stubs)}
	end.

find_relevent_stubs(Func, Args, Stubs) ->
	Result = lists:filter(fun(Elem) ->
			(Elem#stub.func =:= Func) and (Elem#stub.args =:= Args)
		end,
		Stubs
	),
	io:format("~p~n", [Result]),
	Result.

do_result({return, Value}) -> Value.
