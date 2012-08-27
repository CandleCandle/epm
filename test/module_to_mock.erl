-module(module_to_mock, [Pid]).

-export([get_field/0, bar/2]).

get_field() -> Pid.

bar(Arg1, Arg2) ->
	gen_server:call(Pid, {call_site, [Arg1, Arg2]}).