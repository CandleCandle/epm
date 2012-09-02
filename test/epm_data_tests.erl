-module(epm_data_tests).

-include_lib("eunit/include/eunit.hrl").

it_creates_a_gen_server_test() ->
	{ok, Pid} = epm_data:start_link(),
	?assertEqual(true, is_pid(Pid)).