-module(epm_data_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/epm_data_rec.hrl").

it_creates_a_gen_server_test() ->
	{ok, Pid} = epm_data:start_link(),
	?assertEqual(true, is_pid(Pid)).

it_exposes_the_state_for_testing_test() ->
	{ok, Pid} = epm_data:start_link(),
	?assertEqual(true, is_record(gen_server:call(Pid, state), state)).

% calls
% stubs
