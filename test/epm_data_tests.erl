-module(epm_data_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/epm_data_rec.hrl").

it_creates_a_gen_server_test() ->
	{ok, Pid} = epm_data:start_link(),
	?assertEqual(true, is_pid(Pid)).

it_exposes_the_state_for_testing_test() ->
	{ok, Pid} = epm_data:start_link(),
	?assertEqual(true, is_record(gen_server:call(Pid, state), state)).

it_adds_a_stubbed_function_to_the_state_test() ->
	{ok, Pid} = epm_data:start_link(),
	ok = gen_server:call(Pid, {stub, func, [], {return, ok}}),
	State = gen_server:call(Pid, state),
	Stub = lists:nth(1, State#state.stubs),
	?assertEqual(#stub{func = func, args = [], result = {return, ok}}, Stub).

it_returns_the_result_from_a_stubbed_function_test() ->
	{ok, Pid} = epm_data:start_link(),
	ok = gen_server:call(Pid, {stub, func, [], {return, ok}}),
	?assertEqual(ok, gen_server:call(Pid, {call, func, []})).

it_returns_the_correct_stubbed_function_test() ->
	{ok, Pid} = epm_data:start_link(),
	ok = gen_server:call(Pid, {stub, other_func, [], {return, ok}}),
	ok = gen_server:call(Pid, {stub, func, [], {return, ok}}),
	?assertEqual(ok, gen_server:call(Pid, {call, func, []})).


multiple_stubs_of_the_same_func_are_returned_in_order_test() ->
	{ok, Pid} = epm_data:start_link(),
	ok = gen_server:call(Pid, {stub, func, [], {return, ok1}}),
	ok = gen_server:call(Pid, {stub, func, [], {return, ok2}}),
	?assertEqual(ok1, gen_server:call(Pid, {call, func, []})),
	?assertEqual(ok2, gen_server:call(Pid, {call, func, []})).

multiple_stubs_of_the_same_func_are_returned_in_order_with_the_last_one_remaining_test() ->
	{ok, Pid} = epm_data:start_link(),
	ok = epm_data:stub(Pid, func, [], {return, ok1}),
	ok = gen_server:call(Pid, {stub, func, [], {return, ok2}}),
	?assertEqual(ok1, gen_server:call(Pid, {call, func, []})),
	?assertEqual(ok2, epm_data:call(Pid, func, [])),
	?assertEqual(ok2, gen_server:call(Pid, {call, func, []})).

failure_to_mock_results_in_an_error_test() ->
	{ok, Pid} = epm_data:start_link(),
	?assertEqual({error, {not_stubbed, func, []}}, gen_server:call(Pid, {call, func, []})).

using_the_any_matcher_in_a_stubbing_test() ->
	{ok, Pid} = epm_data:start_link(),
	ok = gen_server:call(Pid, {stub, func, [epm:any()], {return, ok}}),
	ok = gen_server:call(Pid, {stub, func, [fixed], {return, ok}}),
	?assertEqual(ok, gen_server:call(Pid, {call, func, [yeah]})),
	?assertEqual(ok, gen_server:call(Pid, {call, func, [this]})).

using_the_any_matcher_with_mismatching_arg_lengths_test() ->
	{ok, Pid} = epm_data:start_link(),
	ok = gen_server:call(Pid, {stub, func, [epm:any()], {return, ok}}),
	?assertEqual({error,{not_stubbed,func,[yeah,that]}}, gen_server:call(Pid, {call, func, [yeah, that]})).


% calls
% stubs