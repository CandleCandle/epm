-module(epm_tests).

-include_lib("eunit/include/eunit.hrl").



with_no_change_test() ->
	Epm = epm:create(module_to_mock),
	epm:stub(Epm, get_field, [], {return, "bar"}),
	Epm2 = module_to_mock:new("foo"),
	?assertEqual("foo", Epm2:get_field()).

with_change_test() ->
	Epm = epm:create(module_to_mock),
	epm:stub(Epm, get_field, [], {return, "bar"}),
	?assertEqual("bar", Epm:get_field()).


