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

with_a_matcher_test() ->
	Epm = epm:create(module_to_mock),
	epm:stub(Epm, bar, [any, epm:any()], {return, "bar"}),
	?assertEqual("bar", Epm:bar(any, something_else)).

with_multiple_stubs_test() ->
	Epm = epm:create(module_to_mock),
	epm:stub(Epm, bar, [any, epm:any()], {return, "foo"}),
	epm:stub(Epm, bar, [any, epm:any()], {return, "bar"}),
	?assertEqual("foo", Epm:bar(any, something)),
	?assertEqual("bar", Epm:bar(any, something_else)).

