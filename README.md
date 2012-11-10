

# Introduction

Support for mocking Erlang paramertised modules

## Usage

The aim is to allow Mockito style mocking with setup being "when" calls and checks with "verify"[1] calls.

Given the module that you want to mock being:

	-module(module_to_mock, [Pid]).
	-export([get_pid/0, bar/2]).
	get_pid() -> Pid.
	bar(Arg1, Arg2) ->
		gen_server:call(Pid, {call_site, [Arg1, Arg2]}).

### Normal Operation, mixing mocks and real instances

	MockedMod = epm:create(module_to_mock),
	epm:stub(MockedMod, get_pid, [], {return, "pid 2"}),
	RealModule = module_to_mock:new("pid"),
	?assertEqual("pid", RealModule:get_field()).
	?assertEqual("pid 2", MockedMod:get_field()).

1. Create an instance of the paramertised module that you want to mock
1. stub out one function "get_field" that takes no arguments so that it always returns the list "bar"
	1. Arguments are: module_instance, function_to_mock, expected_arguments, do_this_when_it_is_called
1. Create a real instance of the module
1. The real instance does exactly what the original module should do.
1. The mocked module returns exactly what was stubbed.

### Error cases

Failure to mock a function that is called will result in a return value of [2]

	{error, {not_stubbed, Func, Args}}

### Multiple Stubbings

Mocking a function twice with the same arguments means that the return values are returned in the same order they were mocked in for example:

	epm:stub(Epm, bar, [any, something], {return, "foo"}),
	epm:stub(Epm, bar, [any, something], {return, "bar"}),
	?assertEqual("foo", Epm:bar(any, something)),
	?assertEqual("bar", Epm:bar(any, something)).

### Matchers

Matchers can be used. Line two is an example of a matcher that matches any possible value for the second argument.

	Epm = epm:create(module_to_mock),
	epm:stub(Epm, bar, [any, epm:any()], {return, "bar"}),
	?assertEqual("bar", Epm:bar(any, something_else)).

The any() matcher is a tuple:

	{epm_matcher, fun(_Actual) -> true end}.

With the second element returning the atoms: 'true' or 'false'


## Footnotes

1. Verifying function calls is currently not implemented.
	1. Currently, verifying calls would require that the call is also stubbed out.
2. This should probably throw the error rather then return it.