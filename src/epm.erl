-module(epm).

-export([create/1, stub/4]).
-export([dump_code/1, build_module/1, ignored_function/1]).

create(_Module) -> ok.

stub(_Module, _Fun, _Args, _Result) -> ok.


dump_code(Mod) ->
	io:format("~s~n",
		[
			erl_prettypr:format(erl_syntax:list(build_module(Mod)))
		]
	).

build_module(Name) ->
	NewName = list_to_atom(atom_to_list(Name) ++ "$$mocked"),
	{Exports, Functions} = generate_functions(Name),
	[{attribute, ?LINE, module, {NewName, ['Pid']}}]
		++ attributes(Name)
		++ [{attribute, ?LINE, export, Exports}]
		++ Functions
		++ [{eof, ?LINE}]
	.

attributes(_Name) -> []. % TODO copy attributes from the mocked module


% {[ exports ], [ functions ]}.
generate_functions(ModuleName) ->
	ListOfTuples = lists:foldr(
		fun(E, Acc) ->
			case ignored_function(E) of
				true -> Acc;
				_ ->
					Acc ++ [{ E, create_function(E) }]
			end
		end,
		[],
		ModuleName:module_info(exports)
	),
	{
		lists:flatmap(fun({Ex, _}) -> [Ex] end, ListOfTuples),
		lists:flatmap(fun({_, Fu}) -> [Fu] end, ListOfTuples)
	}.

ignored_function(E) ->
	lists:any(
		fun (IgE) -> E =:= IgE end,
		[ {new,1}, {instance,1}, {module_info,0}, {module_info,1} ]
	).

create_function({Name, Arity}) ->
	ListOfArgs = list_of_args(Arity),
	{function, ?LINE, Name, Arity,
		[ {		clause,
				?LINE,
				ListOfArgs,
				[],
				[gen_server_call(ListOfArgs)]
			}
		]
	}.

list_of_args(Arity) ->
	lists:map(fun(N) ->
			{var, ?LINE, list_to_atom(lists:flatten(io_lib:format("Arg~B", [N])))}
		end,
		lists:seq(1, Arity-1) % -1 ro remove the default paramertzed module argument.
	).

gen_server_call(Args) ->
	{call, ?LINE, {
			remote, ?LINE,
			{atom, ?LINE, gen_server},
			{atom, ?LINE, call}
		},
		[
			{var, ?LINE, 'Pid'},
			{tuple, ?LINE,
				[
					{atom, ?LINE, call_site},
					generate_arg_list(Args)
				]
			}
		]
	}.

generate_arg_list([]) -> {nil, ?LINE};
generate_arg_list([H|T]) ->
	{cons, ?LINE, H, generate_arg_list(T)}.


