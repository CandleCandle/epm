-module(epm).

-export([create/1, stub/4]).
-export([dump_code/1, build_module/1, ignored_function/1]).

create(Module) ->
	build_and_load_module(Module),
	Mod = new_name_as_atom(Module),
	Mod:new(pid).

stub(_Module, _Fun, _Args, _Result) -> ok.


dump_code(Mod) ->
	io:format("~s~n",
		[
			erl_prettypr:format(erl_syntax:list(build_module(Mod)))
		]
	).

build_and_load_module(Mod) ->
	{ok, ModName, Binary} = compile:forms(build_module(Mod), [verbose,report_errors,report_warnings]),
	{module, ModName} = code:load_binary(ModName, "", Binary).

build_module(Name) ->
	NewName = new_name_as_atom(Name),
	{Exports, Functions} = generate_functions(Name),
	[]
		++ [{attribute, ?LINE, file, {new_name_as_list(Name) ++ ".erl", ?LINE}}]
		++ [{attribute, ?LINE, module, {NewName, ['Pid']}}]
		++ attributes(Name)
		++ [{attribute, ?LINE, export, Exports}]
		++ Functions
		++ [{eof, ?LINE}]
	.

new_name_as_atom(Name) -> list_to_atom(new_name_as_list(Name)).
new_name_as_list(Name) -> atom_to_list(Name) ++ "$$mocked".

attributes(_Name) -> []. % TODO copy attributes from the mocked module


% {[ exports ], [ functions ]}.
generate_functions(ModuleName) ->
	ListOfTuples = lists:foldr(
		fun(E = {Name, Arity}, Acc) ->
			case ignored_function(E) of
				true -> Acc;
				_ ->
					ModifiedFun = {Name, Arity-1},
					Acc ++ [{ ModifiedFun, create_function(ModifiedFun) }]
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
		lists:seq(1, Arity)
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


