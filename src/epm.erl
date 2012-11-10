-module(epm).

-export([create/1, stub/4, any/0]).
-export([dump_code/1, build_module/1, ignored_function/1]).

create(Module) ->
	build_and_load_module(Module),
	Mod = new_name_as_atom(Module),
	{ok, Pid} = epm_data:start_link(Module),
	Mod:new(Pid).

stub(Obj, Func, Args, Result) ->
	Pid = Obj:'internal$$get_gen_server_pid'(),
	gen_server:call(Pid, {stub, Func, Args, Result}).

any() ->
	{epm_matcher, fun(_Actual) -> true end}.

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
		++ get_pid_function()
		++ Functions
		++ [{eof, ?LINE}]
	.

new_name_as_atom(Name) -> list_to_atom(new_name_as_list(Name)).
new_name_as_list(Name) -> atom_to_list(Name) ++ "$$mocked".

attributes(_Name) -> []. % TODO copy attributes from the mocked module

get_pid_function() ->
	FuncName = 'internal$$get_gen_server_pid',
	[
		{attribute, ?LINE, export, [{FuncName, 0}]},
		{function,5,FuncName,0,[{clause,5,[],[],[{var,5,'Pid'}]}]}
	].

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

ignored_function(E = {F, _A}) ->
	lists:any(
		fun (IgE) ->
			case IgE of
				{IgF} -> IgF =:= F;
				_ -> E =:= IgE
			end
		end,
		% functions listed here without an arity mean that any function of that name is ignored.
		[ {new}, {instance}, {module_info,0}, {module_info,1} ]
	).

create_function({Name, Arity}) ->
	ListOfArgs = list_of_args(Arity),
	{function, ?LINE, Name, Arity,
		[ {		clause,
				?LINE,
				ListOfArgs,
				[],
				[gen_server_call(Name, ListOfArgs)]
			}
		]
	}.

list_of_args(Arity) ->
	lists:map(fun(N) ->
			{var, ?LINE, list_to_atom(lists:flatten(io_lib:format("Arg~B", [N])))}
		end,
		lists:seq(1, Arity)
	).

gen_server_call(FunName, Args) ->
	{call, ?LINE, {
			remote, ?LINE,
			{atom, ?LINE, gen_server},
			{atom, ?LINE, call}
		},
		[
			{var, ?LINE, 'Pid'},
			{tuple, ?LINE,
				[
					{atom, ?LINE, call},
					{atom, ?LINE, FunName},
					generate_arg_list(Args)
				]
			}
		]
	}.

generate_arg_list([]) -> {nil, ?LINE};
generate_arg_list([H|T]) ->
	{cons, ?LINE, H, generate_arg_list(T)}.


