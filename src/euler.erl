%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : (Un)common math problems
%%%
%%% Created : Dec 10, 2013
%%% -------------------------------------------------------------------
-module(euler).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([start/0, stop/0, state/0, settings/1, settings/2]).

-define(MNESIA_TIMEOUT, 10000).

%%
%% Start application
%%
%% Returns: ok | {error, Reason}
%%
start() ->
	% Create application dependency store
	ets:new(?MODULE, [named_table, public]),
	ensure_started(kernel),
	ensure_started(stdlib),
	ensure_started(lager),
	ensure_started(mnesia),
	Res = application:start(?MODULE),
	% Let the store persist
	ets:setopts(?MODULE, [{heir, whereis(euler_sup), ?MODULE}]),
	Res.

%%
%% Stop application
%%
%% Returns: -- (halt)
%%
stop() ->
	Deps = lists:sort(fun([_, T1], [_, T2]) ->
						  timer:now_diff(T1, T2) > 0
					  end,
					  ets:match(?MODULE, {'$1', '$2'})),
	ets:delete(?MODULE),
	application:stop(?MODULE),
	% Stop all the dependencies
	ensure_stopped(Deps),
	erlang:halt().

%%
%% Show application state
%%
%% Returns: {ok, Proplist} | {error, Reason}
%%
state() ->
	case lists:keyfind(?MODULE, 1, application:which_applications()) of
		false ->
			{error, "not running"};
		{Name, Desc, Version} ->
			{ok, [{name, Name}, {description, Desc}, {version, Version}]}
	end.

%%
%% Get application settings
%%
%% Params: Module
%% Returns: Proplist
%%
settings(Module) ->
	case application:get_env(?MODULE, Module) of
		{ok, Props} -> Props;
		_ -> []
	end.

%%
%% Set application settings
%%
%% Params: Module
%% Params: {Key, Value} | Proplist
%%
settings(Module, {Key, Value}) ->
	Settings = settings(Module),
	NewSettings = update({Key, Value}, Settings),
	application:set_env(?MODULE, Module, NewSettings);
settings(Module, Settings) ->
	lists:foreach(fun(Setting) -> settings(Module, Setting) end, Settings).

%%
%% Local functions
%%
ensure_started(App) ->
	prestart(App),
	case application:start(App) of
		ok ->
			ets:insert(?MODULE, {App, os:timestamp()}),
			poststart(App);
		{error, {already_started, _}} ->
			ok;
		{error, {not_started, Dep}} ->
			ensure_started(Dep),
			ensure_started(App)
	end.

ensure_stopped([]) ->
	ok;
ensure_stopped([[App, _] | T]) ->
	prestop(App),
	application:stop(App),
	poststop(App),
	ensure_stopped(T).

prestart(mnesia) ->
	case os:getenv("EULER_MASTER") of
		Standalone when (Standalone == false) or (Standalone == []) ->
			% Schema must be created in standalone mode
			mnesia:create_schema([node()]);
		Master ->
			% Join a cluster and clear stale replica
			pong = net_adm:ping(erlang:list_to_atom(Master)),
			lists:foreach(fun(N) ->
							  rpc:call(N, mnesia, del_table_copy, [schema, node()])
						  end,
						  nodes())
	end;
prestart(_) ->
	ok.

-include("eu_prime_tb.hrl").

poststart(mnesia) ->
	case os:getenv("EULER_MASTER") of
		Standalone when (Standalone == false) or (Standalone == []) ->
			% Create tables
			mnesia:create_table(eu_prime_tb, [{attributes, record_info(fields, eu_prime_tb)},
											  {disc_copies, [node()]}, {type, ordered_set},
											  {index, []}]),
			ok = mnesia:wait_for_tables([eu_prime_tb], ?MNESIA_TIMEOUT);
		Master ->
			% Create fresh replicas
			{ok, _} = rpc:call(erlang:list_to_atom(Master), mnesia, change_config, [extra_db_nodes, [node()]]),
			mnesia:change_table_copy_type(schema, node(), disc_copies),
            {atomic, ok} = mnesia:add_table_copy(eu_prime_tb, node(), disc_copies)
	end;
poststart(_) ->
	ok.

prestop(_) ->
	ok.

poststop(mnesia) ->
	% Clear replica before leaving the cluster
	lists:foreach(fun(N) ->
					  case node() of
						  N -> ok;
						  _ -> rpc:call(N, mnesia, del_table_copy, [schema, node()])
					  end
				  end, nodes());
poststop(_) ->
	ok.

update({Key, Value}, []) ->
	[{Key, Value}];
update({Key, Value}, [{Key, _} | Rest]) ->
	[{Key, Value} | Rest];
update({Key, Value}, [H | Rest]) ->
	[H | update({Key, Value}, Rest)].
