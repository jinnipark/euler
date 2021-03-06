%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Remote control for euler. 
%%%
%%% Created : Dec 10, 2013
%%% -------------------------------------------------------------------
-module(euler_control).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([call/1]).

-define(APPLICATION, euler).

call([Node, state]) ->
	case catch rpc:call(Node, ?APPLICATION, state, []) of
		{ok, State} ->
			io:format("~n== ~p running ==~n", [Node]),
			pretty_print(State);
		Error ->
			io:format("~n== ~p error ==~n~p~n~n", [Node, Error])
	end,
	erlang:halt();
call([Node, stop]) ->
	io:format("~n== ~p stopping ==~n", [Node]),
	io:format("~p~n~n", [catch rpc:call(Node, ?APPLICATION, stop, [])]),
	erlang:halt().

%%
%% Local functions
%%
pretty_print([]) ->
	io:format("~n");
pretty_print([{Key, Value} | More]) ->
	io:format("~12s : ~p~n", [Key, Value]),
	pretty_print(More).