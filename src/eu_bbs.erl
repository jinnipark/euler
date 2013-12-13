%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Box-ball system
%%%
%%% Created : Dec 13, 2013
%%% -------------------------------------------------------------------
-module(eu_bbs).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([stable/1, evolve/1, balls/1, expand/1, compact/1]).

%%
%% Find the final ball configuration.
%%
%% Params: list() - compact representation
%% Returns: list()
%%
stable(C) ->
	B = balls(C),
	E = expand(C),
	E1 = evolve(E),
	C1 = compact(E1),
	case lists:sort(balls(C1)) of
		B -> B;
		_ -> stable(C1)
	end.

%%
%% Evolve box-ball configuration by one generation.
%%
%% Params: list() - expanded representation
%% Returns: list()
%%
evolve(P) ->
	evolve(P, 1, P).

evolve(P, N, C) ->
	case N > erlang:length(P) of
		true ->
			C;
		false ->
			case lists:nth(N, P) of
				0 ->
					evolve(P, N + 1, C);
				1 ->
					C1 = take_ball(N, C),
					C2 = put_ball(N + 1, C1),
					evolve(P, N + 1, C2)
			end
	end.

take_ball(N, C) ->
	{C1, [_ | C2]} = lists:split(N - 1, C),
	C1 ++ [0 | C2].

put_ball(N, C) when N > erlang:length(C) ->
	C ++ [1];
put_ball(N, C) ->
	case lists:nth(N, C) of
		0 ->
			{C1, [_ | C2]} = lists:split(N - 1, C),
			C1 ++ [1 | C2];
		1 ->
			put_ball(N + 1, C)
	end.

%%
%% Get a ball configuration from a box-ball configuration.
%%
%% Params: list() - compact representation
%% Returns: list()
%%
balls(C) ->
	balls(C, [], true).

balls([], B, _) ->
	B;
balls([H | C], B, true) ->
	balls(C, B ++ case H of
					  0 -> [];
					  _ -> [H]
				  end, false);
balls([_ | C], B, false) ->
	balls(C, B, true).

%%
%% Change from compact to expanded representation.
%%
%% Params: list()
%% Returns: list()
%%
expand(C) ->
	expand(C, [], true).

expand([], E, _) ->
	E;
expand([B | C], E, Ball) ->
	expand(C, E ++ lists:map(
					   case Ball of
						   true -> fun(_) -> 1 end;
						   false -> fun(_) -> 0 end
					   end, lists:seq(1, B)), not Ball).

%%
%% Change from expanded to compact representation.
%%
%% Params: list()
%% Returns: list()
%%
compact(E) ->
	compact(E, [], {0, true}).

compact([], C, {N, Ball}) ->
	case Ball of
		true -> C ++ [N];
		false -> C
	end;
compact([B | E], C, {N, Ball}) ->
	case (B == 1) == Ball of
		true ->
			compact(E, C, {N + 1, Ball});
		false ->
			compact(E, C ++ [N], {1, not Ball})
	end.
