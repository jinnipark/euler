%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Set operations.
%%%
%%% Created : Dec 11, 2013
%%% -------------------------------------------------------------------
-module(eu_set).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([union/2, intersection/2, unique/1, group/1, combinations/1, combine/2]).

%%
%% Calculate a union of set S1 and S2.
%%
%% Params: list(), list()
%% Returns: list()
%%
union(S1, S2) ->
	union(S1, S2, []).

union([], S2, U) ->
	U ++ S2;
union([E | S1], S2, U) ->
	{_, S3} = take(E, S2),
	union(S1, S3, U ++ [E]).

take(E, L) ->
	take(E, L, {undefined, []}).

take(_E, [], Result) ->
	Result;
take(E, [E | L], {undefined, T}) ->
	{E, T ++ L};
take(E, [E1 | L], {undefined, T}) ->
	take(E, L, {undefined, T ++ [E1]}).

%%
%% Calculate an intersection of set S1 and S2.
%%
%% Params: list(), list()
%% Returns: list()
%%
intersection(S1, S2) ->
	intersection(S1, S2, []).

intersection([], _S2, I) ->
	I;
intersection([E | S1], S2, I) ->
	case take(E, S2) of
		{undefined, S3} -> intersection(S1, S3, I);
		{E, S3} -> intersection(S1, S3, I ++ [E])
	end.

%%
%% Make every element in set S unique.
%%
%% Params: list()
%% Returns: list()
%%
unique(S) ->
	unique(S, []).

unique([], U) ->
	U;
unique([E | S], U) ->
	case lists:member(E, U) of
		true -> unique(S, U);
		false -> unique(S, U ++ [E])
	end.

%%
%% Group the same elements in a set.
%%
%% Params: list()
%% Returns: [list()]
%%
group(S) ->
	group(S, []).

group([], G) ->
	G;
group([E | S], G) ->
	group(S, merge(E, G)).

merge(E, []) ->
	[[E]];
merge(E, [L | G]) ->
	case lists:member(E, L) of
		true -> [[E | L] | G];
		false -> [L | merge(E, G)]
	end.

%%
%% Generate all possible combinations in a group set.
%%
%% Params: [list()]
%% Returns: [list()]
%%
combinations([]) ->
	[];
combinations([H | T]) ->
	combine(H, combinations(T)).

%%
%% Combine two sets.
%%
%% Params: list(), list()
%% Returns: [list()]
%%
combine(A, B) ->
	combine(A, B, []).

combine(A, [], Z) ->
	Z ++ lists:map(fun(E) -> [E] end, A);
combine([], _B, Z) ->
	Z;
combine([H | A], B, Z) ->
	combine(A, B, Z ++ lists:map(fun(E) -> [H | E] end, B)).
