%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Set operations.
%%%
%%% Created : Dec 11, 2013
%%% -------------------------------------------------------------------
-module(eu_set).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([union/2, intersection/2]).

%%
%% Calculate a union of set S1 and S2.
%%
%% Params: list(), list() - sorted
%% Returns: list() - sorted list
%%
union(S1, S2) ->
	union(S1, S2, []).

union([], S2, U) ->
	U ++ S2;
union(S1, [], U) ->
	U ++ S1;
union([E | S1], [E | S2], U) ->
	union(S1, S2, U ++ [E]);
union([E1 | S1], [E2 | S2], U) ->
	case E1 < E2 of
		true -> union(S1, [E2 | S2], U ++ [E1]);
		false -> union([E1 | S1], S2, U ++ [E2])
	end.

%%
%% Calculate an intersection of set S1 and S2.
%%
%% Params: list(), list() - sorted
%% Returns: list() - sorted
%%
intersection(S1, S2) ->
	intersection(S1, S2, []).

intersection([], _S2, I) ->
	I;
intersection(_S1, [], I) ->
	I;
intersection([E | S1], [E | S2], U) ->
	intersection(S1, S2, U ++ [E]);
intersection([E1 | S1], [E2 | S2], U) ->
	case E1 < E2 of
		true -> intersection(S1, [E2 | S2], U);
		false -> intersection([E1 | S1], S2, U)
	end.
