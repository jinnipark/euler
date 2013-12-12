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
