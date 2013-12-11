%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Arithmetic algorithms.
%%%
%%% Created : Dec 11, 2013
%%% -------------------------------------------------------------------
-module(eu_arith).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([lcm/2, gcd/2]).

%%
%% Calculate the least common multiple of N1 and N2.
%%
%% Params: pos_integer(), pos_integer()
%% Returns: pos_integer()
%%
lcm(N1, N2) ->
	lists:foldl(fun(F, Acc) -> F * Acc end, 1,
			eu_set:union(eu_prime:factorize(N1), eu_prime:factorize(N2))).

%%
%% Calculate the greatest common divisor of N1 and N2.
%%
%% Params: pos_integer(), pos_integer()
%% Returns: pos_integer()
%%
gcd(N1, N2) ->
	lists:foldl(fun(F, Acc) -> F * Acc end, 1,
			eu_set:intersection(eu_prime:factorize(N1), eu_prime:factorize(N2))).
