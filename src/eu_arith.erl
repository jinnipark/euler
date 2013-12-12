%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Arithmetic algorithms.
%%%
%%% Created : Dec 11, 2013
%%% -------------------------------------------------------------------
-module(eu_arith).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([factorial/1, lcm/2, gcd/2, divisors/1]).

%%
%% Calculate N!
%%
%% Params: pos_integer()
%% Returns: pos_integer()
%%
factorial(N) ->
	factorial(N, 1).

factorial(0, F) ->
	F;
factorial(N, F) ->
	factorial(N - 1, N * F).

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

%%
%% Get all the divisors of N.
%%
%% Params: pos_integer()
%% Returns: [pos_integer()]
%%
divisors(N) ->
	F = eu_prime:factorize(N), % [2,2,3]
	M = lists:map(fun(L) -> lists:seq(0, erlang:length(L)) end, eu_set:group(F)), % [[0,1,2],[0,1]]
	U = eu_set:unique(F), % [2,3]
	lists:sort(
		lists:map(
			fun(E) -> % [[0,0],[0,1],[1,0],...]
				lists:foldl(
					fun({Factor, Exp}, Acc) -> % [{2,0},{3,0}]
						power(Factor, Exp) * Acc
					end, 1, lists:zip(U, E))
			end, eu_set:combinations(M))).

power(_N, 0) ->
	1;
power(N, Exp) ->
	N * power(N, Exp-1).
