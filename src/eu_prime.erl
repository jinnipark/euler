%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Prime number calculator
%%%
%%% Created : Dec 10, 2013
%%% -------------------------------------------------------------------
-module(eu_prime).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([complete/1]).

-include("eu_prime_tb.hrl").

%%
%% Complete prime number table up to N.
%%
%% Params: N
%%
complete(N) ->
	{atomic, ok} = mnesia:transaction(fun complete_trx/1, [N], 1).

complete_trx(N) ->
	case mnesia:select(eu_prime_tb,
			 [{#eu_prime_tb{value='$1'},
			   [{'>=', '$1', N}], ['$$']}], 1, read) of
		{Primes, _Cont} -> % Table already complete.
			lager:debug("already complete ~w", [Primes]),
			ok;
		'$end_of_table' ->
			Min = case mnesia:last(eu_prime_tb) of
					  '$end_of_table' ->
					  	  lager:debug("filling the first prime 2"),
						  mnesia:write(#eu_prime_tb{value=2, is_prime=true}),
						  3;
					  2 ->
					  	  3;
					  P when P rem 2 == 0 ->
					  	  P + 1;
					  P ->
					  	  P + 2
				  end,
			fill_trx(Min, N),
			update_boundary(mnesia:select(eu_prime_tb,
								[{#eu_prime_tb{value='$1', is_prime='$2'},
								 [{'or', {'==', '$2', false}, {'==', '$1', N}}],
								 ['$$']}]), N)
	end.

fill_trx(Min, Max) when Min > Max ->
	lager:debug("filling complete ~p", [Min]),
	ok;
fill_trx(Min, Max) ->
	case test_trx(Min, mnesia:first(eu_prime_tb)) of
		true ->
			lager:debug("filling ~p", [Min]),
			mnesia:write(#eu_prime_tb{value=Min, is_prime=true});
		false ->
			lager:debug("skipping ~p", [Min]),
			ok
	end,
	fill_trx(Min + 2, Max). % Min is always odd.

test_trx(_, NaN) when not erlang:is_number(NaN) -> % No more prime to test
	true;
test_trx(N, P) ->
	case N rem P of
		0 -> false;
		_ -> test_trx(N, mnesia:next(eu_prime_tb, P))
	end.

update_boundary(Nothing, N) when (not erlang:is_list(Nothing)) or (Nothing == []) ->
	lager:debug("writing boundary ~p", [N]),
	mnesia:write(#eu_prime_tb{value=N, is_prime=false});
update_boundary([[N, true]], N) -> % Eventually, new boundary is a prime
	ok;
update_boundary([[K, false] | Boundaries], N) -> % Found previous boundary
	lager:debug("deleting boundary ~p", [K]),
	mnesia:delete({eu_prime_tb, K}),
	update_boundary(Boundaries, N).
