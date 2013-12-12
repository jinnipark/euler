%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Prime number calculator
%%%
%%% Created : Dec 10, 2013
%%% -------------------------------------------------------------------
-module(eu_prime).
-author("Sungjin Park <jinni.park@gmail.com>").

-export([complete/1, check/1, factorize/1]).

-include("eu_prime_tb.hrl").

%%
%% Complete prime number table up to N.
%%
%% Params: pos_integer()
%%
complete(N) ->
	mnesia:async_dirty(fun complete_trx/1, [N]).

complete_trx(N) ->
	case mnesia:select(eu_prime_tb,
			 [{#eu_prime_tb{value='$1'},
			   [{'>=', '$1', N}], ['$$']}], 1, read) of
		{_, _Cont} -> % Table already complete.
			ok;
		'$end_of_table' -> % Table not complete yet.
			Min = case mnesia:last(eu_prime_tb) of
					  '$end_of_table' ->
					  	  lager:debug("2 is the first prime"),
						  mnesia:write(#eu_prime_tb{value = 2, is_prime = true}),
						  3;
					  2 -> % The first known prime.
					  	  3;
					  B when B rem 2 == 0 -> % Cannot be a prime number.
					  	  lager:debug("delete old boundary ~p", [B]),
					  	  mnesia:delete({eu_prime_tb, B}),
					  	  B + 1;
					  P ->
					  	  [#eu_prime_tb{is_prime = IsPrime}] = mnesia:read(eu_prime_tb, P),
					  	  case IsPrime of
					  	  	  false ->
					  	  	  	  lager:debug("delete old boundary ~p", [P]),
					  	  	  	  mnesia:delete({eu_prime_tb, P});
					  	  	  _ ->
					  	  	  	  ok
					  	  end,
					  	  P + 2
				  end,
			fill_trx(Min, N),
			case mnesia:read(eu_prime_tb, N) of
				[] ->
					lager:debug("new boundary ~p", [N]),
					mnesia:write(#eu_prime_tb{value = N, is_prime = false});
				_ ->
					ok
			end
	end.

fill_trx(Min, Max) when Min > Max ->
	ok;
fill_trx(Min, Max) ->
	case test_trx(Min, mnesia:first(eu_prime_tb)) of
		true ->
			lager:debug("~p is prime", [Min]),
			mnesia:write(#eu_prime_tb{value = Min, is_prime = true});
		false ->
			lager:debug("~p is not prime", [Min]),
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

%%
%% Check if N is a prime number or not.
%%
%% Params: pos_integer()
%% Returns: boolean()
%%
check(N) ->
	mnesia:async_dirty(fun check_trx/1, [N]).

check_trx(N) ->
	ok = complete_trx(N),
	case mnesia:select(eu_prime_tb,
			 [{#eu_prime_tb{value='$1', is_prime='$2'},
			   [{'>=', '$1', N}], ['$$']}], 1, read) of
		{[[N, IsPrime]], _Cont} -> % N is found
			IsPrime;
		{_, _Cont} -> % Something bigger found
			false
	end.

%%
%% Do prime factorization.
%%
%% Params: pos_integer()
%% Returns: [pos_integer()]
%%
factorize(N) ->
	mnesia:async_dirty(fun factorize_trx/2, [N, []]).

factorize_trx(N, Factors) ->
	case check_trx(N) of
		true -> Factors ++ [N];
		false -> factorize_trx(N, Factors, mnesia:first(eu_prime_tb))
	end.

factorize_trx(N, Factors, P) when erlang:is_number(P) ->
	case check_trx(N) of
		true ->
			Factors ++ [N];
		false ->
			case N rem P of
				0 -> factorize_trx(N div P, Factors ++ [P], P);
				_ -> factorize_trx(N, Factors,mnesia:next(eu_prime_tb, P))
			end
	end;
factorize_trx(N, Factors, _NaN) ->
	Factors ++ [N]. 
