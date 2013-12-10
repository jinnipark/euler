%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Supervisor callback
%%%
%%% Created : Dec 10, 2013
%%% -------------------------------------------------------------------
-module(euler_sup).
-author("Sungjin Park <jinni.park@gmail.com>").
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%
%% Start top level supervisor
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.
