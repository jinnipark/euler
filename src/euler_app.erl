%%% -------------------------------------------------------------------
%%% Author  : Sungjin Park <jinni.park@gmail.com>
%%%
%%% Description : Euler application callback
%%%
%%% Created : Dec 10, 2013
%%% -------------------------------------------------------------------
-module(euler_app).
-author("Sungjin Park <jinni.park@gmail.com>").
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    euler_sup:start_link().

stop(_State) ->
    ok.
