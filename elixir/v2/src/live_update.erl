-module(live_update).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    live_update_sup:start_link().

stop(_State) ->
    ok.
