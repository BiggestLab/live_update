-module(live_update_sup).

-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link(?MODULE, {}).

init({}) ->
    {ok,
     {{one_for_all, 6, 3600},
      [{counter, {counter, start_link, []}, permanent, 5000, worker, [counter]}]}}.
