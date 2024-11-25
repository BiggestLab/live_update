-module(counter).

-behaviour(gen_server).

% Public API
-export([increment/0, current_value/0]).
% Control
-export([start_link/0]).
% gen_server callbacks
-export([handle_call/3, handle_cast/2, init/1]).

%% Public API

increment() ->
    gen_server:call(get_pid(), {increment}).

current_value() ->
    gen_server:call(get_pid(), {current_value}).

%% Control implementation

start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    persistent_term:put(?MODULE, {ok, Pid}),
    {ok, Pid}.

%% Gen server

init(_Args) ->
    {ok, 0}.

handle_call({increment}, _From, State) ->
    {reply, ok, State + 1};
handle_call({current_value}, _From, State) ->
    {reply, State, State}.

handle_cast(_Args, _State) ->
    {noreply, {error, cast_not_supported}}.

%% Implementations

get_pid() ->
    {ok, Pid} = persistent_term:get(?MODULE, not_found),
    Pid.
