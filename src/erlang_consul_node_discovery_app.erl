-module(erlang_consul_node_discovery_app).
-behaviour(application).

-export([start/0, start/2]).
-export([stop/1]).


start() ->
    {ok, _} = application:ensure_all_started(erlang_consul_node_discovery),
    ok.


start(_Type, _Args) ->
    erlang_consul_node_discovery_sup:start_link().

stop(_State) ->
    ok.
