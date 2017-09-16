-module(erlang_consul_node_discovery_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	ConsulWorker =  #{
		id => erlang_consul_node_discovery_worker,
        start => {erlang_consul_node_discovery_worker, start_link, []},
        restart => permanent,
        shutdown => 50000,
        type => worker
    },
    Procs = [ConsulWorker],
    {ok, {{one_for_one, 1, 5}, Procs}}.
