-module(erlang_consul_node_discovery_sync_tests).
-include_lib("eunit/include/eunit.hrl").

-behaviour (erlang_consul_node_discovery_worker).
-export([
    add_node/3,
    add_node/4,
    remove_node/1,
    list_nodes/0
]).

-define(IP, {127, 0, 0, 1}).

-define(CONSUL_DATA, [
    [batman       , 'gotham.dc1.scrapinghub.com'    , 17117, inet_tcp],
    ['green-arrow', 'starcity.dc1.scrapinghub.com'  , 17113, inet_tcp],
    [superman     , 'metropolis.dc1.scrapinghub.com', 17114, inet_tcp]
]).

-define(INTERNAL_DATA, [
    {'batman@gotham.dc1.scrapinghub.com',            ?IP, 17115, inet_tcp},
    {'green-arrow@starcity.dc1.scrapinghub.com',     ?IP, 17113, inet_tcp},
    {'superman@smallville.dc1.scrapinghub.com',      ?IP, 17114, inet_tcp},
    {'green-lantern@coast-city.dc1.scrapinghub.com', ?IP, 17119, inet_tcp}
]).

-define(TAB, ?MODULE).

apply_test() ->
    init_callback_module(),
    erlang_consul_node_discovery_sync:apply(?CONSUL_DATA, ?MODULE),
    ?assertEqual([
        {'batman@gotham.dc1.scrapinghub.com',        ?IP, 17117, inet_tcp},
        {'green-arrow@starcity.dc1.scrapinghub.com', ?IP, 17113, inet_tcp},
        {'superman@metropolis.dc1.scrapinghub.com',  ?IP, 17114, inet_tcp}
    ], list_nodes()).

init_callback_module() ->
    ets:new(?TAB, [public, named_table]),
    ets:insert(?TAB, ?INTERNAL_DATA).

add_node(Node, Host, Port) ->
    add_node(Node, Host, Port, default).

add_node(Node, Host, Port, Driver) ->
    NodeBin = atom_to_binary(Node, latin1),
    HostBin = atom_to_binary(Host, latin1),
    Key = binary_to_atom(<<NodeBin/binary, "@", HostBin/binary>>, latin1),
    ets:insert(?TAB, {Key, ?IP, Port, Driver}).

remove_node(NodeName) ->
    ets:delete(?TAB, NodeName).

list_nodes() ->
    lists:usort(ets:tab2list(?TAB)).
