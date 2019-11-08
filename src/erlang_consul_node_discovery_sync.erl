-module(erlang_consul_node_discovery_sync).

-export([
    apply/2
]).

apply(List, CallbackModule) ->
    Current = current(CallbackModule),
    Stale = Current -- List,
    Fresh = List -- Current,
    remove_nodes(Stale, CallbackModule),
    add_nodes(Fresh, CallbackModule).

current(CallbackModule) ->
    Raw = CallbackModule:list_nodes(),
    lists:usort(normalize(Raw)).

normalize(Nodes) when is_list(Nodes) ->
    lists:map(fun normalize/1, Nodes);
normalize({Node, _Ip, Port, Driver}) ->
    {Name, Host} = split_name(Node),
    [Name, Host, Port, Driver].

remove_nodes(Nodes, CallbackModule) ->
    lists:foreach(fun([Name, Host, _, _]) ->
        NodeName = concat(Name, Host),
        CallbackModule:remove_node(NodeName)
    end, Nodes).

add_nodes(Nodes, CallbackModule) ->
    lists:foreach(fun(Args) -> apply(CallbackModule, add_node, Args) end, Nodes).

split_name(Node) ->
    Bin = atom_to_binary(Node, latin1),
    [Name, Host] = binary:split(Bin, <<"@">>),
    {binary_to_atom(Name, latin1), binary_to_atom(Host, latin1)}.

concat(Name, Host) ->
    NameBin = atom_to_binary(Name, latin1),
    HostBin = atom_to_binary(Host, latin1),
    binary_to_atom(<< NameBin/binary, "@", HostBin/binary >>, latin1).
