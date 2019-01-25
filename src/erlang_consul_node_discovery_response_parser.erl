-module(erlang_consul_node_discovery_response_parser).

-export ([parse/1]).

-spec parse(Body) -> Result when
    Body   :: binary(),
    Result :: [{Nodename, PortsList}],
    Nodename :: atom(),
    PortsList :: [inet:port_number()].
%
% In our example the data is provided by Consul in the following format:
%
% Key: <<"xx/node-id_node/xxxx">>
% Value: #{<<"hostname">> => <<"hostname.com">>,
%          <<"port">> => xxxx,
%          <<"ports">> => [xxx,yyy,zzz]}
% And we're extracting the data in following data:
% [{node-id@hostname, [Ports]}]
parse("") -> [];
parse(<<"">>) -> [];
parse(Body) ->
    lists:foldl(fun parse_node_data/2, [], jiffy:decode(Body, [return_maps])).

parse_node_data(NodeMap, Acc) ->
    case parse_key(maps:get(<<"Key">>, NodeMap)) of
        nomatch -> Acc;
        Node when is_binary(Node) ->
            NodeAtom = binary_to_atom(Node, latin1),
            lists:foldl(
              fun(HostPort, IntAcc) -> [[NodeAtom |HostPort] |IntAcc] end,
              Acc,
              parse_value(base64:decode(maps:get(<<"Value">>, NodeMap))))
    end.

parse_key(Key) ->
    case binary:split(Key, [<<"/">>,<<"_">>], [global]) of
        [<<"upstreams">>,Node,<<"node">>|_] -> Node;
        [<<"upstreams">>,Node,<<"main">>|_] -> Node;
        [<<"upstreams">>,Node, Id|_] ->
            case re:run(Id, "\\d+") of
                {match, _} -> <<Node/binary, Id/binary>>;
                nomatch -> nomatch
            end;
        _ -> nomatch
    end.

parse_value(RawValue) ->
    Value = jiffy:decode(RawValue, [return_maps]),
    Host = binary_to_atom(maps:get(<<"hostname">>, Value), latin1),
    PortList =
    case {maps:get(<<"ports">>, Value, []),
          maps:get(<<"namedports">>, Value, #{})} of
        {_, Ports} when is_map(Ports) andalso map_size(Ports) > 0 ->
            maps:fold(fun parse_named_ports/3, [], Ports);
        {Ports, _} when is_list(Ports) ->
            [[lists:last(Ports)]]
    end,
    [[Host |Port] || Port <- PortList].

parse_named_ports(Name, Port, Acc) ->
    PortNames = application:get_env(erlang_consul_node_discovery, port_names, []),
    NameAtom = binary_to_atom(Name, latin1),
    case proplists:get_value(NameAtom, PortNames,
             proplists:get_value(Name, PortNames, false)) of
        false -> Acc;
        true -> [[Port, NameAtom] |Acc];
        Driver when is_atom(Driver) -> [[Port, Driver] |Acc]
    end.
