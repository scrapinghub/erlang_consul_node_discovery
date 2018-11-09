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
    FoldFun = fun(NodeMap, Acc) ->
        Key = maps:get(<<"Key">>, NodeMap),
        Value = base64:decode(maps:get(<<"Value">>, NodeMap)),
        case [parse_key(Key) | parse_value(Value)] of
            [nomatch, _, _] -> Acc;
            [Node, Host, Port] when is_binary(Node) andalso
                                    is_binary(Host) ->
                Name = binary_to_atom(<<Node/binary, "@", Host/binary>>, latin1),
                [{Name, Port}|Acc]
        end
    end,
    lists:foldl(FoldFun, [], jiffy:decode(Body, [return_maps])).
        
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
    Host = maps:get(<<"hostname">>, Value),
    Port = 
    case {maps:get(<<"ports">>, Value, []),
          maps:get(<<"namedports">>, Value, #{})} of
        {_, #{<<"dist">>:=P}} -> P;
        {[P], _} -> P;
        {Ports, _} when is_list(Ports) -> lists:last(Ports) 
    end,
    [Host, Port].

