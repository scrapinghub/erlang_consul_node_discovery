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
        NodeShortName = hd(
            binary:split(lists:nth(2, binary:split(Key, <<"/">>)), <<"_">>)
        ),
        RawValue = base64:decode(maps:get(<<"Value">>, NodeMap)),
        Value = jiffy:decode(RawValue, [return_maps]),
        Host = maps:get(<<"hostname">>, Value),
        case maps:get(<<"ports">>, Value, []) of
            Ports when is_list(Ports) andalso length(Ports) > 0 ->
                %% Taking last one from list
                %% it shouldn't be like that
                %% consul should return labeled ports
                %% currently order of ports is vital for operation via consul discovery
                Port = lists:last(Ports),
                NodeFullName = binary_to_atom(<<NodeShortName/binary, "@", Host/binary>>, latin1),
                [{NodeFullName, Port}|Acc];
            _ ->
                Acc
        end
    end,
    lists:foldl(FoldFun, [], jiffy:decode(Body, [return_maps])).
