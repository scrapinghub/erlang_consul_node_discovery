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
parse(Body) ->
    lists:map(
        fun(NodeMap) ->
            Key = maps:get(<<"Key">>, NodeMap),
            NodeShortName = hd(
                binary:split(lists:nth(2, binary:split(Key, <<"/">>)), <<"_">>)
            ),
            RawValue = base64:decode(maps:get(<<"Value">>, NodeMap)),
            Value = jiffy:decode(RawValue, [return_maps]),
            PortsList = maps:get(<<"ports">>, Value),
            Host = maps:get(<<"hostname">>, Value),
            NodeFullName = binary_to_atom(<<NodeShortName/binary, "@", Host/binary>>, latin1),
            {NodeFullName, PortsList}
        end,
        jiffy:decode(Body, [return_maps])
    ).
