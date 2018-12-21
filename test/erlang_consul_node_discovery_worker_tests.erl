-module(erlang_consul_node_discovery_worker_tests).
-include_lib("eunit/include/eunit.hrl").

erlang_consul_node_discovery_test_() ->
    {
        foreach,
        fun() ->
            meck:new(erlang_node_discovery, [non_strict]),
            meck:expect(erlang_node_discovery, add_node, fun(_, _, _) -> ok end),
            meck:expect(erlang_node_discovery, add_node, fun(_, _, _, _) -> ok end),
            meck:expect(httpc, request, fun(_, _, _, _) -> {error, {no_connection, mocked}} end),
            application:load(erlang_consul_node_discovery),
            application:set_env(erlang_consul_node_discovery, consul_url, "http://127.0.0.1:8000/"),
            application:set_env(erlang_consul_node_discovery, poll_interval, 60000),
            application:set_env(erlang_consul_node_discovery, discovery_callback, erlang_node_discovery),
            application:set_env(erlang_consul_node_discovery, port_names, [{<<"dist">>, inet_tcp}]),

            {ok, WorkerPid} = erlang_consul_node_discovery_worker:start_link(),
            unlink(WorkerPid)
        end,

        fun(_) ->
             case whereis(erlang_consul_node_discovery_worker) of
                    undefined ->
                        ok;
                    P ->
                        erlang:exit(P, kill),
                        erlang:monitor(process, P),
                        receive {'DOWN', _, _, P, _} -> ok end
             end,
             meck:unload()
        end,
        [
            {"Nodes info stored in State of worker process", fun() ->
                Self = self(),
                TempEts = ets:new(temp, [public, bag]),
                meck:expect(erlang_node_discovery, add_node, fun(Node, Host, Port) ->
                    Self ! {adding_node, Node, Host, Port},
                    ets:insert(TempEts, {Node, Host, Port})
                end),
                meck:expect(erlang_node_discovery, add_node, fun(Node, Host, Port, Driver) ->
                    Self ! {adding_node, Node, Host, Port, Driver},
                    ets:insert(TempEts, {Node, Host, Port, Driver})
                end),
                Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [1,2]},
                Val2 = #{<<"hostname">> => <<"h2">>, <<"ports">> => [3,4],
                         <<"namedports">> => #{<<"dist">> => 5, <<"eless_tcp">> => 6}},
                Body = jiffy:encode(
                         [#{<<"Key">>   => <<"upstreams/n-1_node/xxx">>,
                            <<"Value">> => encode_value(Val1)},
                          #{<<"Key">>   => <<"upstreams/n-2_node/xxx">>,
                            <<"Value">> => encode_value(Val2)}]),
                meck:expect(httpc, request,
                    fun(_, _, _, _) -> {ok, {200, [], Body}} end
                ),
                %% dbg:tracer(),
                %% dbg:p(all,c),
                %% dbg:tpl(erlang_consul_node_discovery_response_parser,
                %%         [{'_',[],[{return_trace}]}]),
                %% ?debugVal(jiffy:decode(Body, [return_maps])),
                %% ?debugVal(erlang_consul_node_discovery_response_parser:parse(Body)),
                %% dbg:stop_clear(),

                P = whereis(erlang_consul_node_discovery_worker),
                P ! poll_consul,

                ?assertEqual(
                    lists:sort([{adding_node, 'n-1', h1, 2},
                                {adding_node, 'n-2', h2, 5, inet_tcp},
                                {adding_node, 'n-2', h2, 6, eless_tcp}]),
                    lists:sort(wait_for_messages(3, []))
                ),
                ?assertEqual(
                    lists:sort([{'n-1', h1 ,2},
                                {'n-2', h2, 5, inet_tcp},
                                {'n-2', h2, 6, eless_tcp}]),
                    lists:sort(ets:tab2list(TempEts))
                )
            end},
            {"Messages about new nodes are sent to discovery", fun() ->
                Self = self(),
                TempEts = ets:new(temp, [public, bag]),
                meck:expect(erlang_node_discovery, add_node, fun(Node, Host, Port) ->
                    Self ! {adding_node, Node, Host, Port},
                    ets:insert(TempEts, {Node, {Host, Port}})
                end),
                meck:expect(httpc, request,
                    fun(_, _, _, _) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [1,2]},
                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"upstreams/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, {200, [], Body}}
                    end
                ),

                Pid = whereis(erlang_consul_node_discovery_worker),
                Pid ! poll_consul,

                ?assertEqual(
                    [{adding_node, 'n-1', h1, 2}],
                    wait_for_messages(1, [])
                )
            end},
            {"Messages about node updates (e.g. new port) are sent to discovery", fun() ->
                Self = self(),
                TempEts = ets:new(temp, [public, bag]),
                meck:expect(erlang_node_discovery, add_node, fun(Node, Host, Port) ->
                    Self ! {adding_node, Node, Host, Port},
                    ets:insert(TempEts, {Node, {Host, Port}})
                end),
                meck:expect(httpc, request,
                    fun(_, _, _, _) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [1,2]},
                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"upstreams/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, {200, [], Body}}
                    end
                ),


                Pid = whereis(erlang_consul_node_discovery_worker),
                Pid ! poll_consul,

                [_] = wait_for_messages(1, []),

                meck:expect(httpc, request,
                    fun(_, _, _, _) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [4]},
                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"upstreams/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, {200, [], Body}}
                    end
                ),
                Pid ! poll_consul,
                ?assertEqual(
                    [{adding_node, 'n-1', h1, 4}],
                    wait_for_messages(1, [])
                )
            end}
        ]
    }.

-spec encode_value(Value) -> Result when
    Value  :: binary(),
    Result :: binary().

encode_value(Value) ->
    JSValue = jiffy:encode(Value),
    base64:encode(JSValue).


-spec wait_for_messages(Num, Acc) -> Messages when
    Num      :: non_neg_integer(),
    Acc      :: list(),
    Messages :: list().
wait_for_messages(0, MsgAcc) ->
    lists:reverse(MsgAcc);
wait_for_messages(Num, MsgAcc) ->
    Message = receive
        M -> M
    after 100 ->
        timeout
    end,
    wait_for_messages(Num - 1, [Message|MsgAcc]).
