-module(erlang_consul_node_discovery_worker_tests).
-include_lib("eunit/include/eunit.hrl").

erlang_consul_node_discovery_test_() ->
    {
        foreach,
        fun() ->
            meck:new(erlang_node_discovery, [non_strict]),
            meck:expect(erlang_node_discovery, list_nodes, fun() -> [] end),
            meck:expect(erlang_node_discovery, add_node, fun(_, _) -> ok end),
            meck:expect(httpc, request, fun(_, _, _, _) -> {error, {no_connection, mocked}} end),
            application:load(erlang_consul_node_discovery),
            application:set_env(erlang_consul_node_discovery, consul_url, "http://127.0.0.1:8000/"),
            application:set_env(erlang_consul_node_discovery, poll_interval, 60000),
            application:set_env(erlang_consul_node_discovery, discovery_callback, erlang_node_discovery),

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
                TempEts = ets:new(temp, [public]),
                meck:expect(erlang_node_discovery, add_node, fun(Node, Port) ->
                    Self ! {adding_node, Node, Port},
                    ets:insert(TempEts, {Node, {host, Port}})
                end),
                meck:expect(erlang_node_discovery, remove_node, fun(Node) ->
                    Self ! {removing_node, Node},
                    ets:delete(TempEts, Node)
                end),
                meck:expect(erlang_node_discovery, list_nodes, fun() ->
                    lists:sort(ets:tab2list(TempEts))
                end),
                meck:expect(httpc, request,
                    fun(_, _, _, _) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [1,2]},
                        Val2 = #{<<"hostname">> => <<"h2">>, <<"ports">> => [3,4]},

                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"xxx/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            },
                            #{
                                <<"Key">>   => <<"xxx/n-2_node/xxx">>,
                                <<"Value">> => encode_value(Val2)
                            }
                        ]),
                        {ok, {200, [], Body}}
                    end
                ),

                % erlang_consul_node_discovery_worker:nodes_info()
                P = whereis(erlang_consul_node_discovery_worker),
                P ! poll_consul,

                ?assertEqual(
                    lists:sort([{'n-1@h1' ,2}, {'n-2@h2', 4}]),
                    lists:sort(erlang_consul_node_discovery_worker:nodes_info())
                )
            end},
            {"Messages about new nodes are sent to discovery", fun() ->
                Self = self(),
                TempEts = ets:new(temp, [public, bag]),
                meck:expect(erlang_node_discovery, add_node, fun(Node, Port) ->
                    Self ! {adding_node, Node, Port},
                    ets:insert(TempEts, {Node, {host, Port}})
                end),
                meck:expect(erlang_node_discovery, remove_node, fun(Node) ->
                    Self ! {removing_node, Node},
                    ets:delete(TempEts, Node)
                end),
                meck:expect(erlang_node_discovery, list_nodes, fun() ->
                    lists:sort(ets:tab2list(TempEts))
                end),
                meck:expect(httpc, request,
                    fun(_, _, _, _) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [1,2]},

                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"xxx/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, {200, [], Body}}
                    end
                ),

                % erlang_consul_node_discovery_worker:nodes_info()
                Pid = whereis(erlang_consul_node_discovery_worker),
                Pid ! poll_consul,

                ?assertEqual(
                    lists:sort([{adding_node, 'n-1@h1', 2}]),
                    wait_for_messages(1, [])
                )
            end},
            {"Messages about node updates (e.g. new port) are sent to discovery", fun() ->
                Self = self(),
                TempEts = ets:new(temp, [public, bag]),
                meck:expect(erlang_node_discovery, add_node, fun(Node, Port) ->
                    Self ! {adding_node, Node, Port},
                    ets:insert(TempEts, {Node, {host, Port}})
                end),
                meck:expect(erlang_node_discovery, remove_node, fun(Node) ->
                    Self ! {removing_node, Node},
                    ets:delete(TempEts, Node)
                end),
                meck:expect(erlang_node_discovery, list_nodes, fun() ->
                    lists:sort(ets:tab2list(TempEts))
                end),
                meck:expect(httpc, request,
                    fun(_, _, _, _) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [1,2]},

                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"xxx/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, {200, [], Body}}
                    end
                ),


                Pid = whereis(erlang_consul_node_discovery_worker),
                Pid ! poll_consul,

                _ = wait_for_messages(3, []),

                meck:expect(httpc, request,
                    fun(_, _, _, _) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [4]},

                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"xxx/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, {200, [], Body}}
                    end
                ),
                Pid ! poll_consul,
                ?assertEqual(
                    [{removing_node, 'n-1@h1'}, {adding_node, 'n-1@h1', 4}, timeout],
                    wait_for_messages(3, [])
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
