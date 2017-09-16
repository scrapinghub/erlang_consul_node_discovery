-module(erlang_consul_node_discovery_worker_tests).
-include_lib("eunit/include/eunit.hrl").

erlang_consul_node_discovery_test_() ->
    {
        foreach,
        fun() ->
            meck:expect(
                hackney, get,
                fun(_) ->
                    {ok, 200, [], some_ref}
                end
            ),
            application:load(erlang_consul_node_discovery),
            application:set_env(erlang_consul_node_discovery, consul_url, "http://127.0.0.1:8000/"),
            application:set_env(erlang_consul_node_discovery, pull_interval, 3000),

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
            end
        end,
        [
            {"Nodes info stored in State of worker process", fun() ->
                MyPid = self(),
                meck:expect(
                    hackney, body,
                    fun(_) ->
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
                        {ok, Body}
                    end
                ),
                meck:expect(
                    erlang_node_discovery, add_node,
                    fun(Node, Port) -> MyPid ! {Node, Port} end
                ),

                % erlang_consul_node_discovery_worker:nodes_info()
                P = whereis(erlang_consul_node_discovery_worker),
                P ! pull_consul,

                ?assertEqual(
                    lists:sort([{'n-1@h1',[1,2]},{'n-2@h2',[3,4]}]),
                    lists:sort(erlang_consul_node_discovery_worker:nodes_info())
                )
            end},
            {"Messages about node updates are sent to discovery", fun() ->
                MyPid = self(),
                meck:expect(
                    hackney, body,
                    fun(_) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [1,2]},

                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"xxx/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, Body}
                    end
                ),

                meck:expect(
                    erlang_node_discovery, add_node,
                    fun(Node, Port) -> MyPid ! {Node, Port} end
                ),
                meck:expect(erlang_node_discovery, remove_node, fun(Node) -> Node end),

                % erlang_consul_node_discovery_worker:nodes_info()
                Pid = whereis(erlang_consul_node_discovery_worker),
                Pid ! pull_consul,

                Messages = wait_for_messages(2, []),
                ?assertEqual(
                    lists:sort(Messages),
                    lists:sort([{'n-1@h1', 1}, {'n-1@h1', 2}])
                )
            end},
            {"Messages about node updates are sent to discovery", fun() ->
                MyPid = self(),
                meck:expect(
                    hackney, body,
                    fun(_) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [1,2]},

                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"xxx/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, Body}
                    end
                ),

                meck:expect(
                    erlang_node_discovery, add_node,
                    fun(Node, Port) -> MyPid ! {adding_node, Node, Port} end
                ),
                meck:expect(erlang_node_discovery, remove_node, fun(Node) -> MyPid ! {removing_node, Node} end),

                Pid = whereis(erlang_consul_node_discovery_worker),
                Pid ! pull_consul,

                _ = wait_for_messages(2, []),

                meck:expect(
                    hackney, body,
                    fun(_) ->
                        Val1 = #{<<"hostname">> => <<"h1">>, <<"ports">> => [4]},

                        Body = jiffy:encode([
                            #{
                                <<"Key">>   => <<"xxx/n-1_node/xxx">>,
                                <<"Value">> => encode_value(Val1)
                            }
                        ]),
                        {ok, Body}
                    end
                ),
                Pid ! pull_consul,


                [FirstMsg|Tail] = wait_for_messages(2, []),
                ?assertEqual({removing_node, 'n-1@h1'}, FirstMsg),
                ?assertEqual({adding_node, 'n-1@h1', 4}, hd(Tail))
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
    end,
    wait_for_messages(Num - 1, [Message|MsgAcc]).