-module(erlang_consul_node_discovery_worker).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([nodes_info/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type nodename()     :: binary().
-type hostname() :: binary().


-record(state, {
    consul_url = undefined    :: binary(),
    pull_interval = 60 * 1000 :: pos_integer(),
    timer_ref                 :: reference(),
    nodes                     :: [{nodename(), hostname(), inets:port()}]
}).

%% API.

nodes_info() ->
    gen_server:call(?MODULE, nodes_info, infinity).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    error_logger:info_msg("Starting consul worker!"),
    {ok, ConsulUrl} = application:get_env(erlang_consul_node_discovery, consul_url),
    {ok, PullInterval} = application:get_env(erlang_consul_node_discovery, pull_interval),

    TimerRef = erlang:send_after(PullInterval, self(), pull_consul),

    State = #state{
        consul_url = ConsulUrl,
        pull_interval = PullInterval,
        timer_ref = TimerRef,
        nodes = sets:new()
    },

    {ok, State}.

handle_call(nodes_info, _From, State = #state{nodes = Nodes}) ->
    {reply, sets:to_list(Nodes), State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(pull_consul, State) ->
    #state{
        timer_ref = TimerRef,
        pull_interval = PullInterval,
        consul_url = Url,
        nodes = Nodes
    } = State,

    erlang:cancel_timer(TimerRef),

    NState = case do_pull_consul(Url) of
        {ok, Body} ->
            NewNodesInfo = sets:from_list(
                extract_nodes_info(Body)
            ),

            ToRemove = sets:subtract(Nodes, NewNodesInfo),
            lists:foreach(
              fun({Nodename, _}) ->
                  erlang_node_discovery:remove_node(Nodename)
              end,
              sets:to_list(ToRemove)
            ),

            ToAdd = sets:subtract(NewNodesInfo, Nodes),
            lists:foreach(
              fun({Nodename, Ports}) ->
                    lists:foreach(
                        fun(Port) ->
                            error_logger:info_msg("Connecting to ~p:~p~n", [Nodename, Port]),
                            erlang_node_discovery:add_node(Nodename, Port)
                        end,
                        Ports
                    )
              end,
              sets:to_list(ToAdd)
            ),

            State#state{nodes = NewNodesInfo};
        {error, _} -> State
    end,

    NewTimerRef = erlang:send_after(PullInterval, self(), pull_consul),
    {noreply, NState#state{timer_ref = NewTimerRef}};


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Private functions
-spec do_pull_consul(Url) -> Result when
    Url      :: binary(),
    Result   :: Body
              | {error, Reason},
    Body     :: binary(),
    Reason   :: term().

do_pull_consul(Url) ->
    case hackney:get(Url) of
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
            hackney:body(ClientRef);
        {error, Reason} ->
            error_logger:error_msg("Could not fetch data from Consul, reason: ~p", [Reason]),
            {error, Reason}
    end.

-spec extract_nodes_info(Body) -> Result when
    Body   :: binary(),
    Result :: [{Nodename, PortsList}],
    Nodename :: atom(),
    PortsList :: [inet:port_number()].
%
% The data is provided by Consul in the following format:
%
% Key: <<"xx/node-id_node/xxxx">>
% Value: #{<<"hostname">> => <<"hostname.com">>,
%          <<"port">> => xxxx,
%          <<"ports">> => [xxx,yyy,zzz]}
% And we're extracting the data in following data:
% [{node-id@hostname, [Ports]}]
extract_nodes_info(Body) ->
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
