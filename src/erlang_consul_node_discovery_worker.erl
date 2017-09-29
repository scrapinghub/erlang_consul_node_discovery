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
    nodes                     :: [{nodename(), hostname(), inets:port()}],
    node_register_callback    :: undefined,
    node_unregister_callback  :: undefined,
    consul_response_parser    :: undefined
}).

%% API.

nodes_info() ->
    gen_server:call(?MODULE, nodes_info, infinity).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    % Starting inets profile in order to perform requests
    inets:start(),
    %FIXME: why we need to set each callback separetely? Why not just pass module with well defined API?
    catch begin
        ConsulUrl = case application:get_env(erlang_consul_node_discovery, consul_url) of
            {ok, C} ->
                C;
            undefined ->
                error_logger:warning_msg("Consul url is not set, consul discovery will not be used"),
                throw(ignore)
        end,

        PullInterval = case application:get_env(erlang_consul_node_discovery, pull_interval) of
            {ok, P} -> P;
            undefined -> 30000
        end,

        RegisterCallback = case application:get_env(erlang_consul_node_discovery, node_register_callback) of
            {ok, {RCM, RCF}} ->
                fun RCM:RCF/2;
            undefined ->
                error_logger:warning_msg("Node register callback is not set, consul discovery will not be used"),
                throw(ignore)
        end,

        UnregisterCallback = case application:get_env(erlang_consul_node_discovery, node_unregister_callback) of
            {ok, {UNCM, UNCF}} ->
                fun UNCM:UNCF/1;
            undefined ->
                error_logger:warning_msg("Node unregister callback is not set, consul discovery will not be used"),
                throw(ignore)
        end,

        ConsulResponseParser = case application:get_env(erlang_consul_node_discovery, consul_response_parser) of
            {ok, {CRPM, CRPF}} ->
                fun CRPM:CRPF/1;
            undefined ->
                error_logger:warning_msg("Consul response parser is not set, consul discovery will not be used"),
                throw(ignore)
        end,
        State = #state{
            consul_url = ConsulUrl,
            pull_interval = PullInterval,
            timer_ref = erlang:send_after(PullInterval, self(), pull_consul),
            nodes = sets:new(),
            node_register_callback = RegisterCallback,
            node_unregister_callback = UnregisterCallback,
            consul_response_parser = ConsulResponseParser
        },
        {ok, State}
    end.


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
        nodes = Nodes,
        node_register_callback = NodeRegisterCallback,
        node_unregister_callback = NodeUnregisterCallback,
        consul_response_parser = ConsulResponseParser
    } = State,

    erlang:cancel_timer(TimerRef),

    NState = case do_pull_consul(Url) of
        {ok, Body} ->
            NewNodesInfo = sets:from_list(ConsulResponseParser(Body)),

            ToRemove = sets:subtract(Nodes, NewNodesInfo),
            lists:foreach(
              fun({Nodename, _}) ->
                  NodeUnregisterCallback(Nodename)
              end,
              sets:to_list(ToRemove)
            ),

            ToAdd = sets:subtract(NewNodesInfo, Nodes),
            lists:foreach(
              fun({Nodename, Ports}) ->
                    lists:foreach(
                        fun(Port) ->
                            error_logger:info_msg("Connecting to ~p:~p~n", [Nodename, Port]),
                            NodeRegisterCallback(Nodename, Port)
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
    case httpc:request(Url) of
        {ok, {_StatusCode, _RespHeader, Body}} ->
            {ok, Body};
        {error, Reason} ->
            error_logger:error_msg("Could not fetch data from Consul, reason: ~p", [Reason]),
            {error, Reason}
    end.
