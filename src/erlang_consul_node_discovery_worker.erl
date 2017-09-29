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


-define(poll_consul, poll_consul).

-record(state, {
    consul_url = undefined    :: binary(),
    poll_interval = 60 * 1000 :: pos_integer(),
    timer_ref                 :: reference(),
    nodes                     :: [{nodename(), hostname(), inets:port()}],
    discovery_callback        :: undefined,
    response_parser           :: undefined
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

    catch begin

        ConsulUrl = case application:get_env(erlang_consul_node_discovery, consul_url) of
            {ok, C} ->
                C;
            undefined ->
                error_logger:warning_msg("Consul url is not set, consul discovery will not be used"),
                throw(ignore)
        end,

        CallBack = application:get_env(
            erlang_consul_node_discovery,
            discovery_callback, erlang_node_discovery
        ),

        PollInterval = application:get_env(erlang_consul_node_discovery, poll_interval, 60000),
        ReponseParser = application:get_env(
            erlang_consul_node_discovery, response_parser,
            erlang_consul_node_discovery_response_parser
        ),

        State = #state{
            consul_url = ConsulUrl,
            poll_interval = PollInterval,
            timer_ref = erlang:send_after(PollInterval, self(), ?poll_consul),
            nodes = [],
            response_parser = ReponseParser,
            discovery_callback = CallBack
        },
        {ok, State}
    end.


handle_call(nodes_info, _From, State = #state{nodes = Nodes}) ->
    {reply, Nodes, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(?poll_consul, State) ->
    #state{
        timer_ref = TimerRef,
        poll_interval = PollInterval,
        consul_url = Url,
        response_parser = ResponseParser,
        nodes = Nodes,
        discovery_callback = DiscoveryCallback
    } = State,

    erlang:cancel_timer(TimerRef),

    NState = case do_pull_consul(Url) of
        {ok, Body} ->
            NewNodesInfo = ResponseParser:parse(Body),

            ToRemove = Nodes -- NewNodesInfo,
            lists:foreach(
              fun({Nodename, _}) ->
                  DiscoveryCallback:remove_node(Nodename)
              end,
              ToRemove
            ),

            ToAdd = NewNodesInfo -- Nodes,
            lists:foreach(
              fun({Nodename, Ports}) ->
                    lists:foreach(
                        fun(Port) ->
                            error_logger:info_msg("Connecting to ~p:~p~n", [Nodename, Port]),
                            DiscoveryCallback:add_node(Nodename, Port)
                        end,
                        Ports
                    )
              end,
              ToAdd
            ),

            State#state{nodes = NewNodesInfo};
        {error, _} -> State
    end,

    NewTimerRef = erlang:send_after(PollInterval, self(), ?poll_consul),
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
