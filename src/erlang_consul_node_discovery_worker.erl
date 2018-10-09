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
    consul_url                :: binary(),
    poll_interval = 60 * 1000 :: pos_integer(),
    timer_ref                 :: reference(),
    discovery_callback        :: module(),
    response_parser           :: module() 
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
    ConsulUrl = case application:get_env(erlang_consul_node_discovery, consul_url) of
        {ok, C} ->
            C;
        undefined ->
            error_logger:warning_msg("Consul url is not set, consul discovery will not be used"),
            throw(ignore)
    end,
    CallBack = application:get_env(
        erlang_consul_node_discovery,
        discovery_callback,
        erlang_node_discovery
    ),
    PollInterval = application:get_env(erlang_consul_node_discovery, poll_interval, 60000),
    ReponseParser = application:get_env(
        erlang_consul_node_discovery, 
        response_parser,
        erlang_consul_node_discovery_response_parser
    ),
    State = #state{
        consul_url = ConsulUrl, poll_interval = PollInterval,
        response_parser = ReponseParser, discovery_callback = CallBack
    },
    {ok, init_timer(State, 0)}.


handle_call(nodes_info, _From, State = #state{discovery_callback = DiscoveryCallback}) ->
    {reply, [{Node, Port} || {Node, {_Host, Port}} <- DiscoveryCallback:list_nodes()], State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(poll_consul, State) ->
    poll_consul(State),
    {noreply, init_timer(State)};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Private functions

-spec poll_consul(State) -> ok when
      State :: #state{}.
poll_consul(State = #state{consul_url = Url, response_parser = ResponseParser, discovery_callback = DiscoveryCallback}) ->
    case do_fetch_url(Url) of
        {ok, Body} ->
            ConsulNodes = [{Node, Port} || {Node, Port} <- ResponseParser:parse(Body)],
            CurrentNodes = [{Node, Port} || {Node, {_Host, Port}} <- DiscoveryCallback:list_nodes()],
            % remove_nodes(DiscoveryCallback, CurrentNodes -- ConsulNodes),
            add_nodes(DiscoveryCallback, ConsulNodes -- CurrentNodes),
            ok;
        {error, Reason} ->
            error_logger:error_msg("Failed to fetch url '~ts' with reason: ~p~n", [Url, Reason])
    end,
    ok.


-spec add_nodes(DiscoveryCallback, NodeList) -> ok when
      DiscoveryCallback :: module(),
      NodeList          :: [{node(), inet:port_number()}].
add_nodes(_, []) ->
    ok;
add_nodes(DiscoveryCallback, [{Node, Port} | Rest]) ->
    error_logger:info_msg("Adding ~p at ~p to discovery manager~n", [Node, Port]),
    DiscoveryCallback:add_node(Node, Port),
    add_nodes(DiscoveryCallback, Rest).


-spec remove_nodes(DiscoveryCallback, NodeList) -> ok when
      DiscoveryCallback :: module(),
      NodeList          :: [{node(), inet:port_number()}].
remove_nodes(_, []) ->
    ok;
remove_nodes(DiscoveryCallback, [{Node, _Port} | Rest]) ->
    error_logger:info_msg("Removing ~p from discovery manager~n", [Node]),
    DiscoveryCallback:remove_node(Node),
    remove_nodes(DiscoveryCallback, Rest).


-spec do_fetch_url(Url) -> Result when
    Url    :: binary(),
    Result :: {ok, binary()} | {error, term()}.
do_fetch_url(Url) ->
    case httpc:request(get, {Url, []}, [{timeout, 60000}, {connect_timeout, 20000}], []) of
        {ok, {_, _, Body}} -> {ok, Body};
        {error, Reason}    -> {error, Reason}
    end.


-spec init_timer(State) -> NewState when
      State    :: #state{},
      NewState :: #state{}.
init_timer(State = #state{poll_interval = PollInterval}) ->
    init_timer(State, PollInterval).


-spec init_timer(State, Delay) -> NewState when
      State    :: #state{},
      Delay    :: non_neg_integer(),
      NewState :: #state{}.
init_timer(State = #state{timer_ref = TimerRef}, Delay) ->
    catch erlang:cancel_timer(TimerRef),
    NewTimerRef = erlang:send_after(Delay, self(), poll_consul),
    State#state{timer_ref = NewTimerRef}.
