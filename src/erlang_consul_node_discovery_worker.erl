-module(erlang_consul_node_discovery_worker).
-behaviour(gen_server).

-callback list_nodes() -> list().
-callback remove_node(atom()) -> any().
-callback add_node(Node::atom(), Host::atom(), Port::integer()) -> ok.
-callback add_node(Node::atom(), Host::atom(), Port::integer(), Driver::atom()) -> ok.

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(APP, erlang_consul_node_discovery).
-define(PARSER, erlang_consul_node_discovery_response_parser).

-record(state, {
    consul_url                :: binary(),
    poll_interval = 60 * 1000 :: pos_integer(),
    timer_ref                 :: reference(),
    discovery_callback        :: module(),
    response_parser           :: module()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    % Starting inets profile in order to perform requests
    inets:start(),
    try
        {ok, ConsulUrl} = application:get_env(?APP, consul_url),
        {ok, CallBack} = application:get_env(?APP, discovery_callback),
        PollInterval = application:get_env(?APP, poll_interval, 60000),
        State = #state{consul_url = ConsulUrl,
                       discovery_callback = CallBack,
                       response_parser = ?PARSER,
                       poll_interval = PollInterval},
        {ok, init_timer(State), 0}
    catch
        error:{badmatch, undefined} ->
            error_logger:warning_msg("Consul-Url or Callback-Module is not set, ~p is not started", [?APP]),
            ignore
    end.


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(timeout, State) ->
    handle_info(poll_consul, State);
handle_info(poll_consul, State) ->
    {noreply, poll_consul(State)};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Private functions

-spec poll_consul(State) -> ok when
      State :: #state{}.
poll_consul(State = #state{consul_url = Url, response_parser = Parser}) ->
    case do_fetch_url(Url, State#state.poll_interval) of
        {ok, Body} ->
            erlang_consul_node_discovery_sync:apply(Parser:parse(Body),
                State#state.discovery_callback);
        {error, Reason} ->
            error_logger:error_msg("Failed to fetch url '~ts' with reason: ~p~n", [Url, Reason])
    end,
    State.

-spec do_fetch_url(Url, Timeout) -> Result when
    Url    :: binary(),
    Timeout:: pos_integer(),
    Result :: {ok, binary()} | {error, term()}.
do_fetch_url(Url, Timeout) ->
    ConnectTimeout = Timeout div 3,
    case httpc:request(get, {Url, []}, [{timeout, Timeout-ConnectTimeout},
                                        {connect_timeout, ConnectTimeout}], []) of
        {ok, {_, _, Body}} -> {ok, Body};
        {error, Reason}    -> {error, Reason}
    end.


-spec init_timer(State) -> NewState when
      State    :: #state{},
      NewState :: #state{}.
init_timer(State = #state{poll_interval = PollInterval}) ->
    {ok, TRef} = timer:send_interval(PollInterval, poll_consul),
    State#state{timer_ref = TRef}.

