%%% A manager for UDP communication with trackers
-module(etorrent_udp_tracker_mgr).

-behaviour(gen_server).
-include("log.hrl").

%% API
-export([start_link/0, announce_request/2, announce_request/3, monitor/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, { socket }).

-define(SERVER, ?MODULE).
-define(CTBL, etorrent_udp_connection).
-define(TTBL, etorrent_udp_transact).
-define(DEFAULT_TIMEOUT, timer:seconds(60)).

%%====================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

announce_request(URL, Props) ->
     announce_request(URL, Props, none).

announce_request(URL, Props, Event) ->
    gen_server:cast({announce_request, self(), URL, Props, Event}).

monitor() ->
    erlang:monitor(process, ?SERVER).

%%====================================================================
init([]) ->
    _Tid = ets:new(?CTBL, [named_table, protected]),
    _Tid = ets:new(?TTBL,   [named_table, protected]),
    {ok, Port} = application:get_env(etorrent, udp_port),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}, inet, inet6]),
    {ok, #state{ socket = Socket }}.

handle_call(Request, _From, State) ->
    ?WARN([unknown_call, Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({announce_request, Pid, URL, Props, Event} = Req, #state { socket = Sock } = S) ->
    case dissect_url(URL) of
	{ok, _Scheme, Host, Port} ->
	    case get_connection_id(Host, Port, Sock) of
		{ok, ConnID} ->
		    push_announce_transact(ConnID, Host, Port, Sock, Props, Pid, Event);
		gathering_conn_id ->
		    push_connid_gather_piggyback({Host, Port}, Req)
	    end;
	Term ->
	    etorrent_tracker_communication:udp_respond(Pid, {error, Term})
    end,
    {noreply, S};
handle_cast(Msg, State) ->
    ?WARN([unknown_msg, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?WARN([unknown_info, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
dissect_url(URL) ->
    case etorrent_http_uri:parse(URL) of
	{http, Scheme, _, Host, Port, _, _Q} ->
	    {ok, Scheme, Host, Port};
	T -> {error, T}
    end.

get_connection_id(Host, Port, Socket) ->
    case ets:lookup(?CTBL, {Host, Port}) of
	[] ->
	    TID = etorrent_tracker_udp:random_transaction_id(),
	    P = etorrent_tracker_udp:connection_request(TID),
	    gen_udp:send(Socket, Host, Port, P),
	    true = ets:insert(?CTBL, {{Host, Port}, connection_gather, []}),
	    timer:send_after(?DEFAULT_TIMEOUT, self(), {remove_ctbl, TID}),
	    gathering_conn_id;
	[{_, connection_gather, _}] ->
	    gathering_conn_id;
	[{_, conn_id, ConnID}]  ->
	    {ok, ConnID}
    end.

push_announce_transact(ConnID, Host, Port, Sock, Props, Pid, Event) ->
    TID = etorrent_tracker_udp:random_transaction_id(),
    P = etorrent_tracker_udp:announce_request(ConnID, TID,
					      proplists:get_value(infohash, Props),
					      proplists:get_value(peer_id, Props),
					      {proplists:get_value(downloaded, Props),
					       proplists:get_value(left, Props),
					       proplists:get_value(uploaded, Props)},
					      Event,
					      0,0,0 %% @todo what are these values?
					      ),
    gen_udp:send(Sock, Host, Port, P),
    true = ets:insert(?TTBL, {TID, Pid}),
    timer:send_after(?DEFAULT_TIMEOUT, self(), {remove_ttbl, TID}),
    ok.

push_connid_gather_piggyback({Host, Port}, Request) ->
    [{HP, connection_gather, Reqs}] = ets:lookup(?CTBL, {Host, Port}),
    true = ets:insert(?CTBL, {HP, connection_gather, [Request | Reqs]}),
    ok.

