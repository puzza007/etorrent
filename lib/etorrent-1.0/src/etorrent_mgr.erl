%%%
%%% This module is responsible for managing the run of set of torrent files.
%%%
-module(etorrent_mgr).
-behaviour(gen_server).

-include("etorrent_mnesia_table.hrl").

-export([start_link/1,

         start/1, stop/1,
         check/1,

         stop_all/0]).

-export([handle_cast/2, handle_call/3, init/1, terminate/2]).
-export([handle_info/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state, {local_peer_id}).

%% API

%% Start a new etorrent_t_manager process
start_link(PeerId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PeerId], []).

%% Ask the manager process to start a new torrent, given in File.
start(File) ->
    gen_server:cast(?SERVER, {start, File}).

%% Check a torrents contents
check(Id) ->
    gen_server:cast(?SERVER, {check, Id}).

%% Ask the manager process to stop a torrent, identified by File.
stop(File) ->
    gen_server:cast(?SERVER, {stop, File}).

stop_all() ->
    gen_server:call(?SERVER, stop_all, 120000).

%% Callbacks
init([PeerId]) ->
    {ok, #state { local_peer_id = PeerId}}.

handle_cast({start, F}, S) ->
    case torrent_duplicate(F) of
        true -> {noreply, S};
        false ->
            {ok, _} =
                etorrent_t_pool_sup:add_torrent(
                  F,
                  S#state.local_peer_id,
                  etorrent_counters:next(torrent)),
            {noreply, S}
    end;
handle_cast({check, Id}, S) ->
    {atomic, [T]} = etorrent_tracking_map:select(Id),
    SPid = T#tracking_map.supervisor_pid,
    Child = etorrent_t_sup:get_pid(SPid, control),
    etorrent_t_control:check_torrent(Child),
    {noreply, S};
handle_cast({stop, F}, S) ->
    stop_torrent(F, S).

handle_call(stop_all, _From, S) ->
    {atomic, Torrents} = etorrent_tracking_map:all(),
    lists:foreach(fun(#tracking_map { filename = F }) ->
                          etorrent_t_pool_sup:stop_torrent(F),
                          ok
                  end,
                  Torrents),
    {reply, ok, S};
handle_call(_A, _B, S) ->
    {noreply, S}.

handle_info(Info, State) ->
    error_logger:info_msg("Unknown message: ~p~n", [Info]),
    {noreply, State}.

terminate(_Foo, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
stop_torrent(F, S) ->
    error_logger:info_msg("Stopping ~p~n", [F]),
    case etorrent_tracking_map:select({filename, F}) of
        {atomic, [T]} when is_record(T, tracking_map) ->
            etorrent_t_pool_sup:stop_torrent(F),
            ok;
        {atomic, []} ->
            %% Was already removed, it is ok.
            ok
    end,
    {noreply, S}.


torrent_duplicate(F) ->
    case etorrent_tracking_map:select({filename, F}) of
        {atomic, []} -> false;
        {atomic, [T]} -> duplicate =:= T#tracking_map.state
    end.
