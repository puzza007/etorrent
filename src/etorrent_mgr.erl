%%%
%%% This module is responsible for managing the run of set of torrent files.
%%%
-module(etorrent_mgr).
-behaviour(gen_server).

-include("etorrent_mnesia_table.hrl").
-include("log.hrl").

-export([start_link/1,

         start/1, stop/1,
         check/1]).

-export([handle_cast/2, handle_call/3, init/1, terminate/2]).
-export([handle_info/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state, {local_peer_id}).

%% API

%% =======================================================================

% @doc Start a new etorrent_t_manager process
% @end
-spec start_link(binary()) -> {ok, pid()} | ignore | {error, term()}.
start_link(PeerId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PeerId], []).

% @doc Ask the manager process to start a new torrent, given in File.
% @end
-spec start(string()) -> ok.
start(File) ->
    gen_server:cast(?SERVER, {start, File}).

% @doc Check a torrents contents
% @end
-spec check(integer()) -> ok.
check(Id) ->
    gen_server:cast(?SERVER, {check, Id}).

% @doc Ask the manager process to stop a torrent, identified by File.
% @end
-spec stop(string()) -> ok.
stop(File) ->
    gen_server:cast(?SERVER, {stop, File}).

%% =======================================================================

%% Callbacks
init([PeerId]) ->
    {ok, #state { local_peer_id = PeerId}}.

handle_cast({start, F}, S) ->
    case torrent_duplicate(F) of
        true -> {noreply, S};
        false ->
            case etorrent_t_pool_sup:add_torrent( F, S#state.local_peer_id,
                                                  etorrent_counters:next(torrent)) of
                {ok, _} -> {noreply, S};
                {error, {already_started, _Pid}} -> {noreply, S}
            end
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
    ?WARN([unknown_info, Info]),
    {noreply, State}.

terminate(_Foo, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =======================================================================
stop_torrent(F, S) ->
    ?INFO([stopping, F]),
    case etorrent_tracking_map:select({filename, F}) of
        {atomic, [#tracking_map{}]} ->
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

