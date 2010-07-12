%%%-------------------------------------------------------------------
%%% File    : etorrent_file_logger.erl
%%% Author  : Jesper Louis Andersen <>
%%% Description : Log to a file. Loosely based on log_mf_h from the
%%%  erlang distribution
%%%
%%% Created :  9 Jul 2008 by Jesper Louis Andersen <>
%%%-------------------------------------------------------------------
-module(etorrent_file_logger).

-include("log.hrl").

-behaviour(gen_event).

-export([init/2]).

-export([init/1, handle_event/2, handle_info/2, terminate/2]).
-export([handle_call/2, code_change/3]).

-record(state, {dir, fname, cur_fd, pred}).

%% =======================================================================
%% @doc Initialize the file logger
%% @end
-spec init(string(), string()) -> 
        {string(), string(), fun((_) -> boolean())}.
init(Dir, Filename) -> init(Dir, Filename, fun(_) -> true end).

-spec init(string(), string(), fun((term()) -> boolean())) ->
            {string(), string(), fun((term()) -> boolean())}.
init(Dir, Filename, Pred) -> {Dir, Filename, Pred}.

%% -----------------------------------------------------------------------
file_open(Dir, Fname) ->
    {ok, FD} = file:open(filename:join(Dir, Fname), [append]),
    {ok, FD}.

date_str({{Y, Mo, D}, {H, Mi, S}}) ->
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
                                "~2.2.0w:~2.2.0w",
                                [Y,Mo,D,H,Mi,S])).
%% =======================================================================
init({Dir, Filename, Pred}) ->
    case catch file_open(Dir, Filename) of
        {ok, Fd} -> {ok, #state { dir = Dir, fname = Filename,
                                  cur_fd = Fd, pred = Pred }};
        Error -> Error
    end.

handle_event(Event, S) ->
    Date = date_str(erlang:localtime()),
        #state{dir = _Dir, fname = _Fname, cur_fd = _CurFd, pred = Pred} = S,
        case catch Pred(Event) of
        true ->
        io:format(S#state.cur_fd, "~s : ~p~n", [Date, Event]),
                {ok, S};
        _ ->
        {ok, S}
        end.

handle_info(_, State) ->
    {ok, State}.

terminate(_, State) ->
    case file:close(State#state.cur_fd) of
        ok -> State;
        {error, R} -> ?WARN([cant_close_file,{reason, R}]), State
    end.

handle_call(null, State) ->
    {ok, null, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

