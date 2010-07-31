-module(etorrent_mktorrent).

%% API
-export([mktorrent/3]).

%%====================================================================
%% API
%%====================================================================
mktorrent(InFile, AnnounceURL, OutFile) ->
    PieceHashes = read_and_hash(InFile),
    TorrentData = torrent_file(AnnounceURL, PieceHashes, null),
    write_torrent_file(OutFile, TorrentData).

read_and_hash(_InFile) ->
    todo.

mk_comment(null) -> [];
mk_comment(Comment) when is_list(Comment) -> [{string, "comment"}, {string, Comment}].

mk_infodict(PieceHashes) ->
    {dict, [{string, "pieces"}, {string, PieceHashes}]}.

write_torrent_file(Out, Data) ->
    Encoded = etorrent_bcoding:encode(Data),
    file:write_file(Out, Encoded).

torrent_file(AnnounceURL, PieceHashes, Comment) ->
    InfoDict = mk_infodict(PieceHashes),
    {dict, [{string, "announce"}, {string, AnnounceURL},
	    {string, "info"}, InfoDict] ++ mk_comment(Comment)}.

%%====================================================================
%% Internal functions
%%====================================================================
