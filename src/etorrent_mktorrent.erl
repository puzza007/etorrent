-module(etorrent_mktorrent).

-include_lib("kernel/include/file.hrl").
-include("log.hrl").
%% API
-export([mktorrent/3]).

-define(CHUNKSIZE, 1048576).
%%====================================================================
%% API
%%====================================================================
mktorrent(FD, AnnounceURL, OutFile) ->
    {PieceHashes, FileInfo} = read_and_hash(FD),
    TorrentData = torrent_file(AnnounceURL, PieceHashes, FileInfo, null),
    write_torrent_file(OutFile, TorrentData).

hash_file(File, {PH, InfoBlocks}) ->
    {ok, FI} = file:read_file_info(File),
    IB = {File, FI},
    {ok, IODev} = file:open(File, [read, binary, read_ahead]),
    PHUpdate = add_hashes(IODev, PH),
    {PHUpdate, [IB | InfoBlocks]}.

add_hashes(IODev, PH) ->
    PH2 = hash(IODev, file:read(IODev, ?CHUNKSIZE), PH),
    {_, H} = PH2,
    0 = length(H) rem 20,
    PH2.

cut_chunk({Bin, Hashes}) when byte_size(Bin) >= ?CHUNKSIZE ->
    <<Chunk:?CHUNKSIZE/binary, Rest/binary>> = Bin,
    cut_chunk({Rest, [crypto:sha(Chunk) | Hashes]});
cut_chunk(Otherwise) -> Otherwise.

hash(IODev, eof, PH) ->
    file:close(IODev),
    cut_chunk(PH);
hash(IODev, {ok, NewData}, {Bin, Hashes}) ->
    hash(IODev, file:read(IODev, ?CHUNKSIZE),
	 cut_chunk({<<Bin/binary, NewData/binary>>, Hashes})).

read_and_hash(Arg) ->
    Empty = {{<<>>, []}, []},
    PH = case filelib:is_dir(Arg) of
	true -> filelib:fold_files(Arg, ".*", true, fun hash_file/2, Empty);
	false ->
	    true = filelib:is_regular(Arg),
	    hash_file(Arg, Empty)
    end,
    finish_hash(PH).

finish_hash({{<<>>, Hashes}, FI}) -> {iolist_to_binary(lists:reverse(Hashes)),
				      lists:reverse(FI)};
finish_hash({{Bin, Hashes}, FI}) ->
    H = crypto:sha(Bin),
    {iolist_to_binary(lists:reverse([H | Hashes])),
     lists:reverse(FI)}.

mk_comment(null) -> [];
mk_comment(Comment) when is_list(Comment) -> [{{string, "comment"}, {string, Comment}}].

mk_infodict_single(PieceHashes, Name, Sz) ->
    {dict, [{{string, "pieces"}, {string, PieceHashes}},
	    {{string, "name"}, {string, Name}},
	    {{string, "length"}, {integer, Sz}}]}.

mk_files_list([], Accum, Sz) ->
    {Sz, lists:reverse(Accum)};
mk_files_list([{N, #file_info { size = Size }} | R], Acc, S) ->
    D = {dict, [{{string, "path"}, {string, N}},
		{{string, "size"}, {integer, Size}}]},
    mk_files_list(R, [D | Acc], S + Size).

mk_infodict_multi(PieceHashes, D) ->
    {Sz, L} = mk_files_list(D, [], 0),
    {dict, [{{string, "pieces"}, {string, PieceHashes}},
	    {{string, "length"}, {integer, Sz}},
	    {{string, "files"}, {list, L}}]}.

write_torrent_file(Out, Data) ->
    Encoded = etorrent_bcoding:encode(Data),
    file:write_file(Out, Encoded).

torrent_file(AnnounceURL, PieceHashes, FileInfo, Comment) ->
    InfoDict = case FileInfo of
		   [{Name, #file_info { size = Sz }}] ->
		       mk_infodict_single(PieceHashes, Name, Sz);
		   L when is_list(L) ->
		       mk_infodict_multi(PieceHashes, L)
	       end,
    {dict, [{{string, "announce"}, {string, AnnounceURL}},
	    {{string, "info"}, InfoDict}]
     ++ mk_comment(Comment)}.

