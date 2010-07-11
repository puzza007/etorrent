-module(etorrent_proto_wire).

-export([incoming_packet/2, send_msg/3, decode_bitfield/2, encode_bitfield/2,
        decode_msg/1, remaining_bytes/1,
        complete_handshake/3, receive_handshake/1, initiate_handshake/3]).

-define(DEFAULT_HANDSHAKE_TIMEOUT, 120000).
-define(HANDSHAKE_SIZE, 68).
-define(PROTOCOL_STRING, "BitTorrent protocol").

%% Extensions
-define(EXT_BASIS, 0). % The protocol basis
-define(EXT_FAST,  4). % The Fast Extension

%% Packet types
-define(CHOKE, 0:8).
-define(UNCHOKE, 1:8).
-define(INTERESTED, 2:8).
-define(NOT_INTERESTED, 3:8).
-define(HAVE, 4:8).
-define(BITFIELD, 5:8).
-define(REQUEST, 6:8).
-define(PIECE, 7:8).
-define(CANCEL, 8:8).
-define(PORT, 9:8).

%% FAST EXTENSION Packet types
-define(SUGGEST, 13:8).
-define(HAVE_ALL, 14:8).
-define(HAVE_NONE, 15:8).
-define(REJECT_REQUEST, 16:8).
-define(ALLOWED_FAST, 17:8).

%% =======================================================================

% The type of packets:
-type packet() :: keep_alive
                | choke
                | unchoke
                | interested
                | not_interested
                | {have, integer()}
                | {bitfield, binary()}
                | {request, integer(), integer(), integer()}
                | {piece, integer(), integer(), binary()}
                | {cancel, integer(), integer(), integer()}
                | {port, integer()}
                | {suggest, integer()}
                | have_all
                | have_none
                | {reject_request, integer(), integer(), integer()}
                | {allowed_fast, [integer()]}.

% @doc Decode an incoming (partial) packet
% <p>The incoming packet function will attempt to decode an incoming packet. It
% returns either the decoded packet, or it returns a partial continuation to
% continue packet reading.</p>
% @end
-type cont_state() :: {partial, binary() | {integer(), [binary()]}}.
-spec incoming_packet(none | cont_state(), binary()) ->
    ok | {ok, binary(), binary()} | cont_state().
incoming_packet(none, <<>>) -> ok;

incoming_packet(none, <<0:32/big-integer, Rest/binary>>) ->
    {ok, <<>>, Rest};

incoming_packet(none, <<Left:32/big-integer, Rest/binary>>) ->
    incoming_packet({partial, {Left, []}}, Rest);

incoming_packet({partial, Data}, <<More/binary>>) when is_binary(Data) ->
    incoming_packet(none, <<Data/binary, More/binary>>);

incoming_packet(none, Packet) when byte_size(Packet) < 4 ->
    {partial, Packet};

incoming_packet({partial, {Left, IOL}}, Packet)
        when byte_size(Packet) >= Left, is_integer(Left) ->
    <<Data:Left/binary, Rest/binary>> = Packet,
    Left = byte_size(Data),
    P = iolist_to_binary(lists:reverse([Data | IOL])),
    {ok, P, Rest};

incoming_packet({partial, {Left, IOL}}, Packet)
        when byte_size(Packet) < Left, is_integer(Left) ->
    {partial, {Left - byte_size(Packet), [Packet | IOL]}}.

% @doc Send a message on a socket.
% <p>Returns a pair {R, Sz}, where R is the result of the send and Sz is the
% size of the sent datagram.</p>
% @end
-spec send_msg(port(), packet(), slow | fast) -> {ok | {error, term()},
                                                  integer()}.
send_msg(Socket, Msg, Mode) ->
    Datagram = encode_msg(Msg),
    Sz = byte_size(Datagram),
    case Mode of
        slow ->
            {gen_tcp:send(Socket, <<Sz:32/big, Datagram/binary>>), Sz};
        fast ->
            {gen_tcp:send(Socket, Datagram), Sz}
    end.

% @doc Decode a binary bitfield into a pieceset
% @end
-spec decode_bitfield(integer(), binary()) ->
    {ok, gb_set()} | {error, term()}.
decode_bitfield(Size, BinaryLump) ->
    ByteList = binary_to_list(BinaryLump),
    Numbers = decode_bytes(0, ByteList),
    PieceSet = gb_sets:from_list(lists:flatten(Numbers)),
    case max_element(PieceSet) < Size of
        true ->
            {ok, PieceSet};
        false ->
            {error, bitfield_had_wrong_padding}
    end.

% @doc Encode a pieceset into a binary bitfield
% <p>The Size entry is the full size of the bitfield, so the function knows
% how much and when to pad
% </p>
% @end
-spec encode_bitfield(integer(), gb_set()) -> binary().
encode_bitfield(Size, PieceSet) ->
    PadBits = 8 - (Size rem 8),
    F = fun(N) ->
                case gb_sets:is_element(N, PieceSet) of
                    true -> 1;
                    false -> 0
                end
        end,
    Bits = [F(N) || N <- lists:seq(0, Size-1)] ++
           [0 || _N <- lists:seq(1,PadBits)],
    0 = length(Bits) rem 8,
    list_to_binary(build_bytes(Bits)).

% @doc Decode a message from the wire
% @end
-spec decode_msg(binary()) -> packet().
decode_msg(Message) ->
   case Message of
       <<>> -> keep_alive;
       <<?CHOKE>> -> choke;
       <<?UNCHOKE>> -> unchoke;
       <<?INTERESTED>> -> interested;
       <<?NOT_INTERESTED>> -> not_interested;
       <<?HAVE, PieceNum:32/big>> -> {have, PieceNum};
       <<?BITFIELD, BitField/binary>> -> {bitfield, BitField};
       <<?REQUEST, Index:32/big, Begin:32/big, Len:32/big>> -> {request, Index, Begin, Len};
       <<?PIECE, Index:32/big, Begin:32/big, Data/binary>> -> {piece, Index, Begin, Data};
       <<?CANCEL, Index:32/big, Begin:32/big, Len:32/big>> -> {cancel, Index, Begin, Len};
       <<?PORT, Port:16/big>> -> {port, Port};
       %% FAST EXTENSION MESSAGES
       <<?SUGGEST, Index:32/big>> -> {suggest, Index};
       <<?HAVE_ALL>> -> have_all;
       <<?HAVE_NONE>> -> have_none;
       <<?REJECT_REQUEST, Index:32, Offset:32, Len:32>> ->
           {reject_request, Index, Offset, Len};
       <<?ALLOWED_FAST, FastSet/binary>> ->
           {allowed_fast, decode_allowed_fast(FastSet)}
   end.

% @doc Tell how many bytes there are left on a continuation
% <p>Occasionally, we will need to know how many bytes we are missing on a
% continuation, before we have to full packet. This function reports this.</p>
% @end
-spec remaining_bytes(none | cont_state()) -> {val, integer()}.
remaining_bytes(none) -> {val, 0};
remaining_bytes({partial, _D}) when is_binary(_D) -> {val, 0};
remaining_bytes({partial, {Left, _}}) when is_integer(Left) -> {val, Left}.

% @doc Complete a partially initiated handshake.
% <p>This function is used for incoming peer connections. They start off by
% transmitting all their information, we check it against our current database
% of what we accept. If we accept the message, then this function is called to
% complete the handshake by reflecting back the correct handshake to the
% peer.</p>
% @end
-spec complete_handshake(port(), binary(), binary()) ->
    ok | {error, term()}.
complete_handshake(Socket, InfoHash, LocalPeerId) ->
    try
        ok = gen_tcp:send(Socket, InfoHash),
        ok = gen_tcp:send(Socket, LocalPeerId),
        ok
    catch
        error:_ -> {error, stop}
    end.

% @doc Receive and incoming handshake
% <p>If the handshake is in the incoming direction, the method is to fling off
% the protocol header, so the peer knows we are talking bittorrent. Then it
% waits for the header to arrive. If the header is good, the connection can be
% completed by a call to complete_handshake/3.</p>
% @end
-spec receive_handshake(port()) ->
    {error, term()} | {ok, [{integer(), term()}], binary(), binary()}.
receive_handshake(Socket) ->
    Header = protocol_header(),
    case gen_tcp:send(Socket, Header) of
        ok ->
            receive_header(Socket, await);
        {error, X}  ->
            {error, X}
    end.

% @doc Initiate a handshake in the outgoing direction
% <p>If we are initiating a connection, then it is simple. We just fling off
% everything to the peer and await the peer to get back to us. When he
% eventually gets back, we can check his InfoHash against ours.</p>
% @end
-type capabilities() :: [{integer(), atom()}].
-spec initiate_handshake(port(), binary(), binary()) ->
    {error, term()} | {ok, capabilities(), binary(), binary()}
                    | {ok, capabilities(), binary()}.
initiate_handshake(Socket, LocalPeerId, InfoHash) ->
    % Since we are the initiator, send out this handshake
    Header = protocol_header(),
    try
        ok = gen_tcp:send(Socket, Header),
        ok = gen_tcp:send(Socket, InfoHash),
        ok = gen_tcp:send(Socket, LocalPeerId),
        receive_header(Socket, InfoHash)
    catch
        error:_ -> {error, stop}
    end.
%% =======================================================================

%% Encode a message for the wire
encode_msg(Message) ->
   case Message of
       keep_alive -> <<>>;
       choke -> <<?CHOKE>>;
       unchoke -> <<?UNCHOKE>>;
       interested -> <<?INTERESTED>>;
       not_interested -> <<?NOT_INTERESTED>>;
       {have, PieceNum} -> <<?HAVE, PieceNum:32/big>>;
       {bitfield, BitField} -> <<?BITFIELD, BitField/binary>>;
       {request, Index, Begin, Len} -> <<?REQUEST, Index:32/big, Begin:32/big, Len:32/big>>;
       {piece, Index, Begin, Data} -> <<?PIECE, Index:32/big, Begin:32/big, Data/binary>>;
       {cancel, Index, Begin, Len} -> <<?CANCEL, Index:32/big, Begin:32/big, Len:32/big>>;
       {port, PortNum} -> <<?PORT, PortNum:16/big>>;
       %% FAST EXTENSION
       {suggest, Index} -> <<?SUGGEST, Index:32>>;
       have_all -> <<?HAVE_ALL>>;
       have_none -> <<?HAVE_NONE>>;
       {reject_request, Index, Offset, Len} -> <<?REJECT_REQUEST, Index, Offset, Len>>;
       {allowed_fast, FastSet} -> 
           BinFastSet = encode_fastset(FastSet),
           <<?ALLOWED_FAST, BinFastSet/binary>>
   end.






protocol_header() ->
    PSSize = length(?PROTOCOL_STRING),
    ReservedBytes = encode_proto_caps(),
    <<PSSize:8, ?PROTOCOL_STRING, ReservedBytes/binary>>.


%%--------------------------------------------------------------------
%% Function: receive_header(socket()) -> {ok, proto_version(),
%%                                            remote_peer_id()} |
%%                                       {ok, proto_version(),
%%                                            info_hash(),
%%                                            remote_peer_id()} |
%%                                       {error, Reason}
%% Description: Receive the full header from a peer. The function
%% returns either with an error or successfully with a
%% protocol_version string, the infohash the remote sent us and his
%% peer_id.
%% --------------------------------------------------------------------
receive_header(Socket, InfoHash) ->
    %% Last thing we do on the socket, catch an error here.
    case gen_tcp:recv(Socket, ?HANDSHAKE_SIZE, ?DEFAULT_HANDSHAKE_TIMEOUT) of
        %% Fail if the header length is wrong
        {ok, <<PSL:8/integer, ?PROTOCOL_STRING, _:8/binary,
               _IH:20/binary, _PI:20/binary>>}
          when PSL /= length(?PROTOCOL_STRING) ->
            {error, packet_size_mismatch};
        %% If the infohash is await, return the infohash along.
        {ok, <<_PSL:8/integer, ?PROTOCOL_STRING, ReservedBytes:64/big,
               IH:20/binary, PI:20/binary>>}
          when InfoHash =:= await ->
            {ok, decode_proto_caps(ReservedBytes), IH, PI};
        %% Infohash mismatches. Error it.
        {ok, <<_PSL:8/integer, ?PROTOCOL_STRING, _ReservedBytes:64/big,
               IH:20/binary, _PI:20/binary>>}
          when IH /= InfoHash ->
            {error, infohash_mismatch};
        %% Everything ok
        {ok, <<_PSL:8/integer, ?PROTOCOL_STRING, ReservedBytes:64/big,
               _IH:20/binary, PI:20/binary>>} ->
            {ok, decode_proto_caps(ReservedBytes), PI};
        %% This is not even a header!
        {ok, X} when is_binary(X) ->
            {error, {bad_header, X}};
        %% Propagate errors upwards, most importantly, {error, closed}
        {error, Reason} ->
            {error, Reason}
    end.


%% PROTOCOL CAPS

encode_proto_caps() ->
    ProtoSpec = lists:sum([%?EXT_FAST,
                           ?EXT_BASIS]),
    <<ProtoSpec:64/big>>.

decode_proto_caps(N) ->
    Capabilities = [{?EXT_FAST,  fast_extension}],
    lists:foldl(
      fun
          ({M, Cap}, Acc) when (M band N) > 0 -> [Cap | Acc];
          (_Capability, Acc) -> Acc
      end,
      Capabilities,
      []).


build_bytes(BitField) ->
    build_bytes(BitField, []).

build_bytes([], Acc) ->
    lists:reverse(Acc);
build_bytes(L, Acc) ->
    {Byte, Rest} = lists:split(8, L),
    build_bytes(Rest, [bytify(Byte) | Acc]).

bytify([B1, B2, B3, B4, B5, B6, B7, B8]) ->
    <<B1:1/integer, B2:1/integer, B3:1/integer, B4:1/integer,
      B5:1/integer, B6:1/integer, B7:1/integer, B8:1/integer>>.


max_element(Set) ->
    gb_sets:fold(fun(E, Max) ->
                         case E > Max of
                             true ->
                                 E;
                             false ->
                                 Max
                         end
                 end, 0, Set).

decode_byte(B, Add) ->
    <<B1:1/integer, B2:1/integer, B3:1/integer, B4:1/integer,
      B5:1/integer, B6:1/integer, B7:1/integer, B8:1/integer>> = <<B>>,
    Bytes = [{B1, 0}, {B2, 1}, {B3, 2}, {B4, 3},
             {B5, 4}, {B6, 5}, {B7, 6}, {B8, 7}],
    [N+Add || {K, N} <- Bytes, K =:= 1].

decode_bytes(_SoFar, []) -> [];
decode_bytes(SoFar, [B | Rest]) ->
    [decode_byte(B, SoFar) | decode_bytes(SoFar + 8, Rest)].

%% FASTSET COMPUTATION

decode_allowed_fast(<<>>) -> [];
decode_allowed_fast(<<Index:32, Rest/binary>>) ->
    [Index | decode_allowed_fast(Rest)].

encode_fastset([]) -> <<>>;
encode_fastset([Idx | Rest]) ->
    R = encode_fastset(Rest),
    <<R/binary, Idx:32>>.
