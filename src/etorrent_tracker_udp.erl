-module(etorrent_tracker_udp).

%% API
-export([]).
-compile([export_all]).

-define(CONNECT, 0).
-define(ANNOUNCE, 1).
-define(SCRAPE, 2).
-define(ERROR, 3).

%%====================================================================

%%====================================================================
dispatch_incoming(ValidIDs, InfoHashes, Packet) ->
    <<Ty:32/big, TID:32/big, Rest/binary>> = Packet,
    case sets:is_element(TID, ValidIDs) of
	true ->
	    case Ty of
		?CONNECT when byte_size(Packet) < 16 ->
		    {error, packet_too_small};
		?CONNECT ->
		    handle_connect_response(Rest);
		?ANNOUNCE when byte_size(Packet) < 20 ->
		    {error, packet_too_small};
		?ANNOUNCE ->
		    handle_announce_response(Rest);
		?SCRAPE when byte_size(Packet) < 8 ->
		    {error, packet_too_small};
		?SCRAPE ->
		    handle_scrape_response(InfoHashes, Rest);
		?ERROR when byte_size(Packet) < 8 ->
		    {error, packet_too_small};
		?ERROR ->
		    {error_msg, binary_to_list(Rest)};
		_ ->
		    {error, unknown_udp_tracker_message_type}
	    end;
	false ->
	    {error, wrong_transaction_id}
    end.


random_transaction_id() ->
    crypto:rand_bytes(4).

connection_request(TransactionID) ->
    <<4497486125440:64/big, ?CONNECT:32/big, TransactionID:32/big>>.

announce_request(ConnID, TransactionID, InfoHash, PeerID,
		 {Downloaded, Left, Uploaded}, Event, IPAddr, Key, Port) ->
    true = is_binary(PeerID),
    true = is_binary(InfoHash),
    20 = byte_size(InfoHash),
    20 = byte_size(PeerID),
    EventN = convert_event(Event),
    IPN = convert_IP_addr(IPAddr),
    <<ConnID:64/big,
      ?ANNOUNCE:32/big,
      TransactionID:32/big,
      InfoHash/binary,
      PeerID/binary,
      Downloaded:64/big,
      Left:64/big,
      Uploaded:64/big,
      EventN:32/big,
      IPN/binary,
      Key:32/big,
      (-1):32/big,
      Port:16/big>>.

-spec scrape_request(integer(), integer(), [binary()]) ->
			    binary().
scrape_request(ConnID, TransactionID, InfoHashes) ->
    BinHashes = iolist_to_binary(InfoHashes),
    <<ConnID:64/big,
      ?SCRAPE:32/big,
      TransactionID:32/big,
      BinHashes/binary>>.

convert_event(none) -> 0;
convert_event(completed) -> 1;
convert_event(started) -> 2;
convert_event(stopped) -> 3.

convert_IP_addr({B1, B2, B3, B4}) ->
    <<B1:8, B2:8, B3:8, B4:8>>.

handle_connect_response(Packet) ->
    <<ConnectionID:64/big, _Rest/binary>> = Packet,
    {connect, ConnectionID}.

handle_announce_response(Packet) ->
    <<Interval:32/big,
      Leechers:32/big,
      Seeders:32/big,
      IPs/binary>> = Packet,
    {announce, parse_ips(IPs), [{interval, Interval},
				{leechers, Leechers},
				{seeders, Seeders}]}.

handle_scrape_response(InfoHashes, Packet) ->
    <<ScrapeList/binary>> = Packet,
    {scrape, parse_scrape(InfoHashes, ScrapeList)}.


parse_scrape([], <<>>) -> [];
parse_scrape([IH | IHL], <<Seeders:32/big, Completed:32/big, Leechers:32/big, SCLL/binary>>) ->
    [{IH, {Seeders, Completed, Leechers}} | parse_scrape(IHL, SCLL)].

parse_ips(<<>>) -> [];
parse_ips(<<B1:8, B2:8, B3:8, B4:8, P:16/big, Rest/binary>>) ->
    [{{B1, B2, B3, B4}, P} | parse_ips(Rest)].
