
module type Tracker = sig

  (*val announce : t -> Torrent.t -> peer list*)
end

module Udp = struct
  type t = {
    sock : [`Generic | `Unix] Eio.Net.datagram_socket_ty Eio.Resource.t;
    addr : Eio.Net.Sockaddr.datagram;
  }

  type announce = {
    interval : int32;
    leechers : int32;
    seeders  : int32;
    peers    : (int32 * int) array;
  } [@@deriving show]

  let mk_connect_request t_id =
    let buffer = Bytes.create 16 in
    Bytes.set_int64_be buffer 0  0x41727101980L; (* magic constant *)
    Bytes.set_int32_be buffer 8  0l;             (* action = connect (0) *)
    Bytes.set_int32_be buffer 12 t_id;
    Cstruct.of_bytes buffer

  let mk_announce_request c_id t_id torrent =
    let open Bytes in
    let open Torrent in
    let buffer = Bytes.create 98 in
    set_int64_be buffer 0  c_id;                  (* connection id *)
    set_int32_be buffer 8  1l;                    (* announce(1) *)
    set_int32_be buffer 12 t_id;                  (* transaction id *)
    blit_string torrent.info_hash 0 buffer 16 20; (* info hash *)
    blit_string torrent.info_hash 0 buffer 36 20; (* TODO gen peer_id *)
    set_int64_be buffer 56 0L;                    (* downloaded *)
    set_int64_be buffer 64 0L;                    (* left *)
    set_int64_be buffer 72 0L;                    (* uploaded *)
    set_int32_be buffer 80 0l;                    (* event 0 = none *)
    set_int32_be buffer 84 0l;                    (* ip address *)
    set_int32_be buffer 88 0l;                    (* unique generated key *)
    set_int32_be buffer 92 (-1l);                 (* num_want -1 = default *)
    set_int16_be buffer 96 6969;                  (* port *)
    Cstruct.of_bytes buffer

  let rd_connect_response t_id buf =
      if Bytes.length buf >= 16          &&
         Bytes.get_int32_be buf 0 = 0l   &&
         Bytes.get_int32_be buf 4 = t_id 
        then Some (Bytes.get_int64_be buf 8)
        else None

  let rd_peers buf len =
    let peer_size_bytes = 6 in
    let peer_start_offset = 20 in
    let remaining = len - peer_start_offset in
    let peer_count = remaining / peer_size_bytes in
    Array.init peer_count @@ fun i ->
      let ip_address = Bytes.get_int32_be buf (peer_start_offset + 6 * i) in
      let tcp_port = Bytes.get_int16_be buf ((peer_start_offset + 4) + 6 * i) in
      (ip_address, tcp_port)

  let rd_announce_resp t_id len buf =
    if Bytes.length buf < 20            ||
       Bytes.get_int32_be buf 0 <> 1l   ||
       Bytes.get_int32_be buf 4 <> t_id
      then None
      else
        let interval = Bytes.get_int32_be buf 8 in
        let leechers = Bytes.get_int32_be buf 12 in
        let seeders = Bytes.get_int32_be buf 16 in
        let peers = rd_peers buf len in
        Some {interval; leechers; seeders; peers}

  let connect t =
    let transaction_id = Random.bits32 () in
    let conn_req = mk_connect_request transaction_id in
    Eio.Net.send ~dst:t.addr t.sock [conn_req];
    Eio.Net.recv t.sock conn_req |> ignore;
    rd_connect_response transaction_id (Cstruct.to_bytes conn_req)

  let announce t torrent =
    Option.bind (connect t) @@ fun connection_id ->
      let transaction_id = Random.bits32 () in
      let announce_req = mk_announce_request connection_id transaction_id torrent in
      let recv_buffer = Bytes.create 2048 |> Cstruct.of_bytes in
      Eio.Net.send ~dst:t.addr t.sock [announce_req];
      let _, bytes_recieved = Eio.Net.recv t.sock recv_buffer in
      rd_announce_resp transaction_id bytes_recieved (Cstruct.to_bytes recv_buffer)
end
