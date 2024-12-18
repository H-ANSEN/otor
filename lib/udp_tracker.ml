
let gen_t_id () = Random.bits32 ()

let connect_request t_id =
  let buffer = Bytes.create 16 in
  Bytes.set_int64_be buffer 0  0x41727101980L; (* magic constant *)
  Bytes.set_int32_be buffer 8  0l;             (* action = connect (0) *)
  Bytes.set_int32_be buffer 12 t_id;
  buffer

let connect_response t_id buf =
  if Bytes.length buf >= 16          &&
     Bytes.get_int32_be buf 0 = 0l   &&
     Bytes.get_int32_be buf 4 = t_id 
    then Some (Bytes.get_int64_be buf 8)
    else None

let announce_request c_id t_id torrent =
  let open Torrent in
  let buffer = Bytes.create 98 in
  Bytes.set_int64_be buffer 0  c_id;                  (* connection id *)
  Bytes.set_int32_be buffer 8  1l;                    (* action = announce(1) *)
  Bytes.set_int32_be buffer 12 t_id;                  (* transaction id *)
  Bytes.blit (String.to_bytes torrent.info_hash) 0 buffer 16 20;
  Bytes.blit_string torrent.info_hash 0 buffer 36 20; (* TODO gen peer_id *)
  Bytes.set_int64_be buffer 56 0L;                    (* downloaded *)
  Bytes.set_int64_be buffer 64 0L;                    (* left *)
  Bytes.set_int64_be buffer 72 0L;                    (* uploaded *)
  Bytes.set_int32_be buffer 80 0l;                    (* event 0 = none *)
  Bytes.set_int32_be buffer 84 0l;                    (* ip address *)
  Bytes.set_int32_be buffer 88 0l;                    (* unique generated key *)
  Bytes.set_int32_be buffer 92 (-1l);                 (* num_want -1 = default *)
  Bytes.set_int16_be buffer 96 6969;                  (* port *)
  buffer

let dump_announce_req buffer =
  Bytes.get_int64_be buffer 0   |> Eio.traceln "Connection ID:  %Ld";
  Bytes.get_int32_be buffer 8   |> Eio.traceln "Action:         %ld";
  Bytes.get_int32_be buffer 12  |> Eio.traceln "Transaction ID: %ld";
  Bytes.sub_string buffer 16 20 |> Sha1.string |> Sha1.to_hex |> Eio.traceln "Info Hash:      %s";
  Bytes.sub_string buffer 36 20 |> Sha1.string |> Sha1.to_hex |> Eio.traceln "Peer ID:        %s";
  Bytes.get_int64_be buffer 64  |> Eio.traceln "Downloaded:     %Ld";
  Bytes.get_int64_be buffer 72  |> Eio.traceln "Left:           %Ld";
  Bytes.get_int32_be buffer 80  |> Eio.traceln "Event:          %ld";
  Bytes.get_int32_be buffer 84  |> Eio.traceln "Ip Address:     %ld";
  Bytes.get_int32_be buffer 88  |> Eio.traceln "Key:            %ld";
  Bytes.get_int32_be buffer 92  |> Eio.traceln "Num Want:       %ld";
  Bytes.get_int16_be buffer 96  |> Eio.traceln "Port:           %d";
