(* A torrent file with support for downloading single files.                  *)
(* Torrent files with the 'length' key represent single files, this key will  *)
(* absent in a torrent file with mulitple files or a directory structure      *)

type t = {
  name      : string option;
  announce  : string option;
  info_hash : string;
  pieces    : bytes array;
  piece_len : int64;         (* num bytes the file is split into *)
  length    : int64;
} [@@deriving show]

let split_pieces pieces =
  let hash_len = 20 in 
  let pieces_len = String.length pieces in
  if pieces_len mod hash_len <> 0 
    then None
    else
      let piece_count = String.length pieces / hash_len in
      Array.init piece_count (fun i ->
        let offset = i * hash_len in
        let hash = String.sub pieces offset hash_len in
        String.to_bytes hash) |> Option.some

let of_bencode root =
  let open Bencode in
  let (>>=) = Option.bind in
  bval_opt ~key:"info" root            >>= fun info ->
  get_int_opt ~key:"length" info       >>= fun length ->
  get_int_opt ~key:"piece length" info >>= fun piece_len ->
  get_str_opt ~key:"pieces" info       >>= fun pieces ->
  split_pieces pieces                  >>= fun pieces ->
    let name = get_str_opt ~key:"name" info in
    let announce = get_str_opt ~key:"announce" root in
    let info_hash = to_string info |> Sha1.string |> Sha1.to_bin in
    Some { name; announce; info_hash; pieces; piece_len; length }

let to_uri torrent = 
  let add_param key value uri = Uri.add_query_param uri (key, [value]) in
  Uri.of_string (Option.get torrent.announce) 
    |> add_param "info_hash" torrent.info_hash
    |> add_param "peer_id" "11111222223333344444" (* create unique id here *)
    |> add_param "port" "6881"
    |> add_param "uploaded" "0"
    |> add_param "downloaded" "0"
    |> add_param "left" (Int64.to_string torrent.length)

let to_scrape_uri torrent =
  Uri.of_string (Option.get torrent.announce) |> fun base ->
  Uri.with_path base "scrape"                 |> fun base ->
  Uri.add_query_param base ("info_hash", [torrent.info_hash])

