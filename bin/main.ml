open Otor

let () =
  let ic  = In_channel.open_bin "big-buck-bunny.torrent" in
  let src = In_channel.input_all ic in

  match Bencode.of_string_opt src with
  | Some v -> Printf.printf "%s" (Bencode.to_string v)
  | None -> Printf.printf "parse fail";

  In_channel.close ic
