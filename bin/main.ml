open Otor
open Cohttp_eio

let read_file env name =
  let ( / ) = Eio.Path.( / ) in
  let path = Eio.Stdenv.cwd env / name in
  Eio.Path.load path

let () =
  Eio_main.run @@ fun env ->
    let metainfo_src = read_file env "test.torrent" in
    let metainfo = Option.get @@ Bencode.of_string_opt metainfo_src in
    let torrent = Option.get @@ Torrent.of_bencode metainfo in
    let uri = Torrent.to_scrape_uri torrent in

    Eio.traceln "%s\n" (Option.get torrent.name);

    Eio.Switch.run @@ fun sw ->
      let client = Client.make ~https:None env#net in
      let _, body = Client.get ~sw client uri in
      print_string @@ Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int

