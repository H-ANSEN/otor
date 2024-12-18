open Otor
(*open Cohttp_eio*)

let read_file env name =
  let ( / ) = Eio.Path.( / ) in
  let path = Eio.Stdenv.cwd env / name in
  Eio.Path.load path

let () =
  Eio_main.run @@ fun env ->
    (*let metainfo_src = read_file env "test.torrent" in*)
    let metainfo_src = read_file env "test3.torrent" in
    let metainfo = Option.get @@ Bencode.of_string_opt metainfo_src in
    let torrent = Option.get @@ Torrent.of_bencode metainfo in

    let net = Eio.Stdenv.net env in
    let uri = Uri.of_string (Option.get torrent.announce) in
    let host = Option.get @@ Uri.host uri in
    let port = Int.to_string @@ Option.get @@ Uri.port uri in


    
    Eio.Switch.run @@ fun sw ->
      let sock = Eio.Net.datagram_socket ~sw net `UdpV4 in
      let addr = Eio.Net.getaddrinfo_datagram ~service:port net host |> List.hd in

      let open Tracker.Udp in
      let tracker = { sock; addr } in
      let announce = Tracker.Udp.announce tracker torrent |> Option.get in
      show_announce announce |> Eio.traceln "%s"

    (* HTTP Tracker request/announce
    let uri = Torrent.to_scrape_uri torrent in
    Eio.Switch.run @@ fun sw ->
      let client = Client.make ~https:None env#net in
      let _, body = Client.get ~sw client uri in
      print_string @@ Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
    *)
