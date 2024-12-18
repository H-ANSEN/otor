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
    let t_id = 69l in


    Eio.Switch.run @@ fun sw ->
      let sock = Eio.Net.datagram_socket ~sw net `UdpV4 in
      let addr = Eio.Net.getaddrinfo_datagram ~service:port net host |> List.hd in

      (* CONNECTION REQUEST *)
      let conn_req = Cstruct.of_bytes @@ Udp_tracker.connect_request t_id in
      Eio.Net.send ~dst:addr sock [conn_req];
      Eio.Net.recv sock conn_req |> ignore;

      (* EXTRACT THE CONNECTION ID FROM THE RESP *)
      let c_id = Option.get @@ Udp_tracker.connect_response t_id (Cstruct.to_bytes conn_req) in

      (* ANNOUNCE *)
      let announce_req = Cstruct.of_bytes @@ Udp_tracker.announce_request c_id t_id torrent in
      Udp_tracker.dump_announce_req (Cstruct.to_bytes announce_req);
      Eio.Net.send ~dst:addr sock [announce_req];

      let resp_buff = Cstruct.create 100  in
      let _, b = Eio.Net.recv sock resp_buff in

      let bytes = Cstruct.to_bytes resp_buff in
      let action = Bytes.get_int32_be bytes 0 in
      let t_id = Bytes.get_int32_be bytes 4 in

      Eio.traceln "Bytes recieved: %d" b;
      Eio.traceln "Action:         %ld" action;
      Eio.traceln "Transaction ID: %ld" t_id;

      ()


    (* HTTP Tracker request/announce
    let uri = Torrent.to_scrape_uri torrent in
    Eio.Switch.run @@ fun sw ->
      let client = Client.make ~https:None env#net in
      let _, body = Client.get ~sw client uri in
      print_string @@ Eio.Buf_read.(parse_exn take_all) body ~max_size:max_int
    *)
