let () = Lwt_main.run @@
  let request_handler _ reqd =
    let response = Httpaf.Response.create ~headers:(Httpun_types.Headers.of_list ["connection", "keep-alive"; "content-length", (string_of_int (String.length "Hello World!"))]) `OK in
    (*
    let writer = Httpaf.Reqd.respond_with_streaming reqd (Httpaf.Response.create ~headers:(Httpun_types.Headers.of_list ["connection", "close"]) `OK) in
    let rec push () =
      Httpaf.Body.Writer.write_string writer "Chunk";
      Httpaf.Body.Writer.flush writer (
        function
        | `Written -> push ()
        | `Closed -> print_endline "Closed"
      )
    in push ()
    *)
    Httpaf.Reqd.respond_with_string reqd response "Hello World!"
  in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  let error_handler = fun _ ?request:_ error _ ->
    match error with
    | `Exn exn -> raise exn
    | _ -> failwith "error"
  in
  Lwt.bind
    (Lwt_io.establish_server_with_client_socket listen_address (Httpaf_lwt_unix.Server.create_connection_handler ~request_handler ~error_handler))
    (fun _ -> let forever, _ = Lwt.wait () in forever)