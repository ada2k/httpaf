let () = Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let request_handler _ reqd =
    let writer = Httpaf.Reqd.respond_with_streaming reqd (Httpaf.Response.create ~headers:(Httpun_types.Headers.of_list ["connection", "close"]) `OK) in
    let rec push () =
      Httpaf.Body.Writer.write_string writer "Chunk";
      Httpaf.Body.Writer.flush writer (
        function
        | `Written -> push ()
        | `Closed -> print_endline "Closed"; exit 0
      )
    in push ()
  in
  let handler = Httpaf_eio.Server.create_connection_handler ~error_handler:(fun _ ?request:_ error _ ->
    match error with
    | `Exn exn -> raise exn
    | _ -> failwith "error"
  ) ~request_handler in
  let listen = Eio.Net.listen ~backlog:0 ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.any, 1234)) in
  Eio.Net.run_server ~on_error:raise listen handler