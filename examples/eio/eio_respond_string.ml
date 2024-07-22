let () = Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let request_handler _ reqd =
    let response = Httpaf.Response.create ~headers:(Httpun_types.Headers.of_list ["connection", "keep-alive"; "content-length", "12"]) `OK in
    (*
    let writer = Httpaf.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd response in
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
  let handler = Httpaf_eio.Server.create_connection_handler ~error_handler:(fun _ ?request:_ error _ ->
    match error with
    | `Exn exn -> raise exn
    | _ -> failwith "error"
  ) ~request_handler in
  let listen = Eio.Net.listen ~reuse_addr:true ~backlog:0 ~sw env#net (`Tcp (Eio.Net.Ipaddr.V4.any, 1234)) in
  Eio.Net.run_server ~on_error:raise listen handler