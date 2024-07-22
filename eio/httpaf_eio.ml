open Httpaf

module Buffer = struct
  type t = {
    buffer: Bigstringaf.t;
    mutable len: int;
    mutable off: int;
  }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0 then (
      t.off <- 0;
      t.len <- 0
    ) else
      if t.off > 0 then
        Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
        t.off <- 0

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0;
    n

  let put t ~f =
    compress t;
    let n = f t.buffer ~off:(t.off + t.len) ~len:(Bigstringaf.length t.buffer - t.len) in
    t.len <- t.len + n;
    n
end

let read fd buffer =
  match
    Buffer.put buffer ~f:(fun bigstring ~off ~len ->
      let cstruct = Cstruct.of_bigarray bigstring ~off ~len in
      Eio.Flow.single_read fd cstruct
    )
  with
  | 0 | exception End_of_file -> `Eof
  | read -> `Ok read
  | exception exn -> Eio.Flow.close fd; raise exn

let writev socket iovecs =
  let lenv, cstructs =
    List.fold_left_map (fun acc { Faraday.buffer; off; len } ->
      acc + len, Cstruct.of_bigarray buffer ~off ~len
    ) 0 iovecs
  in
  match Eio.Flow.write socket cstructs with
  | () -> `Ok lenv
  | exception (End_of_file | Eio.Io (Eio.Net.E Connection_reset _, _)) -> `Closed

module Server = struct
  let create_connection_handler ?(config = Config.default) ~error_handler ~request_handler socket client_addr =
    let connection = Server_connection.create ~config ~error_handler:(error_handler client_addr) (request_handler client_addr) in

    let read_buffer = Buffer.create config.read_buffer_size in
    let rec read_loop () =
      let rec read_loop_step () =
        match Server_connection.next_read_operation connection with
        | `Read -> (
          match read socket read_buffer with
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read_eof connection bigstring ~off ~len
            ) |> ignore;
            read_loop_step ()
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read connection bigstring ~off ~len
            ) |> ignore;
            read_loop_step ()
        )
        | `Yield ->
          Server_connection.yield_reader connection read_loop
        | `Close ->
          Eio.Flow.shutdown socket `Receive
      in

      try read_loop_step () with exn -> Server_connection.report_exn connection exn
    in

    let rec write_loop () =
      let rec write_loop_step () =
        match Server_connection.next_write_operation connection with
        | `Write io_vectors ->
          Server_connection.report_write_result connection (writev socket io_vectors);
          write_loop_step ()
        | `Yield ->
          Server_connection.yield_writer connection write_loop
        | `Close _ ->
          Eio.Flow.shutdown socket `Send
      in

      try write_loop_step () with exn -> Server_connection.report_exn connection exn
    in

    Eio.Fiber.both read_loop write_loop;
    Eio.Flow.close socket
end