module Server : sig
  val create_connection_handler
  : ?config:Httpaf.Config.t
  -> error_handler:(Eio.Net.Sockaddr.stream -> Httpaf.Server_connection.error_handler)
  -> request_handler:(Eio.Net.Sockaddr.stream -> Httpaf.Server_connection.request_handler)
  -> [> `Unix] Eio.Net.stream_socket_ty Eio.Resource.t
  -> Eio.Net.Sockaddr.stream
  -> unit
end