(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

type t =
  { faraday                        : Faraday.t
  ; writer                         : Serialize.Writer.t
  ; mutable write_final_if_chunked : bool
  ; buffered_bytes                 : int ref
  }

let of_faraday faraday writer =
  { faraday
  ; writer
  ; write_final_if_chunked = true
  ; buffered_bytes         = ref 0
  }

let create buffer writer =
  of_faraday (Faraday.of_bigstring buffer) writer

let write_char t c =
  Faraday.write_char t.faraday c

let write_string t ?off ?len s =
  Faraday.write_string ?off ?len t.faraday s

let write_bigstring t ?off ?len b =
  Faraday.write_bigstring ?off ?len t.faraday b

let schedule_bigstring t ?off ?len (b:Bigstringaf.t) =
  Faraday.schedule_bigstring ?off ?len t.faraday b

let ready_to_write t = Serialize.Writer.wakeup t.writer

let flush t kontinue =
  Faraday.flush t.faraday kontinue;
  ready_to_write t

let is_closed t =
  Faraday.is_closed t.faraday

let close t =
  Faraday.close t.faraday;
  ready_to_write t;
;;

let has_pending_output t =
  (* Force another write poll to make sure that the final chunk is emitted for
     chunk-encoded bodies.

     Note that the body data type does not keep track of encodings, so it is
     necessary for [transfer_to_writer_with_encoding] to check the encoding and
     clear the [write_final_if_chunked] field when outputting a fixed or
     close-delimited body. *)
  Faraday.has_pending_output t.faraday
  || (Faraday.is_closed t.faraday && t.write_final_if_chunked)

let transfer_to_writer_with_encoding t ~encoding writer =
  let faraday = t.faraday in
  (* Play nicely with [has_pending_output] in the case of a fixed or
     close-delimited encoding. *)
  begin match encoding with
  | `Fixed _ | `Close_delimited -> t.write_final_if_chunked <- false;
  | `Chunked -> ()
  end;
  begin match Faraday.operation faraday with
  | `Yield -> ()
  | `Close ->
    let must_write_the_final_chunk = t.write_final_if_chunked in
    t.write_final_if_chunked <- false;
    if must_write_the_final_chunk then
      Serialize.Writer.schedule_chunk writer [];
    Serialize.Writer.unyield writer;
  | `Writev iovecs ->
    let buffered = t.buffered_bytes in
    begin match IOVec.shiftv iovecs !buffered with
    | []     -> ()
    | iovecs ->
      let lengthv  = IOVec.lengthv iovecs in
      buffered := !buffered + lengthv;
      begin match encoding with
      | `Fixed _ | `Close_delimited -> Serialize.Writer.schedule_fixed writer iovecs
      | `Chunked                    -> Serialize.Writer.schedule_chunk writer iovecs
      end;
      Serialize.Writer.flush writer (fun () ->
        Faraday.shift faraday lengthv;
        buffered := !buffered - lengthv)
    end
  end