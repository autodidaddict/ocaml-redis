(** Decoding of RESP values from an Eio buffered reader.

    The parser reads straight from an {!Eio.Buf_read} buffer with no intermediate
    copies; the reader handles buffering across socket reads. Nothing here is
    network-specific — drive it with {!Eio.Buf_read.of_flow} or
    {!Eio.Buf_read.of_string}. Understands bulk strings (['$']) and arrays
    (['*']). *)

(** [value r] parses a single RESP value from [r], reading more from the
    underlying source as needed.

    @raise Failure on a protocol error (unknown type byte, bad length, missing
      CRLF).
    @raise End_of_file if the source ends partway through a value. *)
val value : Value.t Eio.Buf_read.parser
