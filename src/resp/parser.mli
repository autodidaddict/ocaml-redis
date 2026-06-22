(** Decoding of RESP values from an in-memory buffer.

    Pure and I/O-free, so it can be driven by a test, a file, or a socket reader
    alike. The decoding is a state machine internally, but that is hidden: the
    public surface is just one-shot {!parse} and resumable {!feed}. For now it
    understands only bulk strings (the ['$'] type). *)

(** Why a parse could not produce a value. *)
type error = Parse_error of string

(** Outcome of decoding a single value from the front of a buffer. *)
type outcome =
  | Done of Value.t * string  (** value, plus any bytes that followed it *)
  | Incomplete  (** buffer doesn't hold a full value yet; feed more *)
  | Failed of error

(** [parse buf] decodes a single value from the front of [buf]. *)
val parse : string -> outcome

(** A resumable parser handle carrying the bytes seen so far across feeds. *)
type partial

(** A fresh parser: at a value boundary, nothing buffered. *)
val create : unit -> partial

(** [feed parser chunk] appends [chunk] to the buffered bytes and drains every
    complete value it can, returning them in order along with a parser carrying
    any leftover partial frame. Call again with more bytes to resume.

    e.g. [feed (create ()) "$5\r\nhe"] gives [Ok ([], waiting)], then
    [feed waiting "llo\r\n"] gives [Ok ([Bulk_string (Some "hello")], _)]. *)
val feed : partial -> string -> (Value.t list * partial, error) result
