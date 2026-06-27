(** Decoding of the RDB on-disk format into key/value entries.

    Like {!Resp.Parser}, this is a set of {!Eio.Buf_read} combinators: it
    consumes bytes from a reader and performs no I/O of its own. Drive it with
    {!Eio.Buf_read.of_string} (in-memory, for deterministic tests) or
    {!Eio.Buf_read.of_flow} (a file opened through the filesystem capability) —
    the decoder is identical either way; only the reader's backing decides
    whether real I/O happens. Loading entries into the store, and serialising
    back to an RDB image, are separate concerns handled elsewhere. *)

type entry = {
  key : string;
  value : string;
  expires_at_millis : int option;
      (** Absolute POSIX expiry in milliseconds — RDB's seconds deadlines are
          converted on the way in — or [None] for a key that never expires. *)
}

val read : entry list Eio.Buf_read.parser
(** [read r] decodes a whole RDB image from [r]: the [REDIS] magic and version,
    any metadata subsections (discarded), and the database's key/value pairs with
    their optional expiry, up to the end-of-file marker. The trailing checksum
    after that marker is not consumed.

    @raise Failure on a malformed image or an unsupported encoding — an LZF
      string or a non-string value type, neither of which occurs in this
      challenge. *)

val of_string : string -> entry list
(** [of_string image] is [read] applied to an in-memory [image] — convenient for
    tests and for callers that have already loaded the whole file. *)
