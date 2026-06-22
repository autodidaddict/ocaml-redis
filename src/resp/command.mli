(** Strongly typed client commands lifted from raw RESP values. *)

(** A command the server understands. [Echo] is part of the shape but not yet
    recognised by {!of_value}. *)
type t =
  | Ping
  | Echo of string

(** [of_value v] interprets [v] (a RESP array of bulk strings) as a command, or
    returns a human-readable error if it is unrecognised or malformed. *)
val of_value : Value.t -> (t, string) result
