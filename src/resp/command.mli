(* SET key value [NX | XX] [GET] [EX s | PX ms | EXAT s | PXAT ms | KEEPTTL] *)
type existence =
  | Always  (** no NX/XX supplied *)
  | If_not_exists  (** NX *)
  | If_exists  (** XX *)

(** SET's expiry option. One field, so EX/PX/EXAT/PXAT/KEEPTTL are mutually
    exclusive by construction. EX/PX are relative durations; EXAT/PXAT are
    absolute Unix timestamps — keep them distinct here and resolve to an
    absolute deadline at apply time. *)
type expiry =
  | Expire_seconds of int  (** EX, seconds, relative *)
  | Expire_millis of int  (** PX, milliseconds, relative *)
  | Expire_at_seconds of int  (** EXAT, Unix time, seconds *)
  | Expire_at_millis of int  (** PXAT, Unix time, milliseconds *)
  | Keep_ttl  (** KEEPTTL - retain existing ttl*)

type set_options = {
  key : string;
  value : string;
  existence : existence;
  get : bool;
  expiry : expiry option;
}

(** A strongly typed client command, lifted from a raw {!Value.t}.

    Clients send commands as RESP arrays of bulk strings, e.g.
    [*1\r\n$4\r\nPING\r\n]. {!of_value} turns that wire shape into a typed
    command so dispatch can match on [Ping] / [Echo] rather than on array
    structure. *)
type t = Ping | Echo of string | Get of string | Set of set_options

val of_value : Value.t -> (t, string) result
(** [of_value v] interprets [v] (a RESP array of bulk strings) as a command, or
    returns a human-readable error if it is unrecognised or malformed. *)
