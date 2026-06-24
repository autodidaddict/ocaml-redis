(** An in-memory key/value store backing GET/SET, with per-key expiry.

    The store is a {e capability}: hold a [t] and you may read and write it; code
    that is never handed one cannot touch it. It deliberately carries no clock —
    callers pass the current time as a plain value, so the store needs no I/O
    authority of its own and is exercised in tests with literal timestamps. *)

type t
(** A key/value store. Created once with {!create} and shared across the fibers
    handling each connection. *)

val create : unit -> t
(** [create ()] is a fresh, empty store. *)

val set : t -> ?expires_at:float -> string -> string -> unit
(** [set t ?expires_at key value] stores [value] under [key], replacing any
    existing entry (and its expiry). [expires_at] is an absolute POSIX time in
    seconds at or after which the key is considered gone; omit it for a key that
    never expires. Relative options like [PX] are resolved to an absolute
    deadline by the caller, before reaching here. *)

val get : t -> now:float -> string -> string option
(** [get t ~now key] is the value stored under [key], or [None] if the key is
    absent or has expired as of [now]. Expired keys are dropped on access (lazy
    expiry), so a read both reports and reclaims them. *)
