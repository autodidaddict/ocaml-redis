(** Server configuration, fixed at startup from the command line.

    Like the store, configuration is a {e capability}: it is created once at
    startup and passed explicitly to the code that needs it, rather than read
    from a global. Holding a [t] lets you query config; code that is never handed
    one cannot. *)

type t

val create : dir:string -> dbfilename:string -> t
(** [create ~dir ~dbfilename] is the configuration parsed from the command line. *)

val dir : t -> string
(** [dir t] is the [--dir] the server was started with. *)

val dbfilename : t -> string
(** [dbfilename t] is the [--dbfilename] the server was started with. *)

val get : t -> string -> (string * string) option
(** [get t name] looks up the configuration parameter [name] case-insensitively
    and returns its [(canonical_name, value)] pair, or [None] if there is no such
    parameter. The canonical (lowercase) name is returned regardless of how the
    caller cased [name], so CONFIG GET echoes exactly what real Redis would. *)
