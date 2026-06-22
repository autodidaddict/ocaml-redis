(** Serialisation of RESP values to the wire format. *)

(** [encode value] renders [value] as its RESP byte representation — the inverse
    of {!Parser.parse}. Total over {!Value.t}. *)
val encode : Value.t -> string
