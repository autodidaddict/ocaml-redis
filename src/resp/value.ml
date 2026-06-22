(** Represents a single RESP2/RESP3 value from the Redis protocol *)
type t =
  | Simple_string of string
  | Simple_error of string
  | Integer of int64
  | Bulk_string of string option
  | Array of t list
  | Boolean of bool
  | Double of float
  | Null
  | Map of (t * t) list
  | Set of t list
  | Push of t list
  | Blob_error of string
  | Verbatim_string of {
      format : string;
      value : string;
    }
  | Big_number of string
