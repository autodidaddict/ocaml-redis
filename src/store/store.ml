(* A key/value entry: the stored bytes plus an optional absolute POSIX deadline
   (in milliseconds) at or after which the key is dead. *)
type entry = { value : string; expires_at_millis : int option }
type t = (string, entry) Hashtbl.t

let create () : t = Hashtbl.create 256

let set (t : t) ?expires_at_millis (key : string) (value : string) : unit =
  Hashtbl.replace t key { value; expires_at_millis }

let get (t : t) ~(now_millis : int) (key : string) : string option =
  match Hashtbl.find_opt t key with
  | Some { expires_at_millis = Some exp; _ } when exp <= now_millis ->
    (* Passive expiry: the deadline has passed, so drop the entry as we read it
       and report the key as absent. *)
    Hashtbl.remove t key;
    None
  | Some { value; _ } -> Some value
  | None -> None
