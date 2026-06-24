type t = { dir : string; dbfilename : string }

let create ~dir ~dbfilename = { dir; dbfilename }

(* Redis treats configuration parameter names case-insensitively, so normalise
   before matching. *)
let get (t : t) (name : string) : (string * string) option =
  match String.lowercase_ascii name with
  | "dir" -> Some ("dir", t.dir)
  | "dbfilename" -> Some ("dbfilename", t.dbfilename)
  | _ -> None
