(** Serialisation of {!Value.t} to the RESP wire format — the inverse of
    {!Parser.value}. Pure and I/O-free, so it can be unit tested directly. *)

let rec encode (value : Value.t) : string =
  match value with
  | Simple_string s -> "+" ^ s ^ "\r\n"
  | Simple_error s -> "-" ^ s ^ "\r\n"
  | Integer i -> Printf.sprintf ":%Ld\r\n" i
  | Bulk_string None -> "$-1\r\n"
  | Bulk_string (Some s) -> Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
  | Null -> "_\r\n"
  | Boolean b -> if b then "#t\r\n" else "#f\r\n"
  | Double f -> Printf.sprintf ",%s\r\n" (encode_double f)
  | Big_number s -> Printf.sprintf "(%s\r\n" s
  | Blob_error s -> Printf.sprintf "!%d\r\n%s\r\n" (String.length s) s
  | Verbatim_string { format; value } ->
    let payload = format ^ ":" ^ value in
    Printf.sprintf "=%d\r\n%s\r\n" (String.length payload) payload
  | Array items -> aggregate '*' (List.length items) items
  | Set items -> aggregate '~' (List.length items) items
  | Push items -> aggregate '>' (List.length items) items
  | Map pairs ->
    let body =
      pairs
      |> List.concat_map (fun (k, v) -> [ encode k; encode v ])
      |> String.concat ""
    in
    Printf.sprintf "%%%d\r\n%s" (List.length pairs) body

(* Aggregate types share a [<prefix><count>\r\n] header followed by their
   elements encoded back-to-back. *)
and aggregate (prefix : char) (count : int) (items : Value.t list) : string =
  Printf.sprintf "%c%d\r\n%s" prefix count
    (items |> List.map encode |> String.concat "")

and encode_double (f : float) : string =
  if Float.is_nan f then "nan"
  else if f = Float.infinity then "inf"
  else if f = Float.neg_infinity then "-inf"
  else Printf.sprintf "%.17g" f
