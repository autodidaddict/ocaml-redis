(** Decoding of RESP values from an Eio buffered reader.

    The parser is a set of {!Eio.Buf_read} combinators: it reads straight from the
    reader's internal buffer with no intermediate copies, and {!Eio.Buf_read}
    handles buffering bytes across reads. Drive it with {!Eio.Buf_read.of_flow}
    (a socket) or {!Eio.Buf_read.of_string} (a test). It is therefore I/O-free in
    the sense that nothing here is network-specific. Understands bulk strings
    (['$']) and arrays (['*']). *)

module R = Eio.Buf_read

(* Read a header count line (the bytes after a type byte, up to CRLF) as a
   non-negative int. *)
let count ~(what : string) (r : R.t) : int =
  let line = R.line r in
  match int_of_string_opt line with
  | Some n when n >= 0 -> n
  | _ -> failwith (Printf.sprintf "invalid %s length %S" what line)

let rec value (r : R.t) : Value.t =
  match R.any_char r with
  | '$' -> bulk r
  | '*' -> array r
  | c -> failwith (Printf.sprintf "unexpected type byte %C" c)

and bulk (r : R.t) : Value.t =
  let n = count ~what:"bulk string" r in
  let body = R.take n r in
  R.string "\r\n" r;
  Value.Bulk_string (Some body)

and array (r : R.t) : Value.t =
  let n = count ~what:"array" r in
  (* Parse elements left-to-right; the explicit recursion fixes evaluation order
     (unlike List.init, whose order is unspecified). *)
  let rec elements k acc =
    if k = 0 then List.rev acc else elements (k - 1) (value r :: acc)
  in
  Value.Array (elements n [])
