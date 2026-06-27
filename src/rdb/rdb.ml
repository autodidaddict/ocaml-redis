module R = Eio.Buf_read

type entry = {
  key : string;
  value : string;
  expires_at_millis : int option;
}

(* Read [bytes] bytes as an unsigned integer, least- or most-significant byte
   first. RDB mixes endianness: the 32-bit size form is big-endian, while expiry
   timestamps and the integer string encodings are little-endian. *)
let read_uint ~(little_endian : bool) ~(bytes : int) (r : R.t) : int =
  let s = R.take bytes r in
  let acc = ref 0 in
  for i = 0 to bytes - 1 do
    let shift = if little_endian then 8 * i else 8 * (bytes - 1 - i) in
    acc := !acc lor (Char.code s.[i] lsl shift)
  done;
  !acc

let read_le ~bytes r = read_uint ~little_endian:true ~bytes r
let read_be ~bytes r = read_uint ~little_endian:false ~bytes r

(* Size encoding. The top two bits of the first byte choose the form: a concrete
   length (0b00 6-bit, 0b01 14-bit, 0b10 big-endian 32-bit), or for 0b11 one of
   the special integer/compressed string formats, whose 6-bit code we hand back
   for the string decoder to interpret. *)
let read_size (r : R.t) : [ `Len of int | `Format of int ] =
  let b0 = Char.code (R.any_char r) in
  match b0 lsr 6 with
  | 0b00 -> `Len (b0 land 0x3f)
  | 0b01 ->
    let b1 = Char.code (R.any_char r) in
    `Len (((b0 land 0x3f) lsl 8) lor b1)
  | 0b10 -> `Len (read_be ~bytes:4 r)
  | _ -> `Format (b0 land 0x3f)

(* A size used purely as a length (database index, hash-table sizes), never as a
   string format. *)
let read_length (r : R.t) : int =
  match read_size r with
  | `Len n -> n
  | `Format _ -> failwith "RDB: expected a length, found an integer-encoded size"

(* String encoding: a length-prefixed byte run, or one of the integer formats
   rendered as its decimal string. *)
let read_string (r : R.t) : string =
  match read_size r with
  | `Len n -> R.take n r
  | `Format 0 -> string_of_int (read_le ~bytes:1 r)
  | `Format 1 -> string_of_int (read_le ~bytes:2 r)
  | `Format 2 -> string_of_int (read_le ~bytes:4 r)
  | `Format 3 -> failwith "RDB: LZF-compressed strings are not supported"
  | `Format code -> failwith (Printf.sprintf "RDB: unknown string encoding %d" code)

(* The database body is a sequence of records. Each record is an optional expiry
   opcode (FC milliseconds / FD seconds) followed by a one-byte value type, the
   key, and the value. The structural opcodes FA/FE/FB are interleaved and carry
   no entry of their own. Reads are sequenced with explicit [let]s because
   OCaml's argument evaluation order is unspecified. *)
let rec read_entries (r : R.t) (acc : entry list) : entry list =
  match R.any_char r with
  | '\xFF' -> List.rev acc (* end of file; the trailing checksum is left unread *)
  | '\xFA' ->
    let _name = read_string r in
    let _value = read_string r in
    read_entries r acc
  | '\xFE' ->
    let _db_index = read_length r in
    read_entries r acc
  | '\xFB' ->
    let _table_size = read_length r in
    let _expires_size = read_length r in
    read_entries r acc
  | '\xFC' ->
    let ms = read_le ~bytes:8 r in
    let entry = read_value r ~expires_at_millis:(Some ms) in
    read_entries r (entry :: acc)
  | '\xFD' ->
    let seconds = read_le ~bytes:4 r in
    let entry = read_value r ~expires_at_millis:(Some (seconds * 1000)) in
    read_entries r (entry :: acc)
  | type_byte ->
    (* No expiry opcode: this byte is already the value type. *)
    let entry = read_value_of_type type_byte r ~expires_at_millis:None in
    read_entries r (entry :: acc)

and read_value (r : R.t) ~expires_at_millis : entry =
  read_value_of_type (R.any_char r) r ~expires_at_millis

and read_value_of_type (type_byte : char) (r : R.t) ~expires_at_millis : entry =
  match type_byte with
  | '\x00' ->
    let key = read_string r in
    let value = read_string r in
    { key; value; expires_at_millis }
  | c ->
    failwith (Printf.sprintf "RDB: unsupported value type 0x%02X" (Char.code c))

let read : entry list R.parser =
 fun r ->
  R.string "REDIS" r;
  let _version = R.take 4 r in
  read_entries r []

let of_string (image : string) : entry list = read (R.of_string image)
