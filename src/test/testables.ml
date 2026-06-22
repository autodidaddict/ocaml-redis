(* Alcotest testables for RESP types. Kept here in the test code (shared by both
   test executables) so the resp library itself carries no alcotest dependency. *)

open Resp

let rec value_equal (a : Value.t) (b : Value.t) : bool =
  let open Value in
  match (a, b) with
  | Simple_string x, Simple_string y
  | Simple_error x, Simple_error y
  | Blob_error x, Blob_error y
  | Big_number x, Big_number y -> String.equal x y
  | Integer x, Integer y -> Int64.equal x y
  | Bulk_string x, Bulk_string y -> Option.equal String.equal x y
  | Boolean x, Boolean y -> Bool.equal x y
  | Double x, Double y -> Float.equal x y
  | Null, Null -> true
  | Array x, Array y | Set x, Set y | Push x, Push y -> List.equal value_equal x y
  | Map x, Map y ->
    List.equal
      (fun (k1, v1) (k2, v2) -> value_equal k1 k2 && value_equal v1 v2)
      x y
  | ( Verbatim_string { format = f1; value = v1 },
      Verbatim_string { format = f2; value = v2 } ) ->
    String.equal f1 f2 && String.equal v1 v2
  | _ -> false

let rec value_pp (fmt : Format.formatter) (v : Value.t) : unit =
  let open Value in
  let elements name items =
    Format.fprintf fmt "%s[%a]" name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         value_pp)
      items
  in
  match v with
  | Simple_string s -> Format.fprintf fmt "Simple_string %S" s
  | Simple_error s -> Format.fprintf fmt "Simple_error %S" s
  | Integer i -> Format.fprintf fmt "Integer %Ld" i
  | Bulk_string None -> Format.fprintf fmt "Bulk_string None"
  | Bulk_string (Some s) -> Format.fprintf fmt "Bulk_string %S" s
  | Array items -> elements "Array " items
  | Boolean b -> Format.fprintf fmt "Boolean %b" b
  | Double d -> Format.fprintf fmt "Double %g" d
  | Null -> Format.fprintf fmt "Null"
  | Map pairs ->
    Format.fprintf fmt "Map [%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" value_pp k value_pp v))
      pairs
  | Set items -> elements "Set " items
  | Push items -> elements "Push " items
  | Blob_error s -> Format.fprintf fmt "Blob_error %S" s
  | Verbatim_string { format; value } ->
    Format.fprintf fmt "Verbatim_string {format=%S; value=%S}" format value
  | Big_number s -> Format.fprintf fmt "Big_number %S" s

let value : Value.t Alcotest.testable = Alcotest.testable value_pp value_equal

let command_equal (a : Command.t) (b : Command.t) : bool =
  match (a, b) with
  | Command.Ping, Command.Ping -> true
  | Command.Echo x, Command.Echo y -> String.equal x y
  | _ -> false

let command_pp (fmt : Format.formatter) (c : Command.t) : unit =
  match c with
  | Command.Ping -> Format.fprintf fmt "Ping"
  | Command.Echo s -> Format.fprintf fmt "Echo %S" s

let command : Command.t Alcotest.testable =
  Alcotest.testable command_pp command_equal
