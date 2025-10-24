(* adapter.ml - I/O adapters for different source types
   
   not really used much right now since we mostly use unix_io
   but keeping it for future socket/serial support
*)

open Value

(* source decoders *)
type decoder = bytes -> value option

let decode_float_line bytes =
  try
    let line_str = Bytes.to_string bytes in
    let trimmed = String.trim line_str in
    Some (VFloat (float_of_string trimmed))
  with _ -> None

let decode_int_line bytes =
  try
    let line_str = Bytes.to_string bytes in
    let trimmed = String.trim line_str in
    Some (VInt (int_of_string trimmed))
  with _ -> None

let decode_bool_line bytes =
  try
    let line_str = Bytes.to_string bytes in
    match String.trim (String.lowercase_ascii line_str) with
    | "true" | "1" | "on" -> Some (VBool true)
    | "false" | "0" | "off" -> Some (VBool false)
    | _ -> None
  with _ -> None

let decode_string_line bytes =
  let line_str = Bytes.to_string bytes in
  Some (VString (String.trim line_str))
