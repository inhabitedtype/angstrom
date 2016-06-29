open Angstrom

type json =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of float
  | `Object of (string * json) list
  | `Array of json list ]

let ws = skip_while (function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false)

let lchar c =
  ws *> char c

let rsb = lchar ']'
let rcb = lchar '}'
let ns, vs  = lchar ':', lchar ','
let quo = lchar '"'

let _false : json t = string "false" *> return `False
let _true  : json t = string "true"  *> return `True
let _null  : json t = string "null"  *> return `Null

let num =
  take_while1 (function
    | '\x20' | '\x0a' | '\x0d' | '\x09'
    | '[' | ']' | '{' | '}' | ':' | ',' -> false
    | _               -> true)
  >>= fun s ->
  try return (`Number (float_of_string s))
  with _ -> fail "number"

module S = struct
  type t =
    | Unescaped
    | Escaped
    | Unicode of char list
    | Error   of string
    | Done

  let to_string = function
    | Unescaped -> "unescaped"
    | Escaped   -> "escaped"
    | Unicode _ -> "unicode _"
    | Error e   -> Printf.sprintf "error %S" e
    | Done      -> "done"

  let unescaped buf = function
    | '"'  -> Some Done
    | '\\' -> Some Escaped
    | c    -> Buffer.add_char buf c; Some Unescaped

  let escaped buf = function
    | '\x22' -> Buffer.add_char buf '\x22'; Some Unescaped
    | '\x5c' -> Buffer.add_char buf '\x5c'; Some Unescaped
    | '\x2f' -> Buffer.add_char buf '\x2f'; Some Unescaped
    | '\x62' -> Buffer.add_char buf '\x08'; Some Unescaped
    | '\x66' -> Buffer.add_char buf '\x0c'; Some Unescaped
    | '\x6e' -> Buffer.add_char buf '\x0a'; Some Unescaped
    | '\x72' -> Buffer.add_char buf '\x0d'; Some Unescaped
    | '\x74' -> Buffer.add_char buf '\x09'; Some Unescaped
    | '\x75' -> Some (Unicode [])
    | _      -> Some (Error "invalid escape sequence")

  let hex c =
    match c with
    | '0' .. '9' -> Char.code c - 0x30 (* '0' *)
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _          -> 255

  let unicode buf d = function
    | [c;b;a] ->
      let a = hex a and b = hex b and c = hex c and d = hex d in
      if a lor b lor c lor d = 255 then
        Some (Error "invalid hex escape")
      else begin
        Buffer.add_char buf Char.(unsafe_chr @@ (a lsl 4) + b);
        Buffer.add_char buf Char.(unsafe_chr @@ (c lsl 4) + d);
        Some Unescaped
      end
    | cs -> Some (Unicode (d::cs))

  let str buf =
    let state = ref Unescaped in
    skip_while (fun c ->
      match
        begin match !state with
        | Unescaped  -> unescaped buf c
        | Escaped    -> escaped   buf c
        | Unicode cs -> unicode   buf c cs
        | (Error _ | Done) -> None
        end
      with
        | Some (Error _) | None -> false
        | Some state' -> state := state'; true)
    >>= fun () ->
      match !state with
      | Done        ->
        let result = Buffer.contents buf in
        Buffer.clear buf;
        state := Unescaped;
        return result
      | Error msg ->
        Buffer.clear buf; state := Unescaped; fail msg
      | Unescaped | Escaped | UTF8 _ | UTF16 _ ->
        Buffer.clear buf; state := Unescaped; fail "unterminated string"
end

let json =
  let pair x y = (x, y) in
  let buf = Buffer.create 0x1000 in
  let str = S.str buf in
  fix (fun json ->
    let mem = lift2 pair (quo *> str <* ns) json in
    let obj = any_char *> sep_by vs mem  <* rcb >>| fun ms -> `Object ms in
    let arr = any_char *> sep_by vs json <* rsb >>| fun vs -> `Array  vs in
    let str = any_char *> str >>| fun s -> `String s in
    ws *> peek_char_fail
    >>= function
      | 'f' -> _false
      | 'n' -> _null
      | 't' -> _true
      | '{' -> obj
      | '[' -> arr
      | '"' -> str
      | _   -> num) <?> "json"
