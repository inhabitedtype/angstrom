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

let lsb, rsb = char '[', lchar ']'
let lcb, rcb = char '{', lchar '}'
let ns , vs  = lchar ':', lchar ','
let quo = lchar '"'

let _false : json t = string "false" *> return `False
let _true  : json t = string "true"  *> return `True
let _null  : json t = string "null"  *> return `Null

let num =
  let chars = take_while1 (function
    | '\x20' | '\x0a' | '\x0d' | '\x09'
    | '[' | ']' | '{' | '}' | ':' | ',' -> false
    | _               -> true)
  in
  chars
  >>= fun s ->
    try return (`Number (float_of_string s))
    with _ -> fail "number"

let hex =
  satisfy (function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false)

let _str =
  let esc = char '\x5c' in
  let unescaped =
    (* not '"' nor '\' *)
    take_while1 (function | '"' | '\x5c' -> false | _ -> true) <?> "unescaped char"
  in
  let escaped =
    begin esc *> any_char
    >>= function
      | '\x22' -> return "\x22"
      | '\x5c' -> return "\x5c"
      | '\x2f' -> return "\x2f"
      | '\x62' -> return "\x08"
      | '\x66' -> return "\x0c"
      | '\x6e' -> return "\x0a"
      | '\x72' -> return "\x0d"
      | '\x74' -> return "\x09"
      | '\x75' ->
        lift4 (fun a b c d ->
          Printf.sprintf "%c%c"
            Char.(chr (0x10 * (code a) + (code b)))
            Char.(chr (0x10 * (code c) + (code d))))
          hex hex hex hex
      | _     -> fail "invalid escape sequence"
    end <?> "escaped char"
  in
  let chars = many (unescaped <|> escaped) >>| String.concat "" in
  quo *> chars <* quo

let str =
  (_str >>| fun s -> `String s) <?> "str"

let json =
  let pair x y = (x, y) in
  fix (fun json ->
    let member = lift2 pair (_str <* ns) json in
    let obj = lcb *> sep_by vs member <* rcb >>| fun ms -> `Object ms in
    let arr = lsb *> sep_by vs json   <* rsb >>| fun vs -> `Array  vs in
    ws *> peek_char_fail
    >>= function
      | 'f' -> _false
      | 'n' -> _null
      | 't' -> _true
      | '{' ->  obj
      | '[' -> arr
      | '"' -> str
      | _   -> num) <?> "json"
