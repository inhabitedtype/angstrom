type t =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of float
  | `Object of (string * t) list
  | `Array of t list ]

let ws = take_while (fun c ->
  c = '\x20' || c = '\x0a' || c = '\x0d' || c = '\x09')

let lchar c =
  ws *> char c <* ws

let lsb, rsb = lchar '\x5b', lchar '\x5d'
let lcb, rcb = lchar '\x7b', lchar '\x7d'
let ns , vs  = lchar '\x3a', lchar '\x2c'
let quo = lchar '\x22'
let esc = lchar '\x5c'

let _false = string "false" *> return `False
let _true  = string "true"  *> return `True
let _null  = string "null"  *> return `Null

let num =
  let str = take_while (fun c ->
    c <> '\x20' && c <> '\x0a' && c <> '\x0d' && c <> '\x09'
    && c <> '\x5d' && c <> '\x7d' && c <> '\x3a' && c <> '\x2c')
  in
  str
  >>= function
    | "" -> fail "no input for number"
    | s  -> return (`Number (float_of_string s))

let hex =
  satisfy (fun c ->
    let b = Char.code c in
    (0x30 <= b && b <= 0x39) || (0x41 <= b && b <= 0x46) || (0x61 <= b && b <= 66))

let _str =
  let unescaped =
    (* not '"' or '\' *)
    take_while (fun c -> c <> '\x22' && c <> '\x5c') <?> "unescaped char"
  in
  let escaped =
    esc *>  choice [
      char '\x22' *> return "\x22"
    ; char '\x5c' *> return "\x5c"
    ; char '\x2f' *> return "\x2f"
    ; char '\x62' *> return "\x08"
    ; char '\x66' *> return "\x0c"
    ; char '\x6e' *> return "\x0a"
    ; char '\x72' *> return "\x0d"
    ; char '\x74' *> return "\x09"
    ; char '\x75' *> begin
      (fun a b c d ->
        Printf.sprintf "%c%c"
          Char.(chr (0x10 * (code a) + (code b)))
          Char.(chr (0x10 * (code c) + (code d))))
        <$> hex <*> hex <*> hex <*> hex
      end
    ] <?> "escaped char"
  in
  let chars =
    many (choice [unescaped; escaped] <* commit) >>| String.concat ""
  in
  quo *> chars <* quo

let str =
  (_str >>| fun s -> `String s) <?> "str"

let value =
  fix (fun v ->
    let obj =
      let member = return (fun x y -> (x, y)) <*> _str <* ns <*> v in
      lcb *> sep_by vs member <* rcb >>| fun ms -> `Object ms
    in
    let arr =
      lsb *> sep_by vs v <* rsb >>| fun vs -> `Array vs
    in
    choice [_false; _null; _true; obj; arr; str; num])

let parser =
  ws *> value <* ws
