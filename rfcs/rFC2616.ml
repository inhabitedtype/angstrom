open Angstrom

module P = struct
  let is_space =
    function | ' ' | '\t' -> true | _ -> false

  let is_eol c = c = '\r' || c = '\n'

  let is_hex =
    function | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

  let is_colon c = c = ':'

  let is_colon_or_space c =
    is_space c || is_colon c

  let is_digit =
    function '0' .. '9' -> true | _ -> false

  let is_separator =
    let ss = "()<>@,;:\\\"/[]?={} \t" in
    let len = String.length ss in
    let rec loop c i =
      if i >= len then false
      else if c = String.unsafe_get ss i then true
      else loop c (i + 1)
    in
    fun c -> loop c 0

  let is_token c =
    let i = Char.code c in
    i > 31 && i <> 127 && not (is_separator c)
end

let token = take_while1 P.is_token
let digits = take_while1 P.is_digit
let spaces = skip_while P.is_space

let lex p = p <* spaces

let version =
  string "HTTP/" *> begin
  (fun major minor -> major, minor)
    <$> (digits <* char '.')
    <*> digits
  end

let uri =
  take_till P.is_space

let meth = token
let eol = string "\r\n"

let request_first_line =
  (fun meth uri version -> (meth, uri, version))
    <$> lex meth
    <*> lex uri
    <*> version

let response_first_line =
  (fun version status msg -> (version, status, msg))
    <$> lex version
    <*> lex (take_till P.is_space)
    <*> take_till P.is_eol

let header =
  let colon = spaces *> char ':' <* spaces in
  (fun key value -> (key, value))
    <$> token
    <*> colon *> take_till P.is_eol

let request =
  (fun (meth, uri, version) headers -> (meth, uri, version, headers))
    <$> (request_first_line   <* eol)
    <*> (many (header <* eol) <* eol)

let response =
  (fun (version, status, msg) headers -> (version, status, msg, headers))
   <$> (response_first_line  <* eol)
   <*> (many (header <* eol) <* eol)
