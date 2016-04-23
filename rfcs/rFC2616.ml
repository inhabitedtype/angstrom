open Angstrom

module P = struct
  let is_space c =
    c = ' ' || c = '\t'
  let is_eol c = c = '\r' || c = '\n'
  let is_digit c =
    let i = Char.code c in
    0x30 <= i & i <= 0x39
  let is_hex c =
    let i = Char.code c in
    (0x30 <= i && i <= 0x39)
    || (0x41 <= i && i <= 0x46)
    || (0x61 <= i && i <= 0x66)
  let is_colon c =
    c = ':'
  let is_colon_or_space c =
    is_space c || is_colon c

  let is_separator, is_token =
    let separators = "()<>@,;:\\\"/[]?={} \t" in
    let is_separator c = String.contains separators c in
    let is_token c = c <> 127 && c > 31 && not (String.contains separators c)
    (is_separator, is_token)
end

let token = satisfy P.is_token
let digits = take_while1 P.is_digit
let separator = satisfy P.is_separator
let spaces = skip_while P.is_space
let hex str =
  try return (Int64.of_string ("0x" ^ str)) with _ -> fail "hex"

let lex p = p <* spaces

let version =
  string "HTTP/" *> begin
  (fun major minor -> major, minor)
    <$> (digits <* char '.')
    <*> digits
  end

let uri =
  (fun uri -> try Uri.of_string uri with _ -> fail "uri")
    <$> take_till P.is_space

let meth = lex token
let eol = end_of_line

let request_first_line =
  (fun meth uri version -> (meth, uri, version)
    <$> lex meth
    <*> lex uri
    <*> lex version

let response_first_line =
  commit @@ (fun version status msg -> (version, status, msg))
    <$> lex version
    <*> take_till P.is_space
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
  (fun (version, status, msg) headers -> (vesion, status, msg, headers))
   <$> (response_first_line   <* eol)
   <*> (many (header <* eol)  <* eol)

module K = struct
  let fixed_body ?size len k =
    let rec loop len =
      if len = 0 then
        return ()
      else
        commit (begin match size with
        | None      -> available >>| fun size -> min len size
        | Some size -> return (min len size)
        end >>= take)
        >>= fun buf  ->
          k buf;
          loop (len - size)
    in
    loop len

  let chunked_body ?size k =
    let chunk =
      commit (take_while1 P.is_hex <* semi >>= hex)
      >>= fun size ->
        if size = 0L
          then many skip_line *> return ()
          else fixed_body size k <* end_of_line
    in
    many1 (commit chunk)

  let unknown_body k : unit t =
    let chunk =
      want_input
      >>= function
        | true  -> available >>= fun n -> take n >>| k
        | false -> fail "chunk"
    in
    (* XXX(seliopou): better make sure this isn't used with a keep-alive
     * connection. *)
    many (commit chunk) *> commit end_of_input

  let body ?size encoding k : unit t =
    match encoding with
    | `Unknown   -> unknown_body k
    | `Fixed len -> fixed_body ?size len k
    | `Chunked   -> chunked_body ?size k

  let accum_body p =
    let chunks = ref [] in
    let k c = chunks := c :: !chunks in
    p k >>| fun () ->
      match !chunks with
      | []  -> `Empty
      | [c] -> `String c
      | cs  -> `Strings (List.rev cs)
end

let body ?size encoding =
  K.accum_body (K.body ?size encoding)
