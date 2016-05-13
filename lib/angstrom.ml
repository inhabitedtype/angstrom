(*----------------------------------------------------------------------------
    Copyright (c) 2015 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module A = Bigarray.Array1
module B = struct
  (** XXX(seliopou): Look into replacing this with a circular buffer. *)
  type t =
    { mutable buffer : Cstruct.t
    ; mutable offset : int
    }

  let reuse buffer =
    { buffer; offset = 0 }

  let of_string str =
    let buffer = Cstruct.of_string str in
    reuse buffer

  let of_bigarray ?off ?len bytes =
    let buffer = Cstruct.of_bigarray ?off ?len bytes in
    reuse buffer

  let create ?(size=0x1000) () =
    let buffer = Cstruct.(set_len (create size) 0) in
    reuse buffer

  let _writable_space t =
    let { Cstruct.buffer; len } = t.buffer in
    A.dim buffer - len

  let _trailing_space t =
    let { Cstruct.buffer; off; len } = t.buffer in
    A.dim buffer - (off + len)

  let debug t =
    Printf.sprintf "debug - buf: %s, trailing: %d, writable: %d\n%!"
      (Cstruct.debug t.buffer) (_trailing_space t) (_writable_space t)

  let _compress t =
    let off, len = 0, Cstruct.len t.buffer in
    let buffer = Cstruct.of_bigarray ~off ~len t.buffer.Cstruct.buffer in
    Cstruct.blit t.buffer 0 buffer 0 len;
    t.buffer <- buffer

  let _grow t to_copy =
    let init_size = A.dim t.buffer.Cstruct.buffer in
    let size  = ref init_size in
    let space = _writable_space t in
    while space + !size - init_size < to_copy do
      size := (3 * !size) / 2
    done;
    let buffer = Cstruct.(set_len (create !size)) t.buffer.Cstruct.len in
    Cstruct.blit t.buffer 0 buffer 0 t.buffer.Cstruct.len;
    t.buffer <- buffer
  ;;

  let _ensure_space t len =
    begin if _trailing_space t >= len then
      () (* there is enough room at the end *)
    else if _writable_space t >= len then
      _compress t
    else
      _grow t len
    end;
    t.buffer <- Cstruct.add_len t.buffer len
  ;;

  let copy_in t =
    function
    | `String str ->
      let len = String.length str in
      _ensure_space t len;
      let len' = Cstruct.len t.buffer - len in
      let allocator _ = Cstruct.sub t.buffer len' len in
      ignore (Cstruct.of_string ~allocator str)
    | `Cstruct cs ->
      let len = Cstruct.len cs in
      _ensure_space t len;
      let len' = Cstruct.len t.buffer - len in
      Cstruct.blit cs 0 t.buffer len' len

  let commit t pos =
    t.buffer <- Cstruct.shift t.buffer (pos - t.offset);
    t.offset <- pos

  let input_length t =
    Cstruct.len t.buffer + t.offset

  let get t i =
    Cstruct.get_char t.buffer (i - t.offset)

  let substring t pos len =
    Cstruct.copy t.buffer (pos - t.offset) len

  let count_while t pos f =
    let i = ref pos in
    let len = input_length t in
    while !i < len && f (get t !i) do
      incr i
    done;
    !i - pos

  let unread t pos =
    Cstruct.shift t.buffer (pos - t.offset)
end

type more =
  | Complete
  | Incomplete

type input =
  [ `String  of string
  | `Cstruct of Cstruct.t ]

type 'a state =
  | Fail    of Cstruct.t * string list * string
  | Partial of (input option -> 'a state)
  | Done    of Cstruct.t * 'a

type 'a failure = B.t -> int -> more -> string list -> string -> 'a state
type ('a, 'r) success = B.t -> int -> more -> 'a -> 'r state

let fail_k    buf pos _ marks msg = Fail(B.unread buf pos, marks, msg)
let succeed_k buf pos _       v   = Done(B.unread buf pos, v)

type 'a t =
  { run : 'r. B.t -> int -> more -> 'r failure -> ('a, 'r) success -> 'r state }

let return : type a. a -> a t =
  fun v -> { run = fun buf pos more _fail succ -> succ buf pos more v }

let fail msg =
  { run = fun buf pos more fail succ ->
    fail buf pos more [] msg
  }

let (>>=) p f =
  { run = fun buf pos more fail succ ->
    let succ' buf' pos' more' v = (f v).run buf' pos' more' fail succ in
    p.run buf pos more fail succ'
  }

let (>>|) p f =
  { run = fun buf pos more fail succ ->
    let succ' buf' pos' more' v = succ buf' pos' more' (f v) in
    p.run buf pos more fail succ'
  }

let (<$>) f m =
  m >>| f

let (<*>) f m =
  f >>= fun f ->
  m >>| f

let ( *>) a b =
  a >>= fun _ -> b

let (<* ) a b =
  a >>= fun x ->
  b >>| fun _ -> x

let (<?>) p mark =
  { run = fun buf pos more fail succ ->
    let fail' buf' pos' more' marks msg = fail buf' pos' more' (mark::marks) msg in
    p.run buf pos more fail' succ
  }

let (<|>) p q =
  { run = fun buf pos more fail succ ->
    let fail' buf' pos' more' _marks _msg = q.run buf' pos more' fail succ in
    p.run buf pos more fail' succ
  }

(** BEGIN: getting input *)

let rec prompt buf pos fail succ =
  let k = function
    | None       -> fail buf pos Complete
    | Some (`String "") ->
      prompt buf pos fail succ
    | Some (`Cstruct cs) when Cstruct.len cs = 0 ->
      prompt buf pos fail succ
    | Some input ->
      B.copy_in buf input;
      succ buf pos Incomplete
  in
  Partial k

let demand_input =
  { run = fun buf pos more fail succ ->
    match more with
    | Complete   -> fail buf pos more [] "not enough input"
    | Incomplete ->
      let succ' buf' pos' more' = succ buf' pos' more' ()
      and fail' buf' pos' more' = fail buf' pos' more' [] "not enough input" in
      prompt buf pos fail' succ'
  }

let want_input =
  { run = fun buf pos more _fail succ ->
    if pos < B.input_length buf then
      succ buf pos more true
    else if more = Complete then
      succ buf pos more false
    else
      let succ' buf' pos' more' = succ buf' pos' more' true
      and fail' buf' pos' more' = succ buf' pos' more' false in
      prompt buf pos fail' succ'
  }

let ensure_suspended n buf pos more fail succ =
  let rec go =
    { run = fun buf' pos' more' fail' succ' ->
      if pos' + n <= B.input_length buf' then
        succ' buf' pos' more' ()
      else
        (demand_input >>= fun () -> go).run buf' pos' more' fail' succ'
    }
  in
  (demand_input >>= fun () -> go).run buf pos more fail succ

let unsafe_substring n =
  { run = fun buf pos more fail succ ->
    succ buf pos more (B.substring buf pos n)
  }

let ensure n =
  { run = fun buf pos more fail succ ->
    if pos + n <= B.input_length buf then
      succ buf pos more ()
    else
      ensure_suspended n buf pos more fail succ
  }
  >>= fun () -> unsafe_substring n


(** END: getting input *)

let end_of_input =
  { run = fun buf pos more fail succ ->
    if pos < B.input_length buf then
      fail buf pos more [] "end_of_input"
    else if more = Complete then
      succ buf pos more ()
    else
      let succ' buf' pos' more' = fail buf' pos' more' [] "end_of_input"
      and fail' buf' pos' more' = succ buf' pos' more' () in
      prompt buf pos fail' succ'
  }

let end_of_buffer =
  { run = fun buf pos more fail succ ->
    succ buf pos more (pos = B.input_length buf)
  }

let spans_chunks n =
  { run = fun buf pos more fail succ ->
    if pos + n < B.input_length buf || more = Complete then
      succ buf pos more false
    else
      let succ' buf' pos' more' = succ buf' pos' more' true
      and fail' buf' pos' more' = succ buf' pos' more' false in
      prompt buf pos fail' succ'
  }

let advance n =
  { run = fun buf pos more _fail succ -> succ buf (pos + n) more () }

let pos =
  { run = fun buf pos more _fail succ -> succ buf pos more pos }

let available =
  { run = fun buf pos more _fail succ ->
    succ buf pos more (B.input_length buf - pos)
  }

let get_buffer_and_pos =
  { run = fun buf pos more _fail succ -> succ buf pos more (buf, pos) }

let commit =
  { run = fun buf pos more _fail succ ->
    B.commit buf pos;
    succ buf pos more ()
  }

let peek_char =
  { run = fun buf pos more fail succ ->
    if pos < B.input_length buf then
      succ buf pos more (Some (B.get buf pos))
    else if more = Complete then
      succ buf pos more None
    else
      let succ' buf' pos' more' = succ buf' pos' more' (Some (B.get buf' pos'))
      and fail' buf' pos' more' = succ buf' pos' more' None in
      prompt buf pos fail' succ'
  }

let peek_char_fail =
  { run = fun buf pos more fail succ ->
    if pos < B.input_length buf then
      succ buf pos more (B.get buf pos)
    else
      let succ' buf' pos' more' () = succ buf' pos' more' (B.get buf' pos') in
      ensure_suspended 1 buf pos more fail succ'
  }

let satisfy f =
  peek_char_fail >>= fun c ->
    if f c
      then advance 1 >>| fun () -> c
      else fail "satisfy"

let skip f =
  peek_char_fail >>= fun c ->
    if f c
      then advance 1
      else fail "skip"

let count_while ?(init=0) f =
  (* NB: does not advance position. *)
  let rec go acc =
    get_buffer_and_pos >>= fun (buf, pos) ->
      let n = B.count_while buf (pos + acc) f in
      spans_chunks n
      >>= function
        | true  -> go (n + acc)
        | false -> return (n + acc)
  in
  go init

let string_ f s =
  (* XXX(seliopou): Inefficient. Could check prefix equality to short-circuit
   * the io. *)
  let len = String.length s in
  ensure len >>= fun s'->
    if f s = f s'
      then advance len *> return s'
      else fail "string"

let string s    = string_ (fun x -> x) s
let string_ci s = string_ String.lowercase s

let skip_while f =
  count_while f >>= advance

let take n =
  let n = max n 0 in
  ensure  n >>= fun str ->
  advance n >>| fun () ->
    str

let take_while f =
  count_while f >>= fun n ->
  unsafe_substring n >>= fun str ->
  advance n >>| fun () ->
    str

let take_while1 f =
  end_of_buffer
  >>= begin function
    | true  -> demand_input
    | false -> return ()
  end >>= fun () ->
  get_buffer_and_pos
  >>= fun (buf, pos) ->
    let init = B.count_while buf pos f in
    if init = 0 then
      fail "take_while1"
    else
      count_while ~init f >>= fun n ->
      unsafe_substring n >>= fun str ->
      advance n >>| fun () ->
        str

let take_till f =
  take_while (fun c -> not (f c))

let take_rest =
  let rec go acc =
    want_input >>= function
      | true  ->
        available >>= fun n ->
        unsafe_substring n >>= fun str ->
        advance n >>= fun () ->
          go (str::acc)
      | false ->
        return (List.rev acc)
  in
  go []

let char c =
  satisfy (fun c' -> c = c') <?> (String.make 1 c)

let not_char c =
  satisfy (fun c' -> c <> c') <?> ("not " ^ String.make 1 c)

let any_char =
  satisfy (fun _ -> true)

let choice ps =
  List.fold_right (<|>) ps (fail "empty")

let fix f =
  let rec p = lazy (f r)
  and r = { run = fun buf pos more fail succ ->
    Lazy.(force p).run buf pos more fail succ }
  in
  r

let option x p =
  p <|> return x

let cons x xs = x :: xs

let many p =
  fix (fun m ->
    (return cons <*> p <*> m) <|> return [])

let many1 p =
  return cons <*> p <*> many p

let many_till p t =
  fix (fun m ->
    (return cons <*> p <*> m) <|> (t *> return []))

let sep_by1 s p =
  fix (fun m ->
    cons <$> p <*> ((s *> m) <|> return []))

let sep_by s p =
  (cons <$> p <*> ((s *> sep_by1 s p) <|> return [])) <|> return []

let rec list ps =
  match ps with
  | []    -> return []
  | p::ps -> return cons <*> p <*> list ps

let end_of_line =
  (char '\n' *> return ()) <|> (string "\r\n" *> return ()) <?> "end_of_line"

let parse ?(initial_buffer_size=0x1000) ?(input=`String "") p =
  let buf  = B.create ~size:initial_buffer_size () in
  B.copy_in buf input;
  p.run buf 0 Incomplete fail_k succeed_k

let parse_with_buffer p buf =
  p.run (B.reuse buf) 0 Incomplete fail_k succeed_k

let parse_only p input =
  let buf = B.create () in
  B.copy_in buf input;
  match p.run buf 0 Complete fail_k succeed_k with
  | Fail(_, []   , err) -> Result.Error err
  | Fail(_, marks, err) -> Result.Error (String.concat " > " marks ^ ": " ^ err)
  | Done(_, v)          -> Result.Ok v
  | Partial _           -> assert false

let copy_into_leftover l bytes =
  let buf  = B.reuse l in
  B.copy_in buf bytes;
  buf.B.buffer

let feed state bytes =
  match state with
  | Fail (l, marks, msg) -> Fail(copy_into_leftover l bytes, marks, msg)
  | Partial k            -> k (Some bytes)
  | Done (l, v)          -> Done(copy_into_leftover l bytes, v)

let state_to_option = function
  | Done (_, v) -> Some v
  | _           -> None

let state_to_result = function
  | Done (buf', v)          -> Result.Ok v
  | Partial _               -> Result.Error "incomplete input"
  | Fail (buf', marks, err) -> Result.Error (String.concat " > " marks ^ ": " ^ err)
