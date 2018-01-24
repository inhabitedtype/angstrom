(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

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

module Bigarray = struct 
  (* Do not access Bigarray operations directly. If anything's needed, refer to
   * the internal Bigstring module. *)
end

type bigstring = Bigstring.t


module Unbuffered = struct
  include Parser

  type more = More.t = 
    | Complete
    | Incomplete
end

include Unbuffered
include Parser.Monad
include Parser.Choice

module Buffered = struct
  type unconsumed = Buffering.unconsumed =
    { buf : bigstring
    ; off : int
    ; len : int }

  type input =
    [ `Bigstring of bigstring
    | `String    of string ]

  type 'a state =
    | Partial of ([ input | `Eof ] -> 'a state)
    | Done    of unconsumed * 'a
    | Fail    of unconsumed * string list * string

  let from_unbuffered_state ~f buffering = function
    | Unbuffered.Partial p         -> Partial (f p)
    | Unbuffered.Done(consumed, v) ->
      let unconsumed = Buffering.unconsumed ~shift:consumed buffering in
      Done(unconsumed, v)
    | Unbuffered.Fail(consumed, marks, msg) ->
      let unconsumed = Buffering.unconsumed ~shift:consumed buffering in
      Fail(unconsumed, marks, msg)

  let parse ?(initial_buffer_size=0x1000) p =
    if initial_buffer_size < 1 then
      failwith "parse: invalid argument, initial_buffer_size < 1";
    let buffering = Buffering.create initial_buffer_size in
    let rec f p input =
      Buffering.shift buffering p.committed;
      let more : More.t =
        match input with
        | `Eof            -> Complete
        | #input as input ->
          Buffering.feed_input buffering input;
          Incomplete
      in
      let for_reading = Buffering.for_reading buffering in
      p.continue for_reading ~off:0 ~len:(Bigstring.length for_reading) more
      |> from_unbuffered_state buffering ~f
    in
    Unbuffered.parse p
    |> from_unbuffered_state buffering ~f

  let feed state input =
    match state with
    | Partial k -> k input
    | Fail(unconsumed, marks, msg) ->
      begin match input with
      | `Eof   -> state
      | #input as input ->
        let buffering = Buffering.of_unconsumed unconsumed in
        Buffering.feed_input buffering input;
        Fail(Buffering.unconsumed buffering, marks, msg)
      end
    | Done(unconsumed, v) ->
      begin match input with
      | `Eof   -> state
      | #input as input ->
        let buffering = Buffering.of_unconsumed unconsumed in
        Buffering.feed_input buffering input;
        Done(Buffering.unconsumed buffering, v)
      end

  let state_to_option = function
    | Done(_, v) -> Some v
    | _          -> None

  let state_to_result = function
    | Partial _           -> Result.Error "incomplete input"
    | Done(_, v)          -> Result.Ok v
    | Fail(_, marks, msg) -> Result.Error (Unbuffered.fail_to_string marks msg)

  let state_to_unconsumed = function
    | Done(unconsumed, _)
    | Fail(unconsumed, _, _) -> Some unconsumed
    | _                      -> None

end

let parse_bigstring p bs =
  Unbuffered.parse_bigstring p bs

let parse_string p s =
  let len = String.length s in
  let bs  = Bigstring.create len in
  Bigstring.blit_from_string s 0 bs 0 len;
  parse_bigstring p bs


(** BEGIN: getting input *)

let rec prompt input pos fail succ =
  (* [prompt] should only call [succ] if it has received more input. If there
   * is no chance that the input will grow, i.e., [more = Complete], then
   * [prompt] should call [fail]. Otherwise (in the case where the input
   * hasn't grown but [more = Incomplete] just prompt again. *)
  let parser_uncommitted_bytes = Input.parser_uncommitted_bytes input in
  let parser_committed_bytes   = Input.parser_committed_bytes   input in 
  (* The continuation should not hold any references to input above. *)
  let continue input ~off ~len more =
    if len < parser_uncommitted_bytes then
      failwith "prompt: input shrunk!";
    let input = Input.create input ~off ~len ~committed_bytes:parser_committed_bytes in
    if len = parser_uncommitted_bytes then
      match (more : More.t) with
      | Complete   -> fail input pos More.Complete
      | Incomplete -> prompt input pos fail succ
    else
      succ input pos more
  in
  Partial { committed = Input.bytes_for_client_to_commit input; continue }

let demand_input =
  { run = fun input pos more fail succ ->
    match (more : More.t) with
    | Complete   -> fail input pos more [] "not enough input"
    | Incomplete ->
      let succ' input' pos' more' = succ input' pos' more' ()
      and fail' input' pos' more' = fail input' pos' more' [] "not enough input" in
      prompt input pos fail' succ'
  }

let ensure_suspended n input pos more fail succ =
  let rec go =
    { run = fun input' pos' more' fail' succ' ->
      if pos' + n <= Input.length input' then
        succ' input' pos' more' ()
      else
        (demand_input *> go).run input' pos' more' fail' succ'
    }
  in
  (demand_input *> go).run input pos more fail succ

let unsafe_apply len ~f =
  { run = fun input pos more _fail succ ->
    succ input (pos + len) more (Input.apply input pos len ~f)
  }

let unsafe_apply_opt len ~f =
  { run = fun input pos more fail succ ->
    match Input.apply input pos len ~f with
    | Error e -> fail input pos more [] e
    | Ok    x -> succ input (pos + len) more x
  }

let ensure n =
  { run = fun input pos more fail succ ->
    if pos + n <= Input.length input then
      succ input pos more ()
    else
      ensure_suspended n input pos more fail succ }

let ensure_apply     n ~f = ensure n *> unsafe_apply     n ~f
let ensure_apply_opt n ~f = ensure n *> unsafe_apply_opt n ~f

(** END: getting input *)

let at_end_of_input =
  { run = fun input pos more _ succ ->
    if pos < Input.length input then
      succ input pos more false
    else match more with
    | Complete -> succ input pos more true
    | Incomplete ->
      let succ' input' pos' more' = succ input' pos' more' false
      and fail' input' pos' more' = succ input' pos' more' true in
      prompt input pos fail' succ'
  }

let end_of_input =
  at_end_of_input
  >>= function
    | true  -> return ()
    | false -> fail "end_of_input"

let advance n =
  if n < 0
  then fail "advance"
  else { run = fun input pos more _fail succ -> succ input (pos + n) more () }

let pos =
  { run = fun input pos more _fail succ -> succ input pos more pos }

let available =
  { run = fun input pos more _fail succ ->
    succ input pos more (Input.length input - pos)
  }

let commit =
  { run = fun input pos more _fail succ ->
    Input.commit input pos;
    succ input pos more () }

(* Do not use this if [p] contains a [commit]. *)
let unsafe_lookahead p =
  { run = fun input pos more fail succ ->
    let succ' input' _ more' v = succ input' pos more' v in
    p.run input pos more fail succ' }

let peek_char =
  { run = fun input pos more _fail succ ->
    if pos < Input.length input then
      succ input pos more (Some (Input.get_char input pos))
    else if more = Complete then
      succ input pos more None
    else
      let succ' input' pos' more' =
        succ input' pos' more' (Some (Input.get_char input' pos'))
      and fail' input' pos' more' =
        succ input' pos' more' None in
      prompt input pos fail' succ'
  }

let _char ~msg f =
  { run = fun input pos more fail succ ->
    if pos < Input.length input then
      match f (Input.get_char input pos) with
      | None   -> fail input pos more [] msg
      | Some v -> succ input (pos + 1) more v
    else
      let succ' input' pos' more' () =
        match f (Input.get_char input' pos') with
        | None   -> fail input' pos' more' [] msg
        | Some v -> succ input' (pos' + 1) more' v
      in
      ensure_suspended 1 input pos more fail succ'
  }

(* Like _char but specialized for parsers that just return the character that
 * satisfies [f]. Avoids an allocation. *)
let _char_pred ~msg f =
  { run = fun input pos more fail succ ->
    if pos < Input.length input then
      let c = Input.get_char input pos in
      if f c
      then succ input (pos + 1) more c
      else fail input pos more [] msg
    else
      let succ' input' pos' more' () =
        let c = Input.get_char input' pos' in
        if f c
        then succ input' (pos' + 1) more' c
        else fail input' pos' more' [] msg
      in
      ensure_suspended 1 input pos more fail succ' }

(* This parser is too important to not be optimized. Do a custom job. *)
let rec peek_char_fail =
  { run = fun input pos more fail succ ->
    if pos < Input.length input
    then succ input pos more (Input.get_char input pos)
    else
      let succ' input' pos' more' () =
        peek_char_fail.run input' pos' more' fail succ in
      ensure_suspended 1 input pos more fail succ' }

let satisfy f =
  _char_pred ~msg:"satisfy" f

let char c =
  satisfy (fun c' -> c = c') <?> (String.make 1 c)

let not_char c =
  satisfy (fun c' -> c <> c') <?> ("not " ^ String.make 1 c)

let any_char =
  _char_pred ~msg:"any_char" (fun _ -> true)

let any_uint8 =
  _char ~msg:"any_uint8" (fun c -> Some (Char.code c))

let any_int8 =
  (* https://graphics.stanford.edu/~seander/bithacks.html#VariableSignExtendRisky *)
  let s = Sys.int_size - 8 in
  _char ~msg:"any_int8" (fun c -> Some ((Char.code c lsl s) asr s))

let skip f =
  _char ~msg:"skip" (fun c -> if f c then Some () else None)

let rec count_while ~init ~f ~with_buffer =
  { run = fun input pos more fail succ ->
    let len         = Input.count_while input (pos + init) ~f in
    let input_len   = Input.length input in
    let init'       = init + len in
    (* Check if the loop terminated because it reached the end of the input
     * buffer. If so, then prompt for additional input and continue. *)
    if pos + init' < input_len || more = Complete
    then succ input (pos + init') more (Input.apply input pos init' ~f:with_buffer)
    else
      let succ' input' pos' more' =
        (count_while ~init:init' ~f ~with_buffer).run input' pos' more' fail succ
      and fail' input' pos' more' =
        succ input' (pos' + init') more' (Input.apply input' pos' init' ~f:with_buffer)
      in
      prompt input pos fail' succ'
  }

let rec count_while1 ~f ~with_buffer =
  { run = fun input pos more fail succ ->
    let len         = Input.count_while input pos ~f in
    let input_len   = Input.length input in
    (* Check if the loop terminated because it reached the end of the input
     * buffer. If so, then prompt for additional input and continue. *)
    if len < 1
    then 
      if pos < input_len || more = Complete
      then fail input pos more [] "count_while1"
      else
        let succ' input' pos' more' =
          (count_while1 ~f ~with_buffer).run input' pos' more' fail succ
        and fail' input' pos' more' =
          fail input' pos' more' [] "count_while1"
        in
        prompt input pos fail' succ'
    else if pos + len < input_len || more = Complete
    then succ input (pos + len) more (Input.apply input pos len ~f:with_buffer)
    else
      let succ' input' pos' more' =
        (count_while ~init:len ~f ~with_buffer).run input' pos' more' fail succ
      and fail' input' pos' more' =
        succ input' (pos' + len) more' (Input.apply input' pos' len ~f:with_buffer)
      in
      prompt input pos fail' succ'
  }

let string_ f s =
  (* XXX(seliopou): Inefficient. Could check prefix equality to short-circuit
   * the io. *)
  let len = String.length s in
  ensure_apply_opt len ~f:(fun buffer ~off ~len ->
    let i = ref 0 in
    while !i < len && Char.equal (f (Bigstring.unsafe_get buffer (off + !i)))
                                 (f (String.unsafe_get s !i))
    do
      incr i
    done;
    if len = !i
    then Ok (Bigstring.substring buffer ~off ~len)
    else Error "string")

let string s    = string_ (fun x -> x) s
let string_ci s = string_ Char.lowercase_ascii s

let skip_while f =
  count_while ~init:0 ~f ~with_buffer:(fun _ ~off:_ ~len:_ -> ())

let take n =
  ensure_apply (max n 0) ~f:Bigstring.substring

let take_bigstring n =
  ensure_apply (max n 0) ~f:Bigstring.copy

let take_bigstring_while f =
  count_while ~init:0 ~f ~with_buffer:Bigstring.copy

let take_bigstring_while1 f =
  count_while1 ~f ~with_buffer:Bigstring.copy

let take_bigstring_till f =
  take_bigstring_while (fun c -> not (f c))

let peek_string n =
  unsafe_lookahead (take n)

let take_while f =
  count_while ~init:0 ~f ~with_buffer:Bigstring.substring

let take_while1 f =
  count_while1 ~f ~with_buffer:Bigstring.substring

let take_till f =
  take_while (fun c -> not (f c))

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

let rec list ps =
  match ps with
  | []    -> return []
  | p::ps -> lift2 cons p (list ps)

let count n p =
  if n < 0 then
    failwith "count: invalid argument, n < 0";
  let rec loop = function
    | 0 -> return []
    | n -> lift2 cons p (loop (n - 1))
  in
  loop n

let many p =
  fix (fun m ->
    (lift2 cons p m) <|> return [])

let many1 p =
  lift2 cons p (many p)

let many_till p t =
  fix (fun m ->
    (t *> return []) <|> (lift2 cons p m))

let sep_by1 s p =
  fix (fun m ->
    lift2 cons p ((s *> m) <|> return []))

let sep_by s p =
  (lift2 cons p ((s *> sep_by1 s p) <|> return [])) <|> return []

let skip_many p =
  fix (fun m ->
    (p *> m) <|> return ())

let skip_many1 p =
  p *> skip_many p

let end_of_line =
  (char '\n' *> return ()) <|> (string "\r\n" *> return ()) <?> "end_of_line"

let scan_ state f ~with_buffer =
  { run = fun input pos more fail succ ->
    let state = ref state in
    let parser =
      count_while ~init:0 ~f:(fun c ->
        match f !state c with
        | None -> false
        | Some state' -> state := state'; true)
      ~with_buffer
      >>| fun x -> x, !state
    in
    parser.run input pos more fail succ }

let scan state f =
  scan_ state f ~with_buffer:Bigstring.substring

let scan_state state f =
  scan_ state f ~with_buffer:(fun _ ~off:_ ~len:_ -> ())
  >>| fun ((), state) -> state

let scan_string state f =
  scan state f >>| fst

module BE = struct
  let uint16 = ensure_apply 2 ~f:(fun bs ~off ~len:_ -> Bigstring.unsafe_get_u16_be bs ~off)
  let int16  = ensure_apply 2 ~f:(fun bs ~off ~len:_ -> Bigstring.unsafe_get_16_be  bs ~off)

  let int32  = ensure_apply 4 ~f:(fun bs ~off ~len:_ -> Bigstring.unsafe_get_32_be bs ~off)
  let int64  = ensure_apply 8 ~f:(fun bs ~off ~len:_ -> Bigstring.unsafe_get_64_be bs ~off)

  let float  = ensure_apply 4 ~f:(fun bs ~off ~len:_ -> Int32.float_of_bits (Bigstring.unsafe_get_32_be bs ~off))
  let double = ensure_apply 8 ~f:(fun bs ~off ~len:_ -> Int64.float_of_bits (Bigstring.unsafe_get_64_be bs ~off))
end

module LE = struct
  let uint16 = ensure_apply 2 ~f:(fun bs ~off ~len:_ -> Bigstring.unsafe_get_u16_le bs ~off)
  let int16  = ensure_apply 2 ~f:(fun bs ~off ~len:_ -> Bigstring.unsafe_get_16_le  bs ~off)

  let int32  = ensure_apply 4 ~f:(fun bs ~off ~len:_ -> Bigstring.unsafe_get_32_le bs ~off)
  let int64  = ensure_apply 8 ~f:(fun bs ~off ~len:_ -> Bigstring.unsafe_get_64_le bs ~off)

  let float  = ensure_apply 4 ~f:(fun bs ~off ~len:_ -> Int32.float_of_bits (Bigstring.unsafe_get_32_le bs ~off))
  let double = ensure_apply 8 ~f:(fun bs ~off ~len:_ -> Int64.float_of_bits (Bigstring.unsafe_get_64_le bs ~off))
end

module Unsafe = struct
  let take n f =
    ensure_apply (max n 0) ~f

  let peek n f =
    unsafe_lookahead (take n f)

  let take_while check f =
    count_while ~init:0 ~f:check ~with_buffer:f

  let take_while1 check f =
    count_while1 ~f:check ~with_buffer:f

  let take_till check f =
    take_while (fun c -> not (check c)) f
end
