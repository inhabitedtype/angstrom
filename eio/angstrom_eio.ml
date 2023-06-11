(*----------------------------------------------------------------------------
    Copyright (c) 2023 Inhabited Type LLC.

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

open Angstrom.Buffered

let default_buffer_size = 4096

let default_pushback () = ()

let handle_parse_result state =
  match state_to_unconsumed state with
  | None -> assert false
  | Some us -> us, state_to_result state

let finalize = function
| Partial feed -> feed `Eof
| state -> state

let parse ?(pushback = default_pushback) parser src =
  let buf = Cstruct.create default_buffer_size in
  let rec loop = function
    | (Done _ as state)
    | (Fail _ as state) ->
      handle_parse_result state
    | Partial feed as state -> (
      match Eio.Flow.single_read src buf with
      | 0
      | (exception End_of_file) ->
        finalize state |> handle_parse_result
      | len ->
        let next = feed (`Bigstring (Bigstringaf.sub buf.buffer ~off:0 ~len)) in
        pushback ();
        loop next )
  in
  loop (parse parser)

let rec buffered_state_loop pushback state src (buf : Cstruct.t) =
  match state with
  | Partial k ->
    let next =
      match Eio.Flow.single_read src buf with
      | 0
      | (exception End_of_file) ->
        k `Eof
      | len -> k (`Bigstring (Bigstringaf.sub buf.buffer ~off:0 ~len))
    in
    pushback ();
    buffered_state_loop pushback next src buf
  | state -> state

let with_buffered_parse_state ?(pushback = default_pushback) state src =
  let buf = Cstruct.create default_buffer_size in
  ( match state with
  | Partial _ -> buffered_state_loop pushback state src buf
  | _ -> state )
  |> handle_parse_result

let async_many e k = Angstrom.(skip_many (e <* commit >>| k) <?> "async_many")

let parse_many p write src =
  let wait = ref (default_pushback ()) in
  let k x = wait := write x in
  let pushback () = !wait in
  parse ~pushback (async_many p k) src
