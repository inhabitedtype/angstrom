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

module R = Result
open Angstrom
open Core.Std
open Async.Std

let rec finalize state result =
  match state, result with
  | state    , `Eof_with_unconsumed_data str -> finalize (feed state (`String str)) `Eof
  | Partial k, `Eof                          -> finalize (k None)                   `Eof
  | Partial _, `Stopped ()                   -> assert false
  | (Fail(buf, _, _) | Done(buf, _)) as state, (`Eof | `Stopped ()) ->
    (* buf may contain some data, but there's another copy of it in reader's
     * internal buffer. *)
    buf, state_to_result state

let parse ?initial_input p reader =
  let state = ref (parse ?input:initial_input p) in
  let handle_chunk (buf:Bigstring.t) ~pos ~len =
    state := feed !state (`Cstruct (Cstruct.of_bigarray ~len ~off:pos buf));
    (* These statements apply both to the Fail and Done cases:
     *
     * if len_left > len, then the parser was buffering input that can't be
     * written back to the reader.
     *
     * if len_left < len, then the parser consumed input and writing back to
     * the reader would result in a gap in the input.
     *)
    match !state with
    | Done(buf, r) ->
      let len_left = Cstruct.len buf in
      if len_left <= len then begin
        state := Done(Cstruct.create 0, r);
        return (`Stop_consumed((), len - len_left))
      end else
        return (`Stop ())
    | Fail(buf, tags, msg) ->
      let len_left = Cstruct.len buf in
      if len_left = len then begin
        state := Fail(Cstruct.create 0, tags, msg);
        return (`Stop_consumed((), 0))
      end else
        return (`Stop ())
    | Partial k ->
      return `Continue
  in
  Reader.read_one_chunk_at_a_time reader ~handle_chunk >>| fun result ->
    finalize !state result

let rec parse_many ?initial_input p reader k =
  parse ?initial_input p reader
  >>= function
    | buf, R.Ok a    ->
      k a >>= fun () ->
      parse_many ~initial_input:(`Cstruct buf) p reader k
    | buf, R.Error a ->
      return (buf, R.Error a)
