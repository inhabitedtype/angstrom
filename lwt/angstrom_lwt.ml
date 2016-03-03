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

open Angstrom
open Lwt.Infix

let parse ?initial_input p in_chan =
  let size  = Lwt_io.buffer_size in_chan in
  let bytes = Bytes.create size in
  let rec loop = function
    | Partial k ->
      Lwt_io.read_into in_chan (Bytes.unsafe_to_string bytes) 0 size
      >>= begin function
        | 0   -> loop (k None)
        | len ->
          assert (len > 0);
          loop (k (Some (`String (Bytes.(unsafe_to_string (sub bytes 0 len))))))
      end
    | state -> Lwt.return state
  in
  loop (parse ?input:initial_input ~initial_buffer_size:size p)
  >|= function
    | (Done(buf, _ ) | Fail(buf, _, _)) as state -> buf, state_to_result state
    | Partial _ -> assert false

let rec parse_many ?initial_input p in_chan k =
  parse ?initial_input p in_chan
  >>= function
    | (buf, Result.Ok a)    ->
      k a >>= fun () ->
      parse_many ~initial_input:(`Cstruct buf) p in_chan k
    | (buf, Result.Error a) ->
      Lwt.return (buf, Result.Error a)
