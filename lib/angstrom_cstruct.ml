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
  (**
   This submodule contains a portion of Cstruct (v.1.9.0, taken from
   https://github.com/mirage/ocaml-cstruct) code used inside angstrom
   (to remove package dependency). Currently security checks in methods are neglected.
   Thus, this module's code is licensed by:

  * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
  *
  * Permission to use, copy, modify, and distribute this software for any
  * purpose with or without fee is hereby granted, provided that the above
  * copyright notice and this permission notice appear in all copies.
  *
  * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

  *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = {
  buffer : bigstring;
  off : int;
  len : int;
}

external unsafe_blit_string_to_bigstring : string -> int -> bigstring -> int -> int -> unit = "caml_blit_string_to_bigstring" "noalloc"

external unsafe_blit_bigstring_to_bigstring : bigstring -> int -> bigstring -> int -> int -> unit = "caml_blit_bigstring_to_bigstring" "noalloc"

external unsafe_blit_bigstring_to_bytes : bigstring -> int -> Bytes.t -> int -> int -> unit = "caml_blit_bigstring_to_string" "noalloc"

let blit_from_string src srcoff dst dstoff len =
  unsafe_blit_string_to_bigstring src srcoff dst.buffer (dst.off+dstoff) len

let set_len t len = { t with len }

let add_len t len =
  let len = t.len + len in
  {t with len}

let len t = t.len

let create len =
  let buffer = Bigarray.(Array1.create char c_layout len) in
  { buffer ; len ; off = 0 }

let to_string t =
  let sz = len t in
  let b = Bytes.create sz in
  unsafe_blit_bigstring_to_bytes t.buffer t.off b 0 sz;
  (* The following call is safe, since b is not visible elsewhere. *)
  Bytes.unsafe_to_string b

let of_string ?allocator buf =
  let buflen = String.length buf in
  match allocator with
  |None ->
    let c = create buflen  in
    blit_from_string buf 0 c 0 buflen;
    c
  |Some fn ->
    let c = fn buflen in
    blit_from_string buf 0 c 0 buflen;
    set_len c buflen

let of_bigarray ?(off=0) ?len buffer =
  let dim = Bigarray.Array1.dim buffer in
  let len =
    match len with
    | None     -> dim - off
    | Some len -> len in
  { buffer; off; len }

let debug t =
  Format.sprintf "[%d,%d](%d)" t.off t.len (Bigarray.Array1.dim t.buffer)

let blit src srcoff dst dstoff len =
  unsafe_blit_bigstring_to_bigstring src.buffer (src.off+srcoff) dst.buffer (dst.off+dstoff) len

let sub t off0 len =
  let off = t.off + off0 in
  { t with off; len }

let shift t amount =
  let off = t.off + amount in
  let len = t.len - amount in
  { t with off; len }

let get_char t i =
  EndianBigstring.BigEndian.get_char t.buffer (t.off+i)


let copy src srcoff len =
  let b = Bytes.create len in
  unsafe_blit_bigstring_to_bytes src.buffer (src.off+srcoff) b 0 len;
  (* The following call is safe, since b is not visible elsewhere. *)
  Bytes.unsafe_to_string b
