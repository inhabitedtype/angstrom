/*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

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
  ----------------------------------------------------------------------------*/

//Provides: angstrom_bigstring_blit_to_bytes
//Requires: caml_string_unsafe_set, caml_ba_get_1
function angstrom_bigstring_blit_to_bytes(src, src_off, dst, dst_off, len) {
  for (var i = 0; i < len; i++) {
    caml_string_unsafe_set(dst, dst_off + i, caml_ba_get_1(src, src_off + i));
  }
}

//Provides: angstrom_bigstring_blit_to_bigstring
//Requires: caml_ba_set_1, caml_ba_get_1
function angstrom_bigstring_blit_to_bigstring(src, src_off, dst, dst_off, len) {
  for (var i = 0; i < len; i++) {
    caml_ba_set_1(dst, dst_off + i, caml_ba_get_1(src, src_off + i));
  }
}

//Provides: angstrom_bigstring_blit_from_bytes
//Requires: caml_ba_set_1, caml_string_unsafe_get
function angstrom_bigstring_blit_from_bytes(src, src_off, dst, dst_off, len) {
  for (var i = 0; i < len; i++) {
    caml_ba_set_1(dst, dst_off + i, caml_string_unsafe_get(src, src_off + i));
  }
}
