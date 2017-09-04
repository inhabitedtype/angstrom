type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type t = bigstring

let create size = Bigarray.(Array1.create char c_layout size)
let empty       = create 0

module BA1 = Bigarray.Array1

let length t = BA1.dim t
let unsafe_get = BA1.unsafe_get
let unsafe_set = BA1.unsafe_set

let blit src src_off dst dst_off len =
  BA1.(blit (sub src src_off len) (sub dst dst_off len))

let blit_from_string src src_off dst dst_off len =
  for i = 0 to len - 1 do
    BA1.unsafe_set dst (dst_off + i) (String.unsafe_get src (src_off + i))
  done

let blit_from_bytes src src_off dst dst_off len =
  blit_from_string (Bytes.unsafe_to_string src) src_off dst dst_off len

let blit_to_bytes src src_off dst dst_off len =
  for i = 0 to len - 1 do
    Bytes.unsafe_set dst (dst_off +i) (BA1.unsafe_get src (src_off + i))
  done

let sub t ~off ~len =
  BA1.sub t off len

let substring t ~off ~len =
  let b = Bytes.create len in
  blit_to_bytes t off b 0 len;
  Bytes.unsafe_to_string b

let of_string ~off ~len s =
  let b = create len in
  blit_from_string s off b 0 len;
  b

external caml_bigstring_set_16u : bigstring -> off:int -> int -> unit = "%caml_bigstring_set16u"
external caml_bigstring_set_32u : bigstring -> off:int -> int32 -> unit = "%caml_bigstring_set32u"
external caml_bigstring_set_64u : bigstring -> off:int -> int64 -> unit = "%caml_bigstring_set64u"

module Swap = struct
  external bswap16 : int -> int = "%bswap16"
  external bswap_int32 : int32 -> int32 = "%bswap_int32"
  external bswap_int64 : int64 -> int64 = "%bswap_int64"

  let caml_bigstring_set_16u bs ~off i =
    caml_bigstring_set_16u bs off (bswap16 i)

  let caml_bigstring_set_32u bs ~off i =
    caml_bigstring_set_32u bs off (bswap_int32 i)

  let caml_bigstring_set_64u bs ~off i =
    caml_bigstring_set_64u bs off (bswap_int64 i)
end

let unsafe_set_16_le, unsafe_set_16_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_16u, caml_bigstring_set_16u
  else caml_bigstring_set_16u     , Swap.caml_bigstring_set_16u

let unsafe_set_32_le, unsafe_set_32_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_32u, caml_bigstring_set_32u
  else caml_bigstring_set_32u     , Swap.caml_bigstring_set_32u

let unsafe_set_64_le, unsafe_set_64_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_64u, caml_bigstring_set_64u
  else caml_bigstring_set_64u     , Swap.caml_bigstring_set_64u
