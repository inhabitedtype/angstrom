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

external caml_bigstring_set_16 : bigstring -> off:int -> int   -> unit = "%caml_bigstring_set16u"
external caml_bigstring_set_32 : bigstring -> off:int -> int32 -> unit = "%caml_bigstring_set32u"
external caml_bigstring_set_64 : bigstring -> off:int -> int64 -> unit = "%caml_bigstring_set64u"

external caml_bigstring_get_16 : bigstring -> off:int -> int   = "%caml_bigstring_get16u"
external caml_bigstring_get_32 : bigstring -> off:int -> int32 = "%caml_bigstring_get32u"
external caml_bigstring_get_64 : bigstring -> off:int -> int64 = "%caml_bigstring_get64u"

module Swap = struct
  external bswap16 : int -> int = "%bswap16"
  external bswap_int32 : int32 -> int32 = "%bswap_int32"
  external bswap_int64 : int64 -> int64 = "%bswap_int64"

  let caml_bigstring_set_16 bs ~off i =
    caml_bigstring_set_16 bs off (bswap16 i)

  let caml_bigstring_set_32 bs ~off i =
    caml_bigstring_set_32 bs off (bswap_int32 i)

  let caml_bigstring_set_64 bs ~off i =
    caml_bigstring_set_64 bs off (bswap_int64 i)

  let caml_bigstring_get_16 bs ~off =
    bswap16 (caml_bigstring_get_16 bs off)

  let caml_bigstring_get_32 bs ~off =
    bswap_int32 (caml_bigstring_get_32 bs off)

  let caml_bigstring_get_64 bs ~off =
    bswap_int64 (caml_bigstring_get_64 bs off)
end

let unsafe_set_16_le, unsafe_set_16_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_16, caml_bigstring_set_16
  else caml_bigstring_set_16     , Swap.caml_bigstring_set_16

let unsafe_set_32_le, unsafe_set_32_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_32, caml_bigstring_set_32
  else caml_bigstring_set_32     , Swap.caml_bigstring_set_32

let unsafe_set_64_le, unsafe_set_64_be =
  if Sys.big_endian
  then Swap.caml_bigstring_set_64, caml_bigstring_set_64
  else caml_bigstring_set_64     , Swap.caml_bigstring_set_64

let unsafe_get_u16_le, unsafe_get_u16_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_16, caml_bigstring_get_16
  else caml_bigstring_get_16     , Swap.caml_bigstring_get_16

let unsafe_get_16_le x ~off =
  ((unsafe_get_u16_le x ~off) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let unsafe_get_16_be x ~off =
  ((unsafe_get_u16_be x~off ) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let unsafe_get_32_le, unsafe_get_32_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_32, caml_bigstring_get_32
  else caml_bigstring_get_32     , Swap.caml_bigstring_get_32

let unsafe_get_64_le, unsafe_get_64_be =
  if Sys.big_endian
  then Swap.caml_bigstring_get_64, caml_bigstring_get_64
  else caml_bigstring_get_64     , Swap.caml_bigstring_get_64
