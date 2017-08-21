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
