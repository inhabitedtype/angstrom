type t =
  { mutable buf : Bigstring.t
  ; mutable off : int
  ; mutable len : int }

let of_bigstring ~off ~len buf =
  assert (off >= 0);
  assert (Bigstring.length buf >= len - off);
  { buf; off; len }

let create len =
  of_bigstring ~off:0 ~len:0 (Bigstring.create len)

let writable_space t =
  Bigstring.length t.buf - t.len

let trailing_space t =
  Bigstring.length t.buf - (t.off + t.len)

let compress t =
  Bigstring.blit t.buf t.off t.buf 0 t.len;
  t.off <- 0

let grow t to_copy =
  let old_len = Bigstring.length t.buf in
  let new_len = ref old_len in
  let space = writable_space t in
  while space + !new_len - old_len < to_copy do
    new_len := (3 * !new_len) / 2
  done;
  let new_buf = Bigstring.create !new_len in
  Bigstring.blit t.buf t.off new_buf 0 t.len;
  t.buf <- new_buf;
  t.off <- 0

let ensure t to_copy =
  if trailing_space t < to_copy then
    if writable_space t >= to_copy
    then compress t
    else grow t to_copy

let write_pos t =
  t.off + t.len

let feed_string t ~off ~len str =
  assert (off >= 0);
  assert (String.length str >= len - off);
  ensure t len;
  Bigstring.blit_from_string str off t.buf (write_pos t) len;
  t.len <- t.len + len

let feed_bigstring t ~off ~len b =
  assert (off >= 0);
  assert (Bigstring.length b >= len - off);
  ensure t len;
  Bigstring.blit b off t.buf (write_pos t) len;
  t.len <- t.len + len

let feed_input t = function
  | `String    s -> feed_string    t ~off:0 ~len:(String   .length s) s
  | `Bigstring b -> feed_bigstring t ~off:0 ~len:(Bigstring.length b) b

let shift t n =
  assert (t.len >= n);
  t.off <- t.off + n;
  t.len <- t.len - n

let for_reading { buf; off; len } =
  Bigstring.sub ~off ~len buf

module Unconsumed = struct
  type t =
    { buf : Bigstring.t
    ; off : int
    ; len : int }
end

let unconsumed ?(shift=0) { buf; off; len } =
  assert (len >= shift);
  { Unconsumed.buf; off = off + shift; len = len - shift }

let of_unconsumed { Unconsumed.buf; off; len } =
  { buf; off; len }

type unconsumed = Unconsumed.t =
  { buf : Bigstring.t
  ; off : int
  ; len : int }
