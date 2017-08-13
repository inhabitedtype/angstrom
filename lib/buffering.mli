type t

val create : int -> t
val of_bigstring : off:int -> len:int -> Bigstring.t -> t

val feed_string    : t -> off:int -> len:int -> string -> unit
val feed_bigstring : t -> off:int -> len:int -> Bigstring.t -> unit
val feed_input : t -> [ `String of string | `Bigstring of Bigstring.t ] -> unit

val shift : t -> int -> unit

val for_reading : t -> Bigstring.t

type unconsumed =
  { buf : Bigstring.t
  ; off : int
  ; len : int }

val unconsumed    : ?shift:int -> t -> unconsumed
val of_unconsumed : unconsumed -> t
