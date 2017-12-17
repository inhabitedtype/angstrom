type t

val create : int -> Bigstring.t -> t

val length : t -> int

val initial_commit_pos : t -> int
val uncommitted_bytes  : t -> int
val committed_bytes    : t -> int
val commit_pos         : t -> int

val get_char    : t -> int -> char
val count_while : t -> int -> f:(char -> bool) -> int

val apply : t -> int -> int -> f:(Bigstring.t -> off:int -> len:int -> 'a) -> 'a

val commit : t -> int -> unit
