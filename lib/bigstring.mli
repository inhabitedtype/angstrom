(** {1 Bigstrings}

    This module is not part of Angstrom's public API to as not not cause
    confusion, usability issues, and linking errors related to naming conflicts
    with other existing libraries.

    All of these operations skip bounds checks when possible. *)

type t =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create : int -> t
val empty  : t

val of_string : off:int -> len:int -> string -> t

val length : t -> int
val unsafe_get : t -> int -> char
val unsafe_set : t -> int -> char -> unit

val blit             : t       -> int -> t -> int -> int -> unit
val blit_from_string : string  -> int -> t -> int -> int -> unit
val blit_from_bytes  : Bytes.t -> int -> t -> int -> int -> unit

val blit_to_bytes  : t -> int -> Bytes.t -> int -> int -> unit

val sub : t -> off:int -> len:int -> t
val substring : t -> off:int -> len:int -> string

val unsafe_set_16_le : t -> off:int -> int -> unit
val unsafe_set_16_be : t -> off:int -> int -> unit

val unsafe_set_32_le : t -> off:int -> int32 -> unit
val unsafe_set_32_be : t -> off:int -> int32 -> unit

val unsafe_set_64_le : t -> off:int -> int64 -> unit
val unsafe_set_64_be : t -> off:int -> int64 -> unit
