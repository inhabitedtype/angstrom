(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

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

(** Parser combinators built for speed and memory-efficiency.

    Angstrom is a parser-combinator library that provides a monadic and
    applicative interface for constructing parsers with unbounded lookahead.
    Its parsers can consume input incrementally, whether in a blocking or
    non-blocking environment. To achieve efficient incremental parsing,
    Angstrom offers both a buffered and unbuffered interface to input streams,
    with the {!module:Unbuffered} interface enabling zero-copy IO. With these
    features and low-level iteration parser primitives like {!take_while} and
    {!skip_while}, Angstrom makes it easy to write efficient, expressive, and
    reusable parsers suitable for high-performance applications. *)


type 'a t
(** A parser for values of type ['a]. *)


(** {2 Basic parsers} *)

val peek_char : char option t
(** [peek_char] accepts any char and returns it, or returns [None] if the end
    of input has been reached.

    This parser does not advance the input. Use it for lookahead. *)

val peek_char_fail : char t
(** [peek_char_fail] accepts any char and returns it. If end of input has been
    reached, it will fail.

    This parser does not advance the input. Use it for lookahead. *)

val peek_string : int -> string t
(** [peek_string n] accepts exactly [n] characters and returns them as a
    string. If there is not enough input, it will fail.

    This parser does not advance the input. Use it for lookahead. *)

val char : char -> char t
(** [char c] accepts [c] and returns it. *)

val not_char : char -> char t
(** [not_char] accepts any character that is not [c] and returns the matched
    character. *)

val any_char : char t
(** [any_char] accepts any character and returns it. *)

val satisfy : (char -> bool) -> char t
(** [satisfy f] accepts any character for which [f] returns [true] and returns
    the accepted character. *)

val string : string -> string t
(** [string s] accepts [s] exactly and returns it. *)

val string_ci : string -> string t
(** [string_ci s] accepts [s], ignoring case, and returns the matched string,
    preserving the case of the original input. *)

val skip : (char -> bool) -> unit t
(** [skip f] accepts any character for which [f] returns [true] and discards
    the accepted character. [skip f] equivalent to [satisfy f] but discards the
    accepted character. *)

val skip_while : (char -> bool) -> unit t
(** [skip_while f] accepts input as long as [f] returns [true] and discards
    the accepted characters. *)

val take : int -> string t
(** [take n] accepts exactly [n] characters of input and returns them as a
    string. *)

val take_while : (char -> bool) -> string t
(** [take_while f] accepts input as long as [f] returns [true] and returns the
    accepted characters as a string.

    This parser does not fail. If [f] returns [false] on the first character,
    it will return the empty string. *)

val take_while1 : (char -> bool) -> string t
(** [take_while f] accepts input as long as [f] returns [true] and returns the
    accepted characters as a string.

    This parser requires that [f] return [true] for at least one character of
    input, and will fail otherwise. *)

val take_till : (char -> bool) -> string t
(** [take_till f] accepts input as long as [f] returns [false] and returns the
    accepted characters as a string.

    This parser does not fail. If [f] returns [true] on the first character, it
    will return the empty string. *)

val take_rest : string list t
(** [take_rest] accepts the rest of the input and returns it as a list of
    strings. *)

val end_of_input : unit t
(** [end_of_input] succeeds if all the input has been consumed, and fails
    otherwise. *)

val end_of_line : unit t
(** [end_of_input] accepts either a line feed [\n], or a carriage return
    followed by a line feed [\r\n] and returns unit. *)


(** {2 Little/Big/Native endian parsers} *)

module Le : sig
  (** {2 Little endian parsers} *)

  val int8 : int t
  val int16 : int t
  val int32 : int32 t
  val int64 : int64 t
  (** [intN] reads [N] bits and interprets them as a signed, little endian
      integer. *)

  val uint8 : int t
  val uint16 : int t
  val uint32 : int32 t
  val uint64 : int64 t
  (** [uintN] reads [N] bits and interprets them as an unsigned, little endian
      integer. *)

  val float : float t
  (** [float] reads four bytes and interprets them as a little endian floating
      point value. *)

  val double : float t
  (** [double] reads eight bytes and interprets them as a little endian floating
      point value. *)
end

module Be : sig
  (** {2 Big endian parsers} *)

  val int8 : int t
  val int16 : int t
  val int32 : int32 t
  val int64 : int64 t
  (** [intN] reads [N] bits and interprets them as a signed, big endian
      integer. *)

  val uint8 : int t
  val uint16 : int t
  val uint32 : int32 t
  val uint64 : int64 t
  (** [uintN] reads [N] bits and interprets them as an unsigned, big endian
      integer. *)

  val float : float t
  (** [float] reads four bytes and interprets them as a big endian floating
      point value. *)

  val double : float t
  (** [double] reads eight bytes and interprets them as a big endian floating
      point value. *)
end

module Ne : sig
  (** {2 Native endian parsers} *)

  val int8 : int t
  val int16 : int t
  val int32 : int32 t
  val int64 : int64 t
  (** [intN] reads [N] bits and interprets them as a signed, native endian
      integer. *)

  val uint8 : int t
  val uint16 : int t
  val uint32 : int32 t
  val uint64 : int64 t
  (** [uintN] reads [N] bits and interprets them as an unsigned, native endian
      integer. *)

  val float : float t
  (** [float] reads four bytes and interprets them as a native endian floating
      point value. *)

  val double : float t
  (** [double] reads eight bytes and interprets them as a native endian floating
      point value. *)
end


(** {2 Combinators} *)

val option : 'a -> 'a t -> 'a t
(** [option v p] runs [p], returning the result of [p] if it succeeds and [v]
    if it fails. *)

val list : 'a t list -> 'a list t
(** [list ps] runs each [p] in [ps] in sequence, returning a list of results of
    each [p]. *)

val count : int -> 'a t -> 'a list t
(** [count n p] runs [p] [n] times, returning a list of the results. *)

val many : 'a t -> 'a list t
(** [many p] runs [p] {i zero} or more times and returns a list of results from
    the runs of [p]. *)

val many1 : 'a t -> 'a list t
(** [many1 p] runs [p] {i one} or more times and returns a list of results from
    the runs of [p]. *)

val many_till : 'a t -> 'b t -> 'a list t
(** [many_till p e] runs parser [p] {i zero} or more times until action [e]
    succeeds and returns the list of result from the runs of [p]. *)

val sep_by : 'a t -> 'b t -> 'b list t
(** [sep_by s p] runs [p] {i zero} or more times, interspersing runs of [s] in between. *)

val sep_by1 : 'a t -> 'b t -> 'b list t
(** [sep_by1 s p] runs [p] {i one} or more times, interspersing runs of [s] in between. *)

val skip_many : 'a t -> unit t
(** [skip_many p] runs [p] {i zero} or more times, discarding the results.*)

val skip_many1 : 'a t -> unit t
(** [skip_many1 p] runs [p] {i one} or more times, discarding the results. *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] computes the fixpoint of [f] and runs the resultant parser. The
    argument that [f] receives is the result of [fix f], which [f] must use,
    paradoxically, to define [fix f].

    [fix] is useful when constructing parsers for inductively-defined types
    such as sequences, trees, etc. Consider for example the implementation of
    the {!many} combinator defined in this library:

{[let many p =
  fix (fun m ->
    (cons <$> p <*> m) <|> return [])]}

    [many p] is a parser that will run [p] zero or more times, accumulating the
    result of every run into a list, returning the result. It's defined by
    passing [fix] a function. This function assumes its argument [m] is a
    parser that behaves exactly like [many p]. You can see this in the
    expression comprising the left hand slide of the alternative operator
    [<|>]. This expression runs the parser [p] followed by the parser [m], and
    after which the result of [p] is cons'd onto the list that [m] produces.
    The right-hand side of the alternative operator provides a base case for
    the combinator: if [p] fails and the parse cannot proceed, return an empty
    list.

    Another way to illustrate the uses of [fix] is to construct a JSON parser.
    Assuming that parsers exist for the basic types such as [false], [true],
    [null], strings, and numbers, the question then becomes how to define a
    parser for objects and arrays? Both contain values that are themselves JSON
    values, so it seems as though it's impossible to write a parser that will
    accept JSON objects and arrays before writing a parser for JSON values as a
    whole.

    This is the exact situation that [fix] was made for. By defining the
    parsers for arrays and objects within the function that you pass to [fix],
    you will gain access to a parser that you can use to parse JSON values, the
    very parser you are defining!

{[let json =
  fix (fun json ->
    let arr = char '[' *> sep_by (char ',') json <* char ']' in
    let obj = char '{' *> ... json ... <* char '}' in
    choice [str; num; arr json, ...])]} *)


(** {2 Alternatives} *)

val (<|>) : 'a t -> 'a t -> 'a t
(** [p <|> q] runs [p] and returns the result if succeeds. If [p] fails, then
    the input will be reset and [q] will run instead. *)

val choice : 'a t list -> 'a t
(** [choice ts] runs each parser in [ts] in order until one succeeds and
    returns that result. *)

val (<?>) : 'a t -> string -> 'a t
(** [p <?> name] associates [name] with the parser [p], which will be reported
    in the case of failure. *)

val commit : unit t
(** [commit] prevents backtracking beyond the current position of the input.
    Any consumed input that is still buffered will potentially be overridden to
    make room for new incremental input. *)


(** {2 Monadic/Applicative interface} *)

val return : 'a -> 'a t
(** [return v] creates a parser that will always succeed and return [v] *)

val fail : string -> 'a t
(** [fail msg] creates a parser that will always fail with the message [msg] *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
(** [p >>= f] creates a parser that will run [p], pass its result to [f], run
    the parser that [f] produces, and return its result. *)

val (>>|) : 'a t -> ('a -> 'b) -> 'b t
(** [p >>| f] creates a parser that will run [p], and if it succeeds with
    result [v], will return [f v] *)

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
(** [f <*> p] is equivalent to [f >>= fun f -> p >>| f]. *)

val (<$>) : ('a -> 'b) -> 'a t -> 'b t
(** [f <$> p] is equivalent to [p >>| f] *)

val ( *>) : 'a t -> 'b t -> 'b t
(** [p *> q] runs [p], discards its result and then runs [q]. *)

val (<* ) : 'a t -> 'b t -> 'a t
(** [p <* q] runs [p], then runs [q], discards its result, and returns the
    result of [p]. *)

val lift : ('a -> 'b) -> 'a t -> 'b t
val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
val lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
(** The [liftn] family of functions promote functions to the parser monad.
    For any of these functions, the following equivalence holds:

{[liftn f p1 ... pn = f <$> p1 <*> ... <*> pn]}

    These functions are more efficient than using the applicative interface
    directly, mostly in terms of memory allocation but also in terms of speed.
    Prefer them over the applicative interface, even when the artiy of the
    function to be lifted exceeds the maximum [n] for which there is an
    implementation for [liftn]. In other words, if [f] has an arity of [5] but
    only [lift4] is provided, do the following:

{[lift4 f m1 m2 m3 m4 <*> m5]}

    Even with the partial application, it will be more efficient the
    applicative implementation. *)


(** {2 Running} *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type input =
  [ `String    of string
  | `Bigstring of bigstring ]

val parse_only : 'a t -> input -> ('a, string) Result.result
(** [parse_only t input] runs [t] on [input]. The parser will receive an [`Eof]
    after all of input has been consumed. For use-cases requiring that the
    parser be fed input incrementally, see the {!module:Buffered} and
    {!module:Unbuffered} modules below. *)

(** Buffered parsing interface.

    Parsers run through this module perform internal buffering of input. The
    parser state will keep track of unconsumed input and attempt to minimize
    memory allocation and copying. The {!Buffered.state.Partial} parser state
    will accept newly-read, incremental input and copy it into the internal
    buffer. Users can feed parser states using the {!feed} function. As a
    result, the interface is much easier to use than the one exposed by the
    {!Unbuffered} module.

    On success or failure, any unconsumed input will be returned to the user
    for additional processing. The buffer that the unconsumed input is returned
    in can also be reused. *)
module Buffered : sig
  type unconsumed =
    { buffer : bigstring
    ; off : int
    ; len : int }

  type 'a state =
    | Partial of ([ input | `Eof ] -> 'a state) (** The parser requires more input. *)
    | Done    of unconsumed * 'a (** The parser succeeded. *)
    | Fail    of unconsumed * string list * string (** The parser failed. *)

  val parse : ?initial_buffer_size:int -> ?input:input -> 'a t -> 'a state
  (** [parse ?initial_buffer_size ?input t] runs [t] on [input], if present,
      and await input if needed. [parse] will allocate a buffer of size
      [initial_buffer_size] (defaulting to 4k bytes) to do input buffering and
      automatically grow the buffer as needed. If [input] is a bigstring, its
      contents will be copied into the parser's internal buffer, leaving
      ownership with the caller. *)

  val feed : 'a state -> [ input | `Eof ] -> 'a state
  (** [feed state input] supplies the parser state with more input. If [state] is
      [Partial], then parsing will continue where it left off. Otherwise, the
      parser is in a [Fail] or [Done] state, in which case the [input] will be
      copied into the state's buffer for later use by the caller. *)

  val state_to_option : 'a state -> 'a option
  (** [state_to_option state] returns [Some (bs, v)] if the parser is in the
      [Done (bs, v)] state and [None] otherwise. This function has no effect on
      the current state of the parser. *)

  val state_to_result : 'a state -> ('a, string) Result.result
  (** [state_to_result state] returns [Ok v] if the parser is in the [Done v]
      state and [Error msg] if it is in the [Fail] or [Partial] state.

      This function has no effect on the current state of the parser. *)

  val state_to_unconsumed : 'a state -> unconsumed option
  (** [state_to_bigstring state] returns [Some bs] if [state = Done(bs, _)] or
      [state = Fail(bs, _, _)] and [None] otherwise. *)

end

(** Unbuffered parsing interface.

    Use this module for total control over memory allocation and copying.
    Parsers run through this module perform no internal buffering. Instead, the
    user is responsible for managing a buffer containing the entirety of the
    input that has yet to be consumed by the parser. The
    {Unbuffered.state.Partial} parser state reports to the user how much input
    the parser consumed during its last run, via the
    {!Unbuffered.partial.consumed} field. This area of input must be discarded
    before parsing can resume. Once additional input has been collected, the
    unconsumed input as well as new input must be passed to the parser state
    via the {!Unbuffered.partial.continue} function, together with an
    indication of whether there is {!Unbuffered.more} input to come.

    The logic that must be implemented in order to make proper use of this
    module is intricate and tied to your OS environment. It's advisable to use
    the {!Buffered} module when initially developing and testing your parsers.
    For production use-cases, consider the Async and Lwt support that this
    library includes before attempting to use this module directly. *)
module Unbuffered : sig
  type more =
    | Complete
    | Incomplete

  type 'a state =
    | Partial of 'a partial (** The parser requires more input. *)
    | Done    of 'a (** The parser succeeded. *)
    | Fail    of string list * string (** The parser failed. *)
  and 'a partial =
    { consumed : int
      (** The number of bytes consumed during the last input feeding.
          Callers must drop this number of bytes from the beginning of the
          input on subsequent calls. *)
    ; continue : input -> more -> 'a state
      (** A continuation of a parse that requires additional input. The input
          should include all unconsumed input (as reported by previous partial
          states) in addition to any new input that has become available, as
          well as an indication of whether there is {!more} input to come.  *)
    }

  val parse : ?input:input -> 'a t -> 'a state
  (** [parse ?input t] runs [t] on [input], if present, and await input if
      needed. *)

  val state_to_option : 'a state -> 'a option
  (** [state_to_option state] returns [Some (bs, v)] if the parser is in the
      [Done (bs, v)] state and [None] otherwise. This function has no effect on the
      current state of the parser. *)

  val state_to_result : 'a state -> ('a, string) Result.result
  (** [state_to_result state] returns [Ok v] if the parser is in the [Done v]
      state and [Error msg] if it is in the [Fail] or [Partial] state.

      This function has no effect on the current state of the parser. *)

end

(**/**)

(* These values are not part of the public API. *)

val pos : int t
val want_input : bool t
val available : int t

module Z : sig
  type 'a t

  type 'a state =
    | Partial of 'a partial
    | Done    of 'a
    | Fail    of string list * string
  and 'a partial =
    input -> Unbuffered.more -> 'a state

  (** {2 Basic parsers} *)

  val peek_char : char option t
  val peek_char_fail : char t
  val peek_string : int -> string t

  val char : char -> char t
  val not_char : char -> char t
  val any_char : char t
  val satisfy : (char -> bool) -> char t

  val string : string -> string t
  val string_ci : string -> string t

  val skip : (char -> bool) -> unit t
  val skip_while : (char -> bool) -> unit t
  val take : int -> string t
  val take_while : (char -> bool) -> string t
  val take_while1 : (char -> bool) -> string t
  val take_till : (char -> bool) -> string t
  val take_rest : string list t

  val end_of_input : unit t

  (** {2 Combinators} *)

  val count : int -> 'a t -> 'a list t
  val many : 'a t -> 'a list t
  val many1 : 'a t -> 'a list t
  val many_till : 'a t -> 'b t -> 'a list t
  val sep_by : 'a t -> 'b t -> 'b list t
  val sep_by1 : 'a t -> 'b t -> 'b list t
  val skip_many : 'a t -> unit t
  val skip_many1 : 'a t -> unit t

  val fix : ('a t -> 'a t) -> 'a t

  (** {2 Alternatives} *)
  val (<|>) : 'a t -> 'a t -> 'a t
  val choice : 'a t list -> 'a t
  val (<?>) : 'a t -> string -> 'a t
  val commit : unit t

  (** {2 Monadic/Applicative interface} *)

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t

  val ( *>) : 'a t -> 'b t -> 'b t
  val (<* ) : 'a t -> 'b t -> 'a t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t

  val lift : ('a -> 'b) -> 'a t -> 'b t
  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  val parse_only : 'a t -> input -> ('a, string) Result.result
end

val z : 'a Z.t -> 'a t
