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

(** A parser combinator library built for speed and memory efficiency. *)

type 'a t
(** A parser for values of type ['a]. *)


(** {2 Basic parsers} *)

val peek_char : char option t
(** [peek_char] matches any char and returns it, or returns [None] if the end
    of input has been reached.

    This parser does not consume any input. Use it for lookahead. *)

val peek_char_fail : char t
(** [peek_char_fail] matches any char and returns it. If end of input has been
    reached, it will fail.

    This parser does not consume any input. Use it for lookahead. *)

val char : char -> char t
(** [char c] matches [c] and returns it. *)

val not_char : char -> char t
(** [not_char] matches any character that is not [c] and returns the matched
    character. *)

val any_char : char t
(** [any_char] matches any character and returns it. *)

val string : string -> string t
(** [string s] matches [s] exactly and returns it. *)

val string_ci : string -> string t
(** [string_ci s] matches [s], ignoring case, and returns the matched string,
    preserving the case of the original input. *)

val satisfy : (char -> bool) -> char t
(** [satisfy f] matches any character for which [f] returns [true] and returns
    the matched character. *)

val skip : (char -> bool) -> unit t
(** [skip f] matches any character for which [f] returns [true] and discards
    the matched character. [skip f] equivalent to [satisfy f] but discards the
    matched character. *)

val skip_while : (char -> bool) -> unit t
(** [skip_while f] consumes input as long as [f] returns [true] and discards
    the matched characters. *)

val take : int -> string t
(** [take n] matches exactly [n] characters of input and returns them as a
    string. *)

val take_while : (char -> bool) -> string t
(** [take_while f] consumes input as long as [f] returns [true] and returns the
    matched characters as a string.

    This parser does not fail. If [f] returns [false] on the first character,
    it will return the empty string. *)

val take_while1 : (char -> bool) -> string t
(** [take_while f] consumes input as long as [f] returns [true] and returns the
    matched characters as a string.

    This parser requires that [f] return [true] for at least one character of
    input, and will fail otherwise. *)

val take_till : (char -> bool) -> string t
(** [take_till f] consumes input as long as [f] returns [false] and returns the
    matched characters as a string.

    This parser does not fail. If [f] returns [true] on the first character, it
    will return the empty string. *)

val take_rest : string list t
(** [take_rest] consumes the rest of the input and returns it as chunks. *)

val end_of_input : unit t
(** [end_of_input] succeeds if all the input has been consumed, and fails
    otherwise. *)

val end_of_line : unit t
(** [end_of_input] matches either a line feed, or a carriage return followed by
    a line feed and returns unit. *)


(** {2 Combinators} *)

val option : 'a -> 'a t -> 'a t
(** [option v p] runs [p], returning the result of [p] if it succeeds and [v]
    if it fails. *)

val list : 'a t list -> 'a list t
(** [list ps] runs each [p] in [ps] in sequence, returning a list of results of
    each [p]. *)

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

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] computes the fixpoint of [f] and runs the resultant parser,
    returning its result. *)


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

val commit : 'a t -> 'a t
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


(** {2 Running} *)

type input =
  [ `String  of string
  | `Cstruct of Cstruct.t ]

type 'a state =
  | Fail    of Cstruct.t * string list * string
  | Partial of (input option -> 'a state)
  | Done    of Cstruct.t * 'a

val parse : ?initial_buffer_size:int -> ?input:input -> 'a t -> 'a state
(** [parse ?initial_buffer_size ?input t] runs [t] on [input], if present, and
    and await input if needed. [parse] will allocate a buffer of size
    [initial_buffer_size] (defaulting to 4k bytes) to do input buffering and
    automatically grow the buffer as needed. *)

val parse_with_buffer : 'a t -> Cstruct.t -> 'a state
(** [parse_with_buffer t buffer] runs [t] with a user-allocated buffer [buffer]
    that the parser can take total ownership of. The view into [buffers] should
    be set to the bytes that can be used as input. The remainder of the space
    will be used as the user suppliese additional input to the parser. *)

val parse_only : 'a t -> input -> ('a, string) Result.result
(** [parse_only t input] runs [t] on [input]. *)

val feed : 'a state -> input -> 'a state
(** [feed state input] supplies the parser state with more input. If [state] is
    [Partial], then parsing will continue where it left off. Otherwise, the
    parser is in a [Fail] or [Done] state, in which case the [input] will be
    copied into the state's buffer for later use by the caller. *)

val state_to_option : 'a state -> 'a option
val state_to_result : 'a state -> ('a, string) Result.result


(** {2 State introspection}

    These functions are not part of the public API. *)

val pos : int t
val want_input : bool t
val available : int t
