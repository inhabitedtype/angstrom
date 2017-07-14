open Angstrom

module Alcotest = struct
  include Alcotest

  let result (type a) (type e) a e =
    let (module A: TESTABLE with type t = a) = a in
    let (module E: TESTABLE with type t = e) = e in
    let module M = struct
      type t = (a, e) Result.result
      let pp fmt t = match t with
        | Result.Ok    t -> Format.fprintf fmt "Ok @[(%a)@]" A.pp t
        | Result.Error e -> Format.fprintf fmt "Error @[(%a)@]" E.pp e
      let equal x y = match x, y with
        | Result.Ok    x, Result.Ok    y -> A.equal x y
        | Result.Error x, Result.Error y -> E.equal x y
        | _             , _              -> false
    end in
    (module M: TESTABLE with type t = M.t)

  let none (type a) =
    let module M = struct
      type t = a
      let pp fmt _ = Format.pp_print_string fmt "Alcotest.none"
      let equal _ _ = false
    end in
    (module M: TESTABLE with type t = M.t)

  let any (type a) =
    let module M = struct
      type t = a
      let pp fmt _ = Format.pp_print_string fmt "Alcotest.any"
      let equal _ _ = true
    end in
    (module M: TESTABLE with type t = M.t)
end

let check ?size f p is =
  let open Buffered in
  let state =
    List.fold_left (fun state chunk ->
      feed state (`String chunk))
    (parse ?initial_buffer_size:size p) is
  in
  f (state_to_result (feed state `Eof))

let check_ok ?size ~msg test p is r =
  let r = Result.Ok r in
  check ?size (fun result -> Alcotest.(check (result test string)) msg r result)
    p is

let check_fail ?size ~msg p is =
  let r = Result.Error "" in
  check ?size (fun result -> Alcotest.(check (result none any)) msg r result)
    p is

let check_c  ?size ~msg p is r = check_ok ?size ~msg Alcotest.char            p is r
let check_lc ?size ~msg p is r = check_ok ?size ~msg Alcotest.(list char)     p is r
let check_co ?size ~msg p is r = check_ok ?size ~msg Alcotest.(option char)   p is r
let check_s  ?size ~msg p is r = check_ok ?size ~msg Alcotest.string          p is r
let check_ls ?size ~msg p is r = check_ok ?size ~msg Alcotest.(list string)   p is r
let check_int ?size ~msg p is r = check_ok ?size ~msg Alcotest.int            p is r
let check_int32 ?size ~msg p is r =
  let module Alco_int32 = struct
    type t = int32
    let pp fmt i = Format.pp_print_string fmt (Int32.to_string i)
    let equal (a : int32) (b : int32) = compare a b = 0
  end in
  check_ok ?size ~msg (module Alco_int32) p is r
let check_int64 ?size ~msg p is r =
  let module Alco_int64 = struct
    type t = int64
    let pp fmt i = Format.pp_print_string fmt (Int64.to_string i)
    let equal (a : int64) (b : int64) = compare a b = 0
  end in
  check_ok ?size ~msg (module Alco_int64) p is r
let check_float ?size ~msg p is r =
  let module Alco_float = struct
    type t = float
    let pp fmt f = Format.pp_print_string fmt (string_of_float f)
    let equal (a : float) (b : float) = compare a b = 0
  end in
  check_ok ?size ~msg (module Alco_float) p is r

let basic_constructors =
  [ "peek_char", `Quick, begin fun () ->
      check_co ~msg:"singleton input"  peek_char ["t"]    (Some 't');
      check_co ~msg:"longer input"     peek_char ["true"] (Some 't');
      check_co ~msg:"empty input"      peek_char [""]     None;
  end
  ; "peek_char_fail", `Quick, begin fun () ->
      check_c    ~msg:"singleton input"  peek_char_fail ["t"]    't';
      check_c    ~msg:"longer input"     peek_char_fail ["true"] 't';
      check_fail ~msg:"empty input"      peek_char_fail [""]
  end
  ; "char", `Quick, begin fun () ->
      check_c    ~msg:"singleton 'a'" (char 'a') ["a"]     'a';
      check_c    ~msg:"prefix 'a'"    (char 'a') ["asdf"]  'a';
      check_fail ~msg:"'a' failure"   (char 'a') ["b"];
      check_fail ~msg:"empty buffer"  (char 'a') [""]
  end
  ; "not_char", `Quick, begin fun () ->
      check_c    ~msg:"not 'a' singleton" (not_char 'a') ["b"] 'b';
      check_c    ~msg:"not 'a' prefix"    (not_char 'a') ["baba"] 'b';
      check_fail ~msg:"not 'a' failure"   (not_char 'a') ["a"];
      check_fail ~msg:"empty buffer"      (not_char 'a') [""]
  end
  ; "any_char", `Quick, begin fun () ->
      check_c    ~msg:"non-empty buffer" any_char ["a"] 'a';
      check_fail ~msg:"empty buffer"     any_char [""]
  end
  ; "any_{,u}int8", `Quick, begin fun () ->
    check_int ~msg:"positive sign preserved" any_int8 ["\127"] 127;
    check_int ~msg:"negative sign preserved" any_int8 ["\129"] (-127);
    check_int ~msg:"sign invariant" any_uint8 ["\127"] 127;
    check_int ~msg:"sign invariant" any_uint8 ["\129"] (129)
  end
  ; "string", `Quick, begin fun () ->
      check_s ~msg:"empty string, non-empty buffer" (string "")     ["asdf"] "";
      check_s ~msg:"empty string, empty buffer"     (string "")     [""]     "";
      check_s ~msg:"exact string match"             (string "asdf") ["asdf"] "asdf";
      check_s ~msg:"string is prefix of input"      (string "as")   ["asdf"] "as";

      check_fail ~msg:"input is prefix of string"     (string "asdf") ["asd"];
      check_fail ~msg:"non-empty string, empty input" (string "test") [""]
  end
  ; "string_ci", `Quick, begin fun () ->
      check_s ~msg:"empty string, non-empty input"  (string_ci "")     ["asdf"] "";
      check_s ~msg:"empty string, empty input"      (string_ci "")     [""]     "";
      check_s ~msg:"exact string match"             (string_ci "asdf") ["AsDf"] "AsDf";
      check_s ~msg:"string is prefix of input"      (string_ci "as")   ["AsDf"] "As";

      check_fail ~msg:"input is prefix of string"     (string_ci "asdf") ["Asd"];
      check_fail ~msg:"non-empty string, empty input" (string_ci "test") [""]
  end
  ; "take_while", `Quick, begin fun () ->
      check_s ~msg:"true, non-empty input"  (take_while (fun _ -> true)) ["asdf"] "asdf";
      check_s ~msg:"true, empty input"      (take_while (fun _ -> true)) [""] "";
      check_s ~msg:"false, non-empty input" (take_while (fun _ -> false)) ["asdf"] "";
      check_s ~msg:"false, empty input"     (take_while (fun _ -> false)) [""] "";
  end
  ]

module Endian(Es : EndianString.EndianStringSig) = struct
  type 'a endian = {
    name : string;
    size : int;
    zero : 'a;
    min : 'a;
    max : 'a;
    dump : Bytes.t -> int -> 'a -> unit;
    testable : 'a Alcotest.testable
  }

  let int16 = {
    name = "int16";
    size = 2;
    zero = 0;
    min = ~-32768;
    max = 32767;
    dump = Es.set_int16;
    testable = Alcotest.int
  }
  let int32 = {
    name = "int32";
    size = 4;
    zero = Int32.zero;
    min = Int32.min_int;
    max = Int32.max_int;
    dump = Es.set_int32;
    testable = Alcotest.int32
  }
  let int64 = {
    name = "int64";
    size = 8;
    zero = Int64.zero;
    min = Int64.min_int;
    max = Int64.max_int;
    dump = Es.set_int64;
    testable = Alcotest.int64
  }
  let float = {
    name = "float";
    size = 4;
    zero = 0.0;
    (* XXX: Not really min/max *)
    min = ~-.2e10;
    max = 2e10;
    dump = Es.set_float;
    testable = Alcotest.float
  }
  let double = {
    name = "double";
    size = 8;
    zero = 0.0;
    (* XXX: Not really min/max *)
    min = ~-.2e30;
    max = 2e30;
    dump = Es.set_double;
    testable = Alcotest.float
  }

  let uint16 = { int16 with name = "uint16"; min = 0; max = 65535 }
  let uint32 = { int32 with name = "uint32" }

   let dump e x =
     let buf = Bytes.create e.size in
     e.dump buf 0 x;
     buf

  let make_tests e parse = e.name, `Quick, begin fun () ->
    check_ok ~msg:"zero"     e.testable parse [dump e e.zero]          e.zero;
    check_ok ~msg:"min"      e.testable parse [dump e e.min]           e.min;
    check_ok ~msg:"max"      e.testable parse [dump e e.max]           e.max;
    check_ok ~msg:"trailing" e.testable parse [dump e e.zero ^ "\xff"] e.zero;
  end

  module type EndianSig = module type of LE

  let tests (module E : EndianSig) = [
    make_tests int16  E.int16;
    make_tests int32  E.int32;
    make_tests int64  E.int64;
    make_tests uint16 E.uint16;
    make_tests uint32 E.uint32;
    make_tests float  E.float;
    make_tests double E.double;
  ]
end
let little_endian =
  let module E = Endian(EndianString.LittleEndian) in
  E.tests (module LE)
let big_endian =
  let module E = Endian(EndianString.BigEndian) in
  E.tests (module BE)

let monadic =
  [ "fail", `Quick, begin fun () ->
    check_fail ~msg:"non-empty input" (fail "<msg>") ["asdf"];
    check_fail ~msg:"empty input"     (fail "<msg>") [""]
  end
  ; "return", `Quick, begin fun () ->
    check_s ~msg:"non-empty input" (return "test") ["asdf"] "test";
    check_s ~msg:"empty input"     (return "test") [""]     "test";
  end
  ; "bind", `Quick, begin fun () ->
    check_s ~msg:"data dependency" (take 2 >>= fun s -> string s) ["asas"] "as";
  end
  ]

let applicative =
  [ "applicative", `Quick, begin fun () ->
    check_s ~msg:"`foo *> bar` returns bar" (string "foo" *> string "bar") ["foobar"] "bar";
    check_s ~msg:"`foo <* bar` returns bar" (string "foo" <* string "bar") ["foobar"] "foo";
  end
  ]

let alternative =
  [ "alternative", `Quick, begin fun () ->
      check_c ~msg:"char a | char b" (char 'a' <|> char 'b') ["a"] 'a';
      check_c ~msg:"char b | char a" (char 'b' <|> char 'a') ["a"] 'a';
      check_s ~msg:"string 'a' | string 'b'" (string "a" <|> string "b") ["a"] "a";
      check_s ~msg:"string 'b' | string 'a'" (string "b" <|> string "a") ["a"] "a";
  end ]

let combinators =
  [ "many", `Quick, begin fun () ->
        check_lc ~msg:"empty input"   (many (char 'a')) [""]  [];
        check_lc ~msg:"single char"   (many (char 'a')) ["a"] ['a'];
        check_lc ~msg:"two chars"     (many (char 'a')) ["aa"] ['a'; 'a'];
      end
  ; "many_till", `Quick, begin fun () ->
      check_lc ~msg:"not greedy" (many_till any_char (char '-')) ["ab-ab-"] ['a'; 'b'];
    end
  ; "sep_by1", `Quick, begin fun () ->
      let parser = sep_by1 (char ',') (char 'a') in
      check_lc ~msg:"single char"     parser ["a"]    ['a'];
      check_lc ~msg:"many chars"      parser ["a,a"]  ['a'; 'a'];
      check_lc ~msg:"no trailing sep"  parser ["a,"]   ['a'];
    end
  ; "count", `Quick, begin fun () ->
      check_lc ~msg:"empty input" (count 0 (char 'a')) [""] [];
      check_lc ~msg:"exact input" (count 1 (char 'a')) ["a"] ['a'];
      check_lc ~msg:"additonal input" (count 2 (char 'a')) ["aaa"] ['a'; 'a'];
      check_fail ~msg:"bad input" (count 2 (char 'a')) ["abb"];
    end
  ; "scan_state", `Quick, begin fun () ->
      check_s ~msg:"scan_state" (scan_state "" (fun s -> function
          | 'a' -> Some s
          | '.' -> None
          | c -> Some ((String.make 1 c) ^ s)
        )) ["abaacba."] "bcb";
      let p =
        count 2 (scan_state "" (fun s -> function
            | '.' -> None
            | c -> Some (s ^ String.make 1 c)
          ))
        >>| String.concat "" in
      check_s ~msg:"state reset between runs" p ["bcd."] "bcd";
    end
  ]

let incremental =
  [ "within chunk boundary", `Quick, begin fun () ->
      check_s ~msg:"string on each side of 2 inputs"
        (string "this" *> string "that") ["this"; "that"] "that";
      check_s ~msg:"string on each side of 3 inputs"
        (string "thi" *> string "st" *> string "hat") ["thi"; "st"; "hat"] "hat";
      check_s ~msg:"string straddling 2 inputs"
        (string "thisthat") ["this"; "that"] "thisthat";
      check_s ~msg:"string straddling 3 inputs"
        (string "thisthat") ["thi"; "st"; "hat"] "thisthat";
    end
  ; "across chunk boundary", `Quick, begin fun () ->
      check_s ~size:4 ~msg:"string on each side of 2 chunks"
        (string "this" *> string "that") ["this"; "that"] "that";
      check_s ~size:3 ~msg:"string on each side of 3 chunks"
        (string "thi" *> string "st" *> string "hat") ["thi"; "st"; "hat"] "hat";
      check_s ~size:4 ~msg:"string straddling 2 chunks"
        (string "thisthat") ["this"; "that"] "thisthat";
      check_s ~size:3 ~msg:"string straddling 3 chunks"
        (string "thisthat") ["thi"; "st"; "hat"] "thisthat";
    end
  ; "across chunk boundary with commit", `Quick, begin fun () ->
      check_s ~size:4 ~msg:"string on each side of 2 chunks"
        (string "this" *> commit *> string "that") ["this"; "that"] "that";
      check_s ~size:3 ~msg:"string on each side of 3 chunks"
        (string "thi" *> string "st" *> commit *> string "hat") ["thi"; "st"; "hat"] "hat";
    end ]

let () =
  Alcotest.run "test suite"
    [ "basic constructors"    , basic_constructors
    ; "little endian"         , little_endian
    ; "big endian"            , big_endian
    ; "monadic interface"     , monadic
    ; "applicative interface" , applicative
    ; "alternative"           , alternative
    ; "combinators"           , combinators
    ; "incremental input"     , incremental ]
