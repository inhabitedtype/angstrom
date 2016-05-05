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
  let state =
    List.fold_left (fun state chunk ->
      feed state (`String chunk))
    (parse ?initial_buffer_size:size p) is
  in
  let result =
    match state with
    | Partial k -> state_to_result (k None)
    | _         -> state_to_result state
  in
  f result

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
  ]

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
  end
  ; "commit", `Quick, begin fun () ->
      check_c ~msg:"commit (char a) | char b" (commit (char 'a') <|> char 'b') ["a"] 'a';
      check_c ~msg:"commit (char b) | char a" (commit (char 'b') <|> char 'a') ["a"] 'a';
      check_fail ~msg:"" ((commit (string "ab") <|> string "a") *> string "bd") ["abd"];
  end ]

let combinators =
  [ "many", `Quick, begin fun () ->
      check_lc ~msg:"empty input"   (many (char 'a')) [""]  [];
      check_lc ~msg:"single char"   (many (char 'a')) ["a"] ['a'];
      check_lc ~msg:"two chars"     (many (char 'a')) ["aa"] ['a'; 'a'];
  end ]

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
        (commit (string "this") *> string "that") ["this"; "that"] "that";
      check_s ~size:3 ~msg:"string on each side of 3 chunks"
        (commit (string "thi" *> string "st") *> string "hat") ["thi"; "st"; "hat"] "hat";
    end ]

let () =
  Alcotest.run "test suite"
    [ "basic constructors"    , basic_constructors
    ; "monadic interface"     , monadic
    ; "applicative interface" , applicative
    ; "alternative"           , alternative
    ; "combinators"           , combinators
    ; "incremental input"     , incremental ]
