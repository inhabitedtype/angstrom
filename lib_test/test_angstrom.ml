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
end

let check ?size ~msg test p is r =
  let p = p <* end_of_input in
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
  Alcotest.(check (result test string)) msg
    (Result.Ok r) result

let complete =
  let check_c ~msg p s r = check ~msg Alcotest.char   p [s] r in
  let check_s ~msg p s r = check ~msg Alcotest.string p [s] r in
  [ "single 'a' as char", `Quick, begin fun () ->
      check_c ~msg:"any_char" any_char "a" 'a';
      check_c ~msg:"not 'b'" (not_char 'b') "a" 'a';
      check_c ~msg:"char a" (char 'a') "a" 'a';
      check_c ~msg:"char a | char b" (char 'a' <|> char 'b') "a" 'a';
      check_c ~msg:"char b | char a" (char 'b' <|> char 'a') "a" 'a';
    end
  ; "single 'a' as string", `Quick, begin fun () ->
      check_s ~msg:"take 1" (take 1) "a" "a";
      check_s ~msg:"take_while (='a')" (take_while (fun c -> c = 'a')) "a" "a";
      check_s ~msg:"take_while1 (='a')" (take_while1 (fun c -> c = 'a')) "a" "a";
      check_s ~msg:"string 'a'" (string "a") "a" "a";
      check_s ~msg:"string 'a' | string 'b'" (string "a" <|> string "b") "a" "a";
      check_s ~msg:"string 'b' | string 'a'" (string "b" <|> string "a") "a" "a";
      check_s ~msg:"string_ci 'a'" (string_ci "a") "a" "a";
      check_s ~msg:"string_ci 'A'" (string_ci "A") "a" "a";
    end ]
;;

let incremental =
  let check_s ?size ~msg p ss r = check ?size ~msg Alcotest.string p ss r in
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
    [ "complete input"   , complete
    ; "incremental input", incremental ]
