open Core
open Angstrom

let symbol_encoding =
  take_while (fun c -> c <> '\x00') <* char '\x00'

let symbol_atom =
  char '\xf5' *> symbol_encoding

let test_vect hex () =
  let open Core in
  let open Async in
  let r =
    Pipe.create_reader
      ~close_on_exception:false
      (fun w -> Pipe.write w (Hex.to_string hex)) in
  Reader.of_pipe (Info.of_string "") r >>= fun r ->
  Angstrom_async.parse (take 8) r >>= fun _hdr ->
  Angstrom_async.parse symbol_atom r >>= function
  | Ok _v -> Deferred.unit
  | Error e -> failwith e

let basic = Alcotest_async.[
    test_case "atom symbol" `Quick
      (test_vect (`Hex "010000000b000000f56100")) ;
  ]

let () =
  Alcotest.run "angstrom-async" [
    "basic", basic ;
  ]
