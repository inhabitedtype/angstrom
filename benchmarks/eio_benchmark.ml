open! Eio.Std

let main env =
  let toss _ = () in
  let parser =
    match Sys.argv.(1) with
    | "http" -> Angstrom.(RFC2616.request >>| fun x -> `Http x)
    | "json" -> Angstrom.(RFC7159.json    >>| fun x -> `Json x)
    | _      -> print_endline "usage: eio_json_benchmark.native PARSER"; exit 1
  in
  Angstrom_eio.parse_many parser toss (Eio.Stdenv.stdin env)
  |> function
    | _, Ok ()     -> ()
    | _, Error err -> failwith err
;;

Eio_main.run (main)
