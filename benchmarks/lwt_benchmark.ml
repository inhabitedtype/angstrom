open Lwt

let main () =
  (*
  let toss _ = Lwt.return_unit in
  let parser =
    match Sys.argv.(1) with
    | "http" -> Angstrom.(RFC2616.request >>| fun x -> `Http x)
    | "json" -> Angstrom.(RFC7159.json    >>| fun x -> `Json x)
    | _      -> print_endline "usage: lwt_json_benchmark.native PARSER"; exit 1
  in
  Lwt_io.resize_buffer Lwt_io.stdin 0x10000 >>= fun () ->
  Angstrom_lwt.parse_many parser toss Lwt_io.stdin
  >|= function
    | _, Result.Ok ()     -> ()
    | _, Result.Error err -> failwith err
    *)
  assert false
;;

Lwt_main.run (main ())
