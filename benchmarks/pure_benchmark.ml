module R = Result
open Core
open Core_bench

let read file =
  let open Unix in
  let size = Int64.to_int_exn (stat file).st_size in
  let buf  = String.create size in
  let rec loop pos len fd =
    let n = read ~pos ~len ~buf fd in
    if n > 0 then loop (pos + n) (len - n) fd
  in
  with_file ~mode:[O_RDONLY] file ~f:(fun fd ->
    loop 0 size fd);
  Bigstring.of_string buf

let zero =
  let len = 65_536 in
  Bigstring.of_string (String.make len '\x00')

let make_endian name p =
  Bench.Test.create ~name (fun () ->
    match Angstrom.(parse_bigstring (skip_many p) zero) with
    | R.Ok _ -> ()
    | R.Error err -> failwith err)


(* For input files involving trailing numbers, .e.g, [http-requests.txt.100],
 * go into the [benchmarks/data] directory and use the [replicate] script to
 * generate the file, i.e.,
 *
 *   [./replicate http-requests.txt 100]
 *
 *)
let main () =
  let big_twitter = read "benchmarks/data/twitter.json" in
  let http_get    = read "benchmarks/data/http-requests.txt.100" in
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"json" (fun () ->
      match Angstrom.parse_bigstring RFC7159.json big_twitter with
      | R.Ok _ -> ()
      | R.Error err -> failwith err);
    Bench.Test.create ~name:"http" (fun () ->
      match Angstrom.(parse_bigstring (skip_many RFC2616.request) http_get) with
      | R.Ok _ -> ()
      | R.Error err -> failwith err);
    make_endian "int64 le" Angstrom.LE.int64;
    make_endian "int64 be" Angstrom.BE.int64;
  ])

let () = main ()
