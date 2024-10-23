 
open Angstrom
 
module Lam = struct
  type t = Var of char | App of t * t | Abs of char * t
  [@@deriving show { with_path = false }]

  let var x = Var x
  let abs x l = Abs (x, l)
  let app l r = App (l, r)

  let rec pp ppf =
    let open Format in
    function
    | Var c -> fprintf ppf "%c" c
    | App (l, r) ->
      fprintf ppf "(%a %a)" pp l pp r
    | Abs (x, b) -> fprintf ppf "(\\%c . %a)" x pp b

end

let is_space = function ' ' | '\t' -> true | _ -> false
let spaces = skip_while is_space
let varname = satisfy (function 'a' .. 'z' -> true | _ -> false)

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl

type dispatch = {
  apps : dispatch -> Lam.t Angstrom.t;
  single : dispatch -> Lam.t Angstrom.t;
}

let parse_lam =
  let single pack =
    fix (fun _ ->
        conde
          [
            char '(' *> pack.apps pack <* char ')';
            ( (string "λ" <|> string "\\") *> spaces *> varname
            <* spaces <* char '.'
            >>= fun var ->
              pack.apps pack >>= fun b -> return (Lam.Abs (var, b)) );
            (varname <* spaces >>= fun c -> return (Lam.Var c));
          ])
  in
  let apps pack =
    many1 (spaces *> pack.single pack <* spaces) >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Lam.App (l, r)) x xs
  in
  { single; apps }

let parse_optimistically str =
  Result.get_ok
  @@ Angstrom.parse_string (parse_lam.apps parse_lam) str
       ~consume:Angstrom.Consume.All

let _ =
  Format.printf "%a%!\n" Lam.pp (parse_optimistically "a a   ");
   