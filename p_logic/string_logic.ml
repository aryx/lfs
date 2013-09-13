open Common

open Common_logic

(* Logic for string via basic regexp (<,>, <>),
 * very similar to int logic.
 *
 * Inspired by sebastien ferre code.
 *)

type basic_regexp =
  | Begin of string
  | End of string
  | Contain of string
  | Str of string
(* old: | Joker     not needed in lfs, author:* is handled via simply author: *)

let parse = fun s ->
  match s with
  | s when s =~ "^<\\([a-zA-Z0-9_-]+\\)>$"  -> Contain (matched1 s)
  | s when s =~ "^<\\([a-zA-Z0-9_-]+\\)$"   -> Begin   (matched1 s)
  | s when s =~  "^\\([a-zA-Z0-9_-]+\\)>$"  -> End     (matched1 s)
  | s -> Str s

(*
 * todo? allow full regexp logic ? (cf regexp_logic.pl).
 * But because of builtin &|!, you can do more advanced regexp than
 * we think even with this simple logic.
 *
 * TODO allow at least more than [a-zA-Z0-9_-], in that case cant
 * use anymore regexp, or need escaping
 * => for Begin, need do String.sub s 0 (slength y) = y -> true
 * TODO fuzzy (a la google and agrep)
 *)

let (string_logic:logic) = fun (Prop s1) (Prop s2) ->
  let (x1, x2) = (parse s1, parse s2) in
  (match (x1, x2) with
  | (Str x, Str y)  -> x = y
  | (Str x, Begin y) -> x =~ ("^" ^ y)
  | (Str x, End y)   -> x =~ (".*" ^ y ^ "$")
  | (Str x, Contain y) -> x =~ (".*" ^ y ^ ".*")
  | (Begin x, Begin y) -> x =~ ("^" ^ y)             (* <abc |= <ab *)
  | (End x, End y)     -> x =~ (".*" ^ y ^ "$")      (* abc> |= bc> *)
  | (Contain x, Contain y) -> x =~ (".*" ^ y ^ ".*") (* <aaa> |= <a> *)
  | _ -> false
  )

let is_formula (Prop s) = match parse s with Str _ -> false | _ -> true

let (main: unit) = interact_logic string_logic      is_formula
