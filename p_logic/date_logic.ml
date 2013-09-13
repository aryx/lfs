open Common

open Common_logic

(*
 * Logic for date.
 *
 * obsolete? use ferre date instead ?
 *
 * syntax:  *-*-1992:*:*:*
 * sugar:
 * -*-1992
 * day:23
 * month:January|12
 * year:1992
 * hour:1
 * minute:1
 * second:2 :) not very useful (in fact yes if use this solver on duree (ex duree mp3)
 * summer/autumn/...
 *
 * par rapport a ferre: peut dire juste le mois ou juste le jour
 *
 * TODO will enable lifestream like ?
 * cd futur ?
 * cd yesterday (=> have sugar a la filter)
 * cd now (today)
 * => pas de rep todo :)
 *
 * RESEARCH logic temporal can give good idea ?
 *
 *
 * TODO intlogic
 *
 * TODO nameday:Lundi
 *
 * TODO man find, no need newer,... simpler, no need to use a -or for find, ...
 *)

(* d/m/y:h/m/s *)
type date = ((interv option * interv option  * interv option) *
	     (interv option * interv option  * interv option))
    and interv = Exact of int (* intlogic *)

let parse_int = fun s ->
  if s = "*" then None
  else Some (Exact (s_to_i s)) (* intlogic *)

let parse_month = function
  | "*" -> None
  | s ->
      Some
        (Exact (
	  match s with
	  | ("January"|"Janvier") -> 1
	  | ("February"|"Fevrier") -> 2
	  | ("March"|"Mars") -> 3
	  | ("April"|"Avril") -> 4
	  | ("May"|"Mai") -> 5
	  | ("June"|"Juin") -> 6
	  | ("July"|"Juillet") -> 7
	  | ("August"|"Aout") -> 8
	  | ("September"|"Septembre") -> 9
	  | ("October"|"Octobre") -> 10
	  | ("November"|"Novembre") -> 11
	  | ("December"|"Decembre") -> 12
	  | s -> s_to_i s
	))

let rec parse = fun s ->
  match s with
  | s when s =~ "^\\(.+\\)-\\(.+\\)-\\(.+\\):\\(.+\\):\\(.+\\):\\(.+\\)$" ->
      let (d,mo,y, h,m,s) = matched6 s in
      ((parse_int d, parse_month mo, parse_int y), (* TODO? parse_year, allow 77 *)
       (parse_int h, parse_int m, parse_int s))
  (* sugar *)
  | s when s =~ "^\\(.+\\)-\\(.+\\)-\\(.+\\)*" -> parse (s ^ ":*:*:*")
  | s when s =~ "^\\(.+\\):\\(.+\\):\\(.+\\)*" -> parse ("*-*-*:" ^ s)
  | s when s =~ "day:\\(.+\\)"   -> parse (matched1 s ^ "-*-*")
  | s when s =~ "month:\\(.+\\)" -> parse ("*-" ^ matched1 s ^ "-*")
  | s when s =~ "year:\\(.+\\)"  -> parse ("*-*-" ^ matched1 s)
  | s when s =~ "hour:\\(.+\\)"   -> parse (matched1 s ^ ":*:*")
	(* TODO:  handle usa, pm am sugar *)
  | s when s =~ "minute:\\(.+\\)" -> parse ("*:" ^ matched1 s ^ ":*")
  | s when s =~ "second:\\(.+\\)" -> parse ("*:*:" ^ matched1 s)
  | _ -> failwith "syntax error"

let rec invariant = fun ((d, mo, y), (h, m, s)) ->
  let wrap f x =
    match x with
    | None -> true
    | Some (Exact i) -> f i
  in
  d  +> wrap (fun i -> i >= 1 && i <= 31) && (* TODO? sometimes can check when given month if valid *)
  mo +> wrap (fun i -> i >= 1 && i <= 12) &&
  h  +> wrap (fun i -> i >= 0 && i <= 24) &&
  m  +> wrap (fun i -> i >= 0 && i <= 59) &&
  s  +> wrap (fun i -> i >= 0 && i <= 59) &&
  true

type datelist = interv option list
let string_of_datelist = fun xs ->
  join "-" (xs +> map (function | None -> "*" | Some (Exact i) -> i_to_s i))

let rec (date_logic:logic) = fun (Prop s1) (Prop s2) ->
  let (x1, x2) = (parse s1, parse s2) in
  let _ = assert (invariant x1) in
  let _ = assert (invariant x2) in
  (match (x1, x2) with
  | (((d1, mo1, y1), (h1,m1,s1)) , ((d2, mo2, y2), (h2,m2,s2))) ->
      solver_rec ([d1;mo1;y1;h1;m1;s1], [d2;mo2;y2;h2;m2;s2])
  )
  and solver_rec = function
  | ([],[]) -> true
  | (x::xs, y::ys) ->
(*debug:      let _ = pr2 (string_of_datelist (x::xs)) in let _ = pr2 (string_of_datelist (y::ys)) in *)
      (match (x,y) with
      |	(None, None) -> solver_rec (xs, ys)
      |	(None, Some _) -> false
      |	(Some _, None) ->  true   && solver_rec (xs, ys)  (* -22- |= * *)
      |	(Some x, Some y) -> x = y && solver_rec (xs, ys) (* intlogic *)
      )
  | _ -> raise Impossible

let is_formula (Prop s) = true (* TODO, but as we allow different form for same leaf, cant optimise :( *)

let (main: unit) = interact_logic date_logic     is_formula
