(******************************************************************************)
(* TODO *)
(******************************************************************************)

(*
 macro Or a la merd (try or_left with -> or_right)

  Pcaml.str_item:
    [ [ "type"; LIDENT "nogen"; tdl = LIST1 type_declaration SEP "and" ->
          <:str_item< type $list:tdl$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          let sil = gen_ioxml_impl loc tdl in
  Pcaml.sig_item:
    [ [ "type"; LIDENT "nogen"; tdl = LIST1 type_declaration SEP "and" ->
          <:sig_item< type $list:tdl$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
need put too in sig_item ?

look in common.* to do more macro (and also in my docs/langage)

put the tricks i have in perl
 aspect ==> need fix_caml ? or can do with camlp4 (i think yes, but complicated)
 profiling mem/cpu/overhead/....
 pr pr2 ....
emacs tricks (C-M-1, ...)
au moins pour le debug

generalise my stuff so that it is easy to add functionnality
 (kind of tools a la haskell je_sais_plus_lenom)


faire meilleur lang
 where
 comprehension
 [.. syntax (just from ...) do the desugar as in haskell)

 `...` perhaps with help of lexer (can extend the lexer easily ?)
 type class ?
 layout ?
 section
 joly syntax pour les types ([], ())
 autogenerated accessor function

 \x -> ... NON visuellement dur a voir
 my stuff with self fun ?
 suppr let rec


class Show par exemple, peut etre fait
en passant en param a la function genere automatiquement 2 3 foncs
 (genre string_of_var, connection_between, ...) ?
can we make class stuff with my fix_caml ?
 just need pass around the dictionnary

pb let obj' = f obj
 mieux obj = f obj
 encore mieux macro, that "modify" obj
  comme let obj ||= f
  (ex ++ s'ecrirait let i ||= (+1)
*)

(******************************************************************************)
(*
   design interactivly: (=~ design and call macroexpand of lisp)
     $~> ocaml
     #load "camlp4o.cma";;
     #load "pa_extend.cmo";;
    add Extend rule, copy paste in interpreter
    Grammar.Entry.parse expr (Stream.of_string "2 + 3");;
      expr is already binded with the expr for caml
    can use the exception tricks to display (as in exception Print of type_decl)

   using p4 in p4 (with pattern that are quotation) is confusing
    => better to do a violent match over the real Ast type

   good example in pa_o.ml (defintion of caml grammar in p4)
*)
(******************************************************************************)
(*
   compile it:
    ocamlc -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo -impl' -I +camlp4 -impl macro.ml4

   test output:
    camlp4o ./macro.cmo pr_o.cmo common.ml

   use in file (handled by OcamlMakefile):
    put at start of your file (*pp camlp4o ./macro.cmo *)
    or manually compile with ocamlc -c -pp "camlp4o ./macro.cmo" -g my_file.ml
*)
(******************************************************************************)

open Pcaml
open MLast


(* same as in common *)
let (+>) o f = f o

let rec zip xs ys =
  match (xs,ys) with
  | ([],_) -> []
  | (_,[]) -> []
  | (x::xs,y::ys) -> (x,y)::zip xs ys

let foldl1 p = function x::xs -> List.fold_left p x xs | _ -> failwith "foldl1"

let rec join_gen a = function
  | [] -> []
  | [x] -> [x]
  | x::xs -> x::a::(join_gen a xs)

let _counter = ref 0
let counter () = (_counter := !_counter +1; !_counter)


(* just cos dont want care about location and pass it around to each func *)
(* old: let l = (1,1) *)
let l = (Lexing.dummy_pos, Lexing.dummy_pos)

(******************************************************************************)

(* inspired by an example in the camlp4 tutorial
   NEED recent version of camlp4 > 3.06+6 (available in the cvs only)

   update=tywith do better ? ioXml do better ?

   It auto-genarate string_of_<name_type> and print_<name_type> function.
   After defining a type, you can normally use the corresponding string_of function.
   For polymorphic type, you need to pass as a parameter the string_of function
    of the corresponding polymorphic variable

   ex:
    from "type t = int * int"
    it generates
	let rec string_of_t a0 =
	   match a0 with
	    a24, a25 -> ((("(" ^ string_of_int a24) ^ ",") ^ string_of_int a25) ^ ")"
	and print_t a = print_string (string_of_t a)

    from "type ('a, 'b) either = Left of 'a | Right of 'b"
    it generates
	let rec string_of_either (str__of_a, str__of_b) a0 =
	  match a0 with
	    Left a24 -> (("Left" ^ "(") ^ str__of_a a24) ^ ")"
	  | Right a26 -> (("Right" ^ "(") ^ str__of_b a26) ^ ")"
	and print_either funs a = print_string (string_of_either funs a)



  as i have not access to the source file of caml, you certainly have to put
    in one of your source file those definitions:

         let string_of_list f xs =
           "[" ^ (xs +> List.map f +> String.concat ";" ) ^ "]"

         let string_of_array f xs =
           "[|" ^ (xs +> Array.to_list +> List.map f +> String.concat ";") ^ "|]"

         let string_of_option f = function
           | None -> "None "
           | Some x -> "Some " ^ (f x)

         let print_bool x = print_string (if x then  "True" else "False")

         let print_list pr xs =
           do { print_string "["; List.iter (fun x -> pr x; print_string ",") xs; print_string "]" }

         let print_option pr = function
           | None -> print_string "None"
           | Some x -> print_string "Some ("; pr x; print_string ")"



*)

(* COMMENT START
let _ =
  DELETE_RULE
    Pcaml.str_item: "type"; LIST1 Pcaml.type_declaration SEP "and"
  END;

EXTEND
      Pcaml.str_item:
      [ [ "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ->

	let name () = "a" ^ string_of_int (counter ()) in

	(* subtil bug, when we have only one param, we cant construct a tuple with one param,
	   we should think that (1) and 1 are treated the same way, but seems this work
	   is done in caml in the parser, which mean that we must not give to caml an Ast with
	   a 1-uple
	*)
	let ex_tuple = function
	  | [] -> failwith "pb"
	  | [x] -> x
	  | xs -> ExTup (l,xs)
	in
	let pa_tuple = function
	  | [] -> failwith "pb"
	  | [x] -> x
	  | xs -> PaTup (l,xs)
	in

	let funcs = tdl +> List.map (fun ((l,str), param_polymorphs, ctyp, z) ->
	  let join_app  xs = xs +> foldl1 (fun a e -> <:expr< $a$ ^ $e$>>) in
	  let join_virg xs = xs +> join_gen (ExStr (l, ",")) in

	  let name_module_func = function
	    | TyLid (l, s)                           -> ExLid(l, "string_of_" ^ s)
	    | TyAcc (_, TyUid (_,file), TyLid (_,s)) ->
              (*  Module.string_of ....
		 ExAcc(l, ExUid (l,file), ExLid(l, "string_of_" ^ s))
		but dont work that much cos builtin lib or modules have not those function
	      *)
		ExLid(l, String.lowercase file ^ "_" ^ "string_of_" ^ s)
	    | _ -> failwith "pb"
	  in
	  (* TODOSTYLE? function fmatch id patt expr who put the None, ... *)
	  let rec body_ctyp id = function
	    | (TyLid (_) | TyAcc (_)) as c -> ExApp (l,name_module_func c, ExLid (l,id))
              (* | TyUid of loc and string *)

	    | TyQuo (l, s) -> ExApp (l, ExLid(l, "str__of_" ^ s), ExLid (l,id))
	    | TySum (l,_bool, xs) ->
		ExMat(l, ExLid (l, id),
		      xs +> List.map (fun (l, s, ctyps) ->
			let newids = xs +> List.map (fun _ -> name ()) in
			match ctyps, newids with
			| ([],_) -> (PaUid(l, s), None, ExStr (l,s))
			| (x::xs,id::ids) ->
			    let patt = zip xs ids +>
			      List.fold_left
				(fun a (e,id) -> PaApp (l, a, PaLid (l,id)))
				(PaApp (l, PaUid(l, s), PaLid(l, id))) in
			    (patt, None,
			     zip (x::xs) (id::ids) +> List.map (fun (e,id) -> body_ctyp id e) +> join_virg +>
			     (fun xs -> [ExStr (l,s); ExStr (l,"(")] @ xs @ [ExStr (l,")")]) +> join_app
			    )
			| _ -> failwith "pb"
				     )
		     )
	    | TyTup (l, xs) ->
		let newids = xs +> List.map (fun _ -> name ()) in
		ExMat(l, ExLid (l,id),
		      [PaTup(l, newids +> List.map (fun id -> PaLid (l,id))),
		      None,
		      zip xs newids +> List.map (fun (e,id) -> body_ctyp id e) +> join_virg +>
			(fun xs -> [ExStr (l,"(")] @ xs @ [ExStr (l,")")]) +> join_app
		      ]
		     )
	    | TyArr (l,c1,c2) -> ExStr(l, "<fun>")
	    | TyApp (l,c1,c2) ->
		let rec extract_all_params acc = function
		  | TyApp (l,c1', c2') -> extract_all_params (acc @ [c2']) c1'
		  | x -> (x, acc) in
		let (type_parameted, params) = extract_all_params [] (TyApp (l, c1, c2)) in
		let funcs = params +> List.map (fun c ->
		  let id = name () in
		  let f = body_ctyp id c in
		  ExFun (l, [PaLid(l, id), None, f]))
		in
		  (match type_parameted with
		  |  (TyLid (_) | TyAcc (_)) as c ->
		      ExApp (l, ExApp (l, name_module_func c, ex_tuple funcs), ExLid (l,id))
		  | _ -> ExStr (l,"<illegal syntax in type, should be a typeconst>")
		  )
	    | TyRec (l, _bool, xs) ->
		let newids = xs +> List.map (fun _ -> name ()) in
		ExMat(l, ExLid (l, id),
		      [ PaRec (l, zip xs newids +> List.map (fun ((_,e,_,_), id) -> PaLid(l, e), PaLid(l, id))),
			None,
			zip xs newids
			  +> List.map
			    (fun ((l,s,_b,e),id) -> [ExStr(l,s); ExStr(l," = ");body_ctyp id e] +> join_app)
			  +> join_virg
			  +> (fun xs -> [ExStr (l,"{")] @ xs @ [ExStr (l,"}")]) +> join_app
		      ])

	      (* TODO when needed
	    | TyAli of loc and ctyp and ctyp
	    | TyAny of loc
	    | TyCls of loc and list string
	    | TyLab of loc and string and ctyp
	    | TyMan of loc and ctyp and ctyp
	    | TyOlb of loc and string and ctyp
	    | TyPol of loc and list string and ctyp
	    | TyObj of loc and list (string * ctyp) and bool
	    | TyVrn of loc and list row_field and option (option (list string))
	      and row_field =
	      | RfTag of string and bool and list ctyp
	      | RfInh of ctyp
	      *)
	    | _ -> (ExStr (l, "<not yet implemnted>"))

	  in
	  let f_str_func_params body =
	    (* currifie:
	    param_polymorphs +> List.rev +> List.fold_left (fun acc (str_poly, (_,_)) ->
	      ExFun(l, [(PaLid (l, name_f_str_params str_poly),None, acc)]))
	      body
	    *)
	    if  param_polymorphs = [] then body
	    else ExFun (l, [pa_tuple
			       (param_polymorphs +> List.map (fun (str_poly,(_,_)) ->
				 PaLid (l, "str__of_" ^ str_poly))),
			     None, body])
	  in

	  [(PaLid (l, "string_of_" ^ str), (* let string_of... *)
	   f_str_func_params     (*  str__a str__b ... *)
	      (ExFun(l, [(PaLid (l,"a0"), None,  (* a0 = .... *)
			 (body_ctyp "a0" ctyp))])))    (*  match a with .... *)

	   ;(PaLid (l, "print_" ^ str),
	     (if param_polymorphs = []
	     then fun e -> e
	     else fun e -> (ExFun (l, [PaLid (l, "funs"), None, e]))
	     )
	       (ExFun (l, [(PaLid (l,"a")), None,
			    ExApp (l, ExLid (l, "print_string"),
				   if param_polymorphs = []
				   then ExApp(l, ExLid (l, "string_of_" ^ str), ExLid (l, "a"))
				   else ExApp (l,
					       ExApp (l, ExLid (l, "string_of_" ^ str),
						      ExLid (l, "funs")),
					       ExLid (l, "a")))])))
	  ])
	in
	let recursif = true in
	(StDcl (loc, [(StTyp (loc, tdl));StVal (loc, recursif, funcs +> List.flatten)]))
]]
      ;
    END
;;

END COMMENT *)
(*
let x = Grammar.Entry.parse str_item (Stream.of_string "type synon = int * int")
let x = Grammar.Entry.parse str_item (Stream.of_string "type 'a numdict =  C of ('a -> 'a)")
let x = Grammar.Entry.parse str_item (Stream.of_string "type test = C1 of string * string | C2 and t2 = C3 | C4")
let x = Grammar.Entry.parse str_item (Stream.of_string "type ('a,'b) test = C1 of 'a | C2 of 'b")
let x = Grammar.Entry.parse str_item (Stream.of_string "type point = {x: int; y:int}")
let x = Grammar.Entry.parse str_item (Stream.of_string "type ('a,'b) assoc  = ('a * 'b) list")
let x = Grammar.Entry.parse str_item (Stream.of_string "type ('a,'b) vec  = ('a , 'b) assoc")
let x = Grammar.Entry.parse str_item (Stream.of_string "type robot_list = robot IntMap.t")

let x = Grammar.Entry.parse expr (Stream.of_string "match a with {x = a; y = b} -> a + b")
let x = Grammar.Entry.parse expr (Stream.of_string "match a with (a,b,c,d) -> a + b + c + d")
let x = Grammar.Entry.parse expr (Stream.of_string "match a with C1 (s, s2) -> s | C2 s -> s | C3 -> s")
let x = Grammar.Entry.parse str_item (Stream.of_string "let rec fact x = if x = 0 then 1 else x * fact (x -1);; ")
let x = Grammar.Entry.parse str_item (Stream.of_string "let print_toto f a  = function () -> ();; ")
let x = Grammar.Entry.parse str_item (Stream.of_string "let print_toto f1 f2 a  = function () -> ();; ")
let x = Grammar.Entry.parse str_item (Stream.of_string "let print_toto (f1,f2) a  = function () -> ();; ")
let x = Grammar.Entry.parse str_item (Stream.of_string "let print_assoc (f1,f2) a0  = print_list (fun a -> function () -> ());; ")
let x = Grammar.Entry.parse str_item (Stream.of_string "let print_assoc (f1,f2) a0  = IntMap.print_list (fun a -> function () -> ());; ")


let x = Grammar.Entry.parse expr (Stream.of_string "1 + 1")
let x = Grammar.Entry.parse expr (Stream.of_string "\"tptp\" ^ string_of a")
let x = Grammar.Entry.parse expr (Stream.of_string "print_string (string_of a)")

let t   = macro_expand "type t = int * int;;"
let t   = macro_expand "type test = C1 of string * string | C2 and t2 = C3 | C4"
let t   = macro_expand "type 'a test = C1 of string * string | C2 | C3 of 'a and t2 = C3 | C4"
let t   = macro_expand "type ('a,'b) either = Left of 'a | Right of 'b"
let t   = macro_expand "type color_test = Red | Yellow"
let t   = macro_expand "type ('a,'b) assoc  = ('a * 'b) list"
let t = macro_expand "type 'a numdict =  C of ('a -> 'a)"
let t = macro_expand "type 'a numdict =  NumDict of (('a-> 'a -> 'a) * ('a-> 'a -> 'a) * ('a-> 'a -> 'a) * 	('a -> 'a))"
let t = macro_expand "type 'a bintree = Leaf of 'a | Branch of ('a bintree * 'a bintree)"
let t = macro_expand "type ('a,'b) assoc  = ('a * 'b) list"
let t = macro_expand "type ('a,'b) vec  = ('a , 'b) assoc"
let t = macro_expand "type vector = (float * float * float)"
let t = macro_expand "type point = {x: int; y:int}"
let t = macro_expand "type robot_list = robot IntMap.t"
let t = macro_expand "type robot_list = IntMap.t"
*)
(******************************************************************************)
(*
map
fold
fold_some
apres pourra faire facilement des trucs du genre get_all_vars,
 ...

 facto code ?

'a 'b --> 'c 'd pour map

intmap fold = ?
les fold des libs de caml = ?

*)


(******************************************************************************)

(*
EXTEND
      expr: BEFORE "simple"
           [[ "do"; "{";  e1 = expr; "}" ->
	     <:expr<  do {  $e1$ }  >> ]];
END;;
*)
(*  can also do                <:expr<  let _ =  $e1$ in ()  >> ]]; *)

EXTEND
      expr: BEFORE "simple"
           [[ "do"; "{";  e1 = expr; "}" ->
	      ExSeq ((Lexing.dummy_pos,Lexing.dummy_pos),[e1])
	    ]];
END;;


(******************************************************************************)
(* use:
   let g = (x + y)
     where x = 1
       and y = 1
*)

EXTEND
      expr:
           [[ e2 = expr; "where"; rest = LIST1 Pcaml.let_binding SEP "and" ->
	     rest +> List.rev +> List.fold_left (fun acc (patt, expr) ->
	       ExLet(l, false, [patt,expr], acc)) e2
	    ]];
END;;

(*
let x = Grammar.Entry.parse expr (Stream.of_string "let x = 1 in let y a = 1 in x + y");;
let t = macro_expand "let2 x = 1 in 1"
let t = macro_expand "x where x = 1"
let t = macro_expand "x + y where x = 1 and y = 1"
*)

(******************************************************************************)
(*
use:
 { x + y | (x,y) <- [(1,1);(2,2);(3,3)] and x > 2 and y < 3}
  TODO support only one generator for the moment
*)
(*
EXTEND
    expr:
           [[
             "{{"; e1 = expr; "|"; p1 = patt; "<-"; e2 = expr; "}}" ->
	       let maps = (ExAcc (l, ExUid (l, "List"), ExLid (l,"map"))) in
	       ExApp (l, ExApp(l, maps,
			       ExFun (l, [p1, None, e1])
			      ), e2)
            | "{{"; e1 = expr; "|"; p1 = patt; "<-"; e2 = expr; "and"; e3s = LIST1 expr SEP "and"; "}}" ->
	       let maps    = (ExAcc (l, ExUid (l, "List"), ExLid (l,"map"))) in
	       let filters =(ExAcc (l, ExUid (l, "List"), ExLid (l,"filter"))) in

	       ExApp (l,
		      ExApp(l, maps,ExFun (l, [p1, None, e1])),
		      e3s +> List.fold_left (fun acc expr ->
			ExApp (l,
			       ExApp(l, filters, ExFun(l, [p1, None, expr])),
			       acc)
					    )
			e2)
	    ]];
END;;
*)
(*
   [ expr1 | x <- expr2]        ----> expr2 +> List.map (fun x -> expr1)
   [ expr1 | x <- expr2, x > 2] ----> expr2 +> List.filter (fun x -> x > 2) +> List.map (fun x -> expr1)
 TODO better than | to separate filter
*)

(******************************************************************************)
(*
use:
   let x = {1 .. 10} +> List.map (fun i -> i)
   you need space between token (dont know really why)
   you need the enum function: let rec enum x n = if x = n then [n] else x::enum (x+1)  n

*)
(*
EXTEND
    expr:
           [[
             "{{"; e1 = expr; ".."; e2 = expr; "}}" ->
	       <:expr<enum $e1$ $e2$>>
	    ]];
END;;
*)
(******************************************************************************)

(*
EXTEND
    expr: BEFORE "simple"
           [[
             e1 = expr; "to"; e2 = expr; "to"; e3 = expr ->
	       <:expr<e2 $e1$ $e3$>>
	    ]];
END;;
*)


(******************************************************************************)
(*
 let add_update_plan w e = w.update_plan <- e::w.update_plan
 sucks cos no first class field
 perhaps need a preprocessor over record to let the field be first class
 indeed :) camlp4
*)

EXTEND
  expr:BEFORE "simple"
  [ [
    "modify"; record = NEXT; "."; field = LIDENT ; f = NEXT ->
      <:expr< $record$.$lid:field$ := $f$ $record$.$lid:field$>>
  ]];
END;;

(*
   #  type point  = {mutable x:int; mutable y:int};;
   # let o = { x= 0; y = 0};;
   # modify o.y succ;;

   I haven't tested this extension any further, but the main issue I can see is
   that "modify" becomes a keyword, which can be a problem if there are some
   variables with that name in your code.

   -- Virgile Prevosto <virgile.prevosto@lip6.fr>
*)

(******************************************************************************)
(*
Original version, without quotation
let _ =
  DELETE_RULE
    Pcaml.str_item: "type"; LIST1 Pcaml.type_declaration SEP "and"
  END;

EXTEND
      Pcaml.str_item:
      [ [ "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ->

	let name () = "a" ^ string_of_int (counter ()) in

	(* subtil bug, when we have only one param, we cant construct a tuple with one param,
	   we should think that (1) and 1 are treated the same way, but seems this work
	   is done in caml in the parser, which mean that we must not give to caml an Ast with
	   a 1-uple
	*)
	let ex_tuple = function
	  | [] -> failwith "pb"
	  | [x] -> x
	  | xs -> ExTup (l,xs)
	in
	let pa_tuple = function
	  | [] -> failwith "pb"
	  | [x] -> x
	  | xs -> PaTup (l,xs)
	in

	let funcs = tdl +> List.map (fun ((l,str), param_polymorphs, ctyp, z) ->
	  let join_app  xs = xs +> foldl1 (fun a e -> ExApp(l, ExApp (l, ExLid (l, "^"), a), e)) in
	  let join_virg xs = xs +> join_gen (ExStr (l, ",")) in

	  let name_module_func = function
	    | TyLid (l, s)                           -> ExLid(l, "string_of_" ^ s)
	    | TyAcc (_, TyUid (_,file), TyLid (_,s)) -> ExAcc(l, ExUid (l,file), ExLid(l, "string_of_" ^ s))
	    | _ -> failwith "pb"
	  in
	  (* TODOSTYLE? function fmatch id patt expr who put the None, ... *)
	  let rec body_ctyp id = function
	    | (TyLid (_) | TyAcc (_)) as c -> ExApp (l,name_module_func c, ExLid (l,id))
              (* | TyUid of loc and string *)

	    | TyQuo (l, s) -> ExApp (l, ExLid(l, "str__of_" ^ s), ExLid (l,id))
	    | TySum (l, xs) ->
		ExMat(l, ExLid (l, id),
		      xs +> List.map (fun (l, s, ctyps) ->
			let newids = xs +> List.map (fun _ -> name ()) in
			match ctyps, newids with
			| ([],_) -> (PaUid(l, s), None, ExStr (l,s))
			| (x::xs,id::ids) ->
			    let patt = zip xs ids +> List.fold_left
				(fun a (e,id) -> PaApp (l, a, PaLid (l,id))
				) (PaApp (l, PaUid(l, s), PaLid(l, id))) in
			    (patt, None,
			     zip (x::xs) (id::ids) +> List.map (fun (e,id) -> body_ctyp id e) +> join_virg +>
			     (fun xs -> [ExStr (l,s); ExStr (l,"(")] @ xs @ [ExStr (l,")")]) +> join_app
			    )
			| _ -> failwith "pb"
				     )
		     )
	    | TyTup (l, xs) ->
		let newids = xs +> List.map (fun _ -> name ()) in
		ExMat(l, ExLid (l,id),
		      [PaTup(l, newids +> List.map (fun id -> PaLid (l,id))),
		      None,
		      zip xs newids +> List.map (fun (e,id) -> body_ctyp id e) +> join_virg +>
			(fun xs -> [ExStr (l,"(")] @ xs @ [ExStr (l,")")]) +> join_app
		      ]
		     )
	    | TyArr (l,c1,c2) -> ExStr(l, "<fun>")
	    | TyApp (l,c1,c2) ->
		let rec extract_all_params acc = function
		  | TyApp (l,c1', c2') -> extract_all_params (acc @ [c2']) c1'
		  | x -> (x, acc) in
		let (type_parameted, params) = extract_all_params [] (TyApp (l, c1, c2)) in
		let funcs = params +> List.map (fun c ->
		  let id = name () in
		  let f = body_ctyp id c in
		  ExFun (l, [PaLid(l, id), None, f]))
		in
		  (match type_parameted with
		  |  (TyLid (_) | TyAcc (_)) as c ->
		      ExApp (l, ExApp (l, name_module_func c, ex_tuple funcs), ExLid (l,id))
		  | _ -> ExStr (l,"<illegal syntax in type, should be a typeconst>")
		  )
	    | TyRec (l, xs) ->
		let newids = xs +> List.map (fun _ -> name ()) in
		ExMat(l, ExLid (l, id),
		      [ PaRec (l, zip xs newids +> List.map (fun ((_,e,_,_), id) -> PaLid(l, e), PaLid(l, id))),
			None,
			zip xs newids
			  +> List.map
			    (fun ((l,s,_b,e),id) -> [ExStr(l,s); ExStr(l," = ");body_ctyp id e] +> join_app)
			  +> join_virg
			  +> (fun xs -> [ExStr (l,"{")] @ xs @ [ExStr (l,"}")]) +> join_app
		      ])

	      (* TODO when needed
	    | TyAli of loc and ctyp and ctyp
	    | TyAny of loc
	    | TyCls of loc and list string
	    | TyLab of loc and string and ctyp
	    | TyMan of loc and ctyp and ctyp
	    | TyOlb of loc and string and ctyp
	    | TyPol of loc and list string and ctyp
	    | TyObj of loc and list (string * ctyp) and bool
	    | TyVrn of loc and list row_field and option (option (list string))
	      and row_field =
	      | RfTag of string and bool and list ctyp
	      | RfInh of ctyp
	      *)
	    | _ -> (ExStr (l, "<not yet implemnted>"))

	  in
	  let f_str_func_params body =
	    (* currifie:
	    param_polymorphs +> List.rev +> List.fold_left (fun acc (str_poly, (_,_)) ->
	      ExFun(l, [(PaLid (l, name_f_str_params str_poly),None, acc)]))
	      body
	    *)
	    if  param_polymorphs = [] then body
	    else ExFun (l, [pa_tuple
			       (param_polymorphs +> List.map (fun (str_poly,(_,_)) ->
				 PaLid (l, "str__of_" ^ str_poly))),
			     None, body])
	  in

	  [(PaLid (l, "string_of_" ^ str), (* let string_of... *)
	   f_str_func_params     (*  str__a str__b ... *)
	      (ExFun(l, [(PaLid (l,"a0"), None,  (* a0 = .... *)
			 (body_ctyp "a0" ctyp))])))    (*  match a with .... *)

	   ;(PaLid (l, "print_" ^ str),
	     (if param_polymorphs = []
	     then fun e -> e
	     else fun e -> (ExFun (l, [PaLid (l, "funs"), None, e]))
	     )
	       (ExFun (l, [(PaLid (l,"a")), None,
			    ExApp (l, ExLid (l, "print_string"),
				   if param_polymorphs = []
				   then ExApp(l, ExLid (l, "string_of_" ^ str), ExLid (l, "a"))
				   else ExApp (l,
					       ExApp (l, ExLid (l, "string_of_" ^ str),
						      ExLid (l, "funs")),
					       ExLid (l, "a")))])))
	  ])
	in
	let recursif = true in
	(StDcl (loc, [(StTyp (loc, tdl));StVal (loc, recursif, funcs +> List.flatten)]))
]]
      ;
    END
;;

*)
(******************************************************************************)

(*
let gen_print_funs loc tdl =
         <:str_item< not yet implemented >>

       let _ =
         EXTEND
           Pcaml.str_item:
             [ [ "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ->
                   let si1 = <:str_item< type $list:tdl$ >> in
                   let si2 = gen_print_funs loc tdl in
                   <:str_item< declare $si1$; $si2$; end >> ] ]
           ;
         END

*)
(*
let fun_name n = "print_" ^ n
let fun_param_name n = "pr_" ^ n
let param_name cnt = "x" ^ string_of_int cnt

let list_mapi f l =
  let rec loop cnt =
    function
        x :: l -> f cnt x :: loop (cnt + 1) l
      | [] -> []
  in
  loop 1 l

let gen_print_type loc t =
  let rec eot =
    function
        <:ctyp< $t1$ $t2$ >> -> <:expr< $eot t1$ $eot t2$ >>
      | <:ctyp< $lid:s$ >> -> <:expr< $lid:fun_name s$ >>
      | <:ctyp< '$s$ >> -> <:expr< $lid:fun_param_name s$ >>
      | _ -> <:expr< fun _ -> print_string "..." >>
  in
  eot t

let gen_call loc n f = <:expr< $f$ $lid:param_name n$ >>

  let gen_print_cons_patt loc c tl =
    let pl =
      list_mapi (fun n _ -> <:patt< $lid:param_name n$ >>)
        tl
    in
    List.fold_left (fun p1 p2 -> <:patt< $p1$ $p2$ >>)
      <:patt< $uid:c$ >> pl

let gen_print_con_extra_syntax loc el =
  let rec loop =
    function
        [] | [_] as e -> e
      | e :: el -> e :: <:expr< print_string ", " >> :: loop el
  in
  <:expr< print_string " (" >> :: loop el @
  [<:expr< print_string ")" >>]

let gen_print_cons_expr loc c tl =
  let pr_con = <:expr< print_string $str:c$ >> in
  match tl with
    [] -> pr_con
  | _ ->
      let pr_params =
        let type_funs = List.map (gen_print_type loc) tl in
        list_mapi (gen_call loc) type_funs
      in
      let pr_all = gen_print_con_extra_syntax loc pr_params in
      let el = pr_con :: pr_all in
      <:expr< do { $list:el$ } >>

      let gen_print_cons (loc, c, tl) =
        let p = gen_print_cons_patt loc c tl in
        let e = gen_print_cons_expr loc c tl in
        p, None, e

let gen_print_sum loc cdl =
  let pwel = List.map gen_print_cons cdl in
  <:expr< fun [ $list:pwel$ ] >>

let gen_one_print_fun loc ((loc, n), tpl, tk, cl) =
      let body =
        match tk with
          <:ctyp< [ $list:cdl$ ] >> -> gen_print_sum loc cdl
        | _ -> <:expr< fun _ -> failwith $str:fun_name n$ >>
      in
      let body =
        List.fold_right
          (fun (v, _) e ->
            <:expr< fun $lid:fun_param_name v$ -> $e$ >>)
          tpl body
      in
      <:patt< $lid:fun_name n$ >>, body

let gen_print_funs loc tdl =
  let pel = List.map (gen_one_print_fun loc) tdl in
  <:str_item< value rec $list:pel$ >>

*)
(*
  let _ =
    DELETE_RULE
      Pcaml.str_item: "type"; LIST1 Pcaml.type_declaration SEP "and"
      END;
    EXTEND
      Pcaml.str_item:
      [ [ "type"; tdl = LIST1 Pcaml.type_declaration SEP "and" ->
        let si1 = <:str_item< type $list:tdl$ >> in
        let si2 = gen_print_funs loc tdl in
        <:str_item< declare $si1$; $si2$; end >> ] ]
      ;
    END
*)
(* by author of camlp4 *)

(******************************************************************************)
(*
     # let expand _ s =
          match s with
            "PI" -> "3.14159"
          | "goban" -> "19*19"
          | "chess" -> "8*8"
          | "ZERO" -> "0"
          | "ONE" -> "1"
          | _ -> "\"" ^ s ^ "\""
        ;;

   Let us call the quotation ``foo''. We can associate the quotation ``foo'' to the above expander ``expand'' by typing:

      # Quotation.add "foo" (Quotation.ExStr expand);;

   We can experiment the new quotation immediately:

      # <:foo<PI>>;;
      - : float = 3.14159
*)

(******************************************************************************)
(*
let add_infix lev op =
          EXTEND
            expr: LEVEL $lev$
              [ [ x = expr; $op$; y = expr -> <:expr< $lid:op$ $x$ $y$ >> ] ]
            ;
          END;;
*)

(******************************************************************************)

(*
    type term =
          Var of string
        | Func of string * term
        | Appl of term * term
      ;;

   The first case, Var, represents variables.

   The second case, Func, represents functions. Its first parameter is the function parameter and its second parameter
   the function body. We write that in concrete syntax [parameter]body.

   The third case, App, represents an application of two lambda terms. We write that in concrete syntax (term1 term2).

   But, for the moment, we just defined a type term, and we can just write these terms using the constructors. Here is
   an example:

      let id = Func ("x", Var "x")
      let k = Func ("x", Func ("y", Var "x"))
      let s =
        Func ("x", Func ("y", Func ("z",
          Appl (Appl (Var "x", Var "y"), Appl (Var "x", Var "z")))))
      let delta = Func ("x", Appl (Var "x", Var "x"))
      let omega = Appl (delta, delta)

   A nice quotation expander would allow us to use concrete syntax. The same piece of program could look like this,
   which is more readable:

      let id = << [x]x >>
      let k = << [x][y]x >>
      let s = << [x][y][z]((x y) (x z)) >>
      let delta = << [x](x x) >>
      let omega = << (^delta ^delta) >>


let gram = Grammar.gcreate (Plexer.gmake ());;
      let term_eoi = Grammar.Entry.create gram "term";;
      let term = Grammar.Entry.create gram "term";;
      EXTEND
         term_eoi: [ [ x = term; EOI -> x ] ];
         term:
            [ [ "["; x = LIDENT; "]"; t = term -> <:expr< Func $str:x$ $t$ >>
              | "("; t1 = term; t2 = term; ")" -> <:expr< Appl $t1$ $t2$ >>
              | x = LIDENT -> <:expr< Var $str:x$ >> ] ]
         ;
      END;;
      let term_exp s = Grammar.Entry.parse term_eoi (Stream.of_string s);;
      let term_pat s = failwith "not implemented term_pat";;
      Quotation.add "term" (Quotation.ExAst (term_exp, term_pat));;
      Quotation.default := "term";;
*)

(******************************************************************************)
