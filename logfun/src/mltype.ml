
open Token
open Logic

let toks_bot = [Sharp]

module MyAtom =
  Atom.Make
    (struct
      let names _ = true
    end)

module MyTop =
  Top.Make
    (struct
      let toks_top = []
      let feat_top = true
    end)

module MyInterv =
  Openinterval.DotDot

module MyProd =
  Prod.Make
    (struct
      let toks_suf = []
      let toks_sep = []
      let toks_pre = []
      let toks_bot = toks_bot
    end)

module MySum =
  Sum.Make
    (struct
      let toks_a1 = []
      let toks_a2 = []
      let toks_bot = toks_bot
    end)

module MyBracket =
  Bracket.Make
    (struct
      let toks_left = [LeftPar]
      let toks_right = [RightPar]
    end)

module MyBottom =
  Bottom.Make
    (struct
      let toks_bot = toks_bot
    end)

(* --------------------------------------- *)

module PrefixVar =
  Prefix.Make
    (struct
      let toks = [BackQuote]
    end)

module TopVar =
  Top.Make
    (struct
      let toks_top = [Term "_"]
      let feat_top = true
    end)

module VectorArgs =
  Vector.Make
    (struct
      let toks_begin = [LeftPar]
      let toks_end = [RightPar]
      let toks_sep = [Comma]
      let toks_empty = [LeftPar; RightPar]
      let toks_bot = toks_bot
    end)

module TopVal =
  Top.Make
    (struct
      let toks_top = [Term "_"]
      let feat_top = true
    end)

module MultisetTuple =
  Multiset.Make
    (struct
      let toks_begin = [LeftBra]
      let toks_end = [RightBra]
      let toks_sep = [Comma]
      let toks_empty = [LeftBra; RightBra]
      let no_begin_end_when_singleton = false
    end)

module SingleTuple =
  Single.Make
    (struct
      let toks_K = []
      let toks_O = [SemiColon]
      let toks_O_end = []
    end)

module MultisetFun =
  Multiset.Make
    (struct
      let toks_begin = [LeftAcc]
      let toks_end = [RightAcc]
      let toks_sep = [Comma]
      let toks_empty = [LeftAcc; RightAcc]
      let no_begin_end_when_singleton = false
    end)

module SingleFun =
  Single.Make
    (struct
      let toks_K = []
      let toks_O = [Colon]
      let toks_O_end = []
    end)

module SumFun =
  Sum.Make
    (struct
      let toks_a1 = [LT] (* arguments of functions *)
      let toks_a2 = [GT] (* results of functions *)
      let toks_bot = toks_bot
    end)

module TopType =
  Top.Make
    (struct
      let toks_top = [Dot]
      let feat_top = true
    end)

module TopDescr =
  Top.Make
    (struct
      let toks_top = [Interro]
      let feat_top = false
    end)


(* ---------- *)

module Var = TopVar(MyAtom)
module Constr = MyAtom

module Val (Type : T) : T =
  MySum
    (PrefixVar(Var))
    (MySum
       (MyProd(Constr)(VectorArgs(Type)))
       (MyBracket(Type)))

module Tuple (Type : T) =
  MyBottom
    (SingleTuple
       (MultisetTuple
	  (Val(Type))))

module Fun (Type : T) =
  MyBottom
    (SingleFun
       (MultisetFun
	  (SumFun
	     (Tuple(Type))
	     (Tuple(Type)))))

module rec Type : T =
  TopType
    (MySum
       (Tuple(Type))
       (Fun(Type)))

module Make =
  struct
    include Type

    let rec xlc_to_val closed x = function
      | [] -> x
      | c::lc ->
	  let suf = LeftBra :: x @ [RightBra] in
	  let x' = if closed then SemiColon :: suf else suf in
	  let x'' = c @ LeftPar :: x' @ [RightPar] in
	  xlc_to_val closed x'' lc

    let rec lt2_to_tuple = function
      | [] -> assert false
      | [t2] -> t2 @ [RightBra]
      | t2::lt2 -> t2 @ Comma :: lt2_to_tuple lt2

    let rec lt1_to_fun = function
      | [] -> assert false
      | [None] -> [RightAcc]
      | [Some res] -> GT :: res @ [RightAcc]
      | None::l -> lt1_to_fun l
      | Some arg::l -> LT :: arg @ Comma :: lt1_to_fun l

    let lt1_to_type0 closed lt1 =
      match lt1 with
      | [] -> assert false
      | [Some t1] -> t1
      | [None]
      | _ ->
	  let suf = lt1_to_fun lt1 in
	  if closed then Colon :: LeftAcc :: suf else LeftAcc :: suf

    let rec parse_trad = parser
      | [<'Colon; t0 = parse0 true>] -> t0
      | [<t0 = parse0 false>] -> t0
    and parse0 closed = parser
      | [<'Dot>] -> [Dot]
      | [<lt1 = parse00 closed>] -> lt1_to_type0 closed lt1
      | [<lt1 = parse0_suite closed>] -> lt1_to_type0 closed (None :: lt1)
    and parse00 closed = parser
      | [<'Interro; 'Ident _; 'Colon; t1 = parse1 closed; lt1 = parse0_suite closed>] ->
	  let suf = LeftBra :: Term "option" ::LeftPar :: t1 @ [RightPar; RightBra] in
	  let t1_option = if closed then SemiColon :: suf else suf in
	  Some t1_option :: lt1
      | [<t1 = parse1 closed; lt1 = Syntax.parse_opt (parse0_suite closed) []>] -> Some t1 :: lt1
    and parse0_suite closed = parser
      | [<'Minus; 'GT; lt1 = Syntax.parse_opt (parse00 closed) [None]>] -> lt1
    and parse1 closed = parser
      | [<'Star>] ->
	  let suf = [LeftBra; RightBra] in
	  if closed then SemiColon :: suf else suf
      | [<t2 = parse2 closed; lt2 = parse1_suite closed>] ->
	  let suf = lt2_to_tuple (t2::lt2) in
	  if closed then SemiColon :: LeftBra :: suf else LeftBra :: suf
    and parse1_suite closed = parser
      | [<'Star; t2 = parse2 closed; lt2 = parse1_suite closed>] -> t2::lt2
      | [<>] -> []
    and parse2 closed = parser
      | [<'BackQuote; v = Var.parse; lc = parse2_suite>] ->
	  let x = BackQuote :: Var.print v in
	  xlc_to_val closed x lc
      | [<c = parse_constr; str>] ->
	  ( match str with parser
	  | [<'Colon; v = parse2 closed>] -> v
	  | [<lc = parse2_suite>] ->
	      let x = c @ [LeftPar; RightPar] in
	      xlc_to_val closed x lc)
      | [<'LeftPar; t0 = parse0 closed; x, c_opt = parse3 closed t0; lc = parse2_suite>] ->
	  ( match c_opt, lc with
	  | None, [] -> LeftPar :: x @ [RightPar]
	  | None, c::lc -> xlc_to_val closed (c @ LeftPar :: x @ [RightPar]) lc
	  | Some c, lc -> xlc_to_val closed (c @ LeftPar :: x @ [RightPar]) lc)
    and parse2_suite = parser
      | [<c = parse_constr; lc = parse2_suite>] -> c :: lc
      | [<>] -> []
    and parse3 closed t0 = parser
      | [<'RightPar>] ->
	  t0, None
      | [<lt0 = Syntax.parse_plus (parser [<'Comma; t0 = parse0 closed>] -> t0); 'RightPar; c = parse_constr>] ->
	  t0 @ (List.fold_right (fun t0 res -> Comma :: t0 @ res) lt0 []), Some c
    and parse_constr = parser
      | [<'Sharp; p = parse_path>] -> [Term ("#" ^ p)]
      | [<p = parse_path>] -> [Term p]
    and parse_path = parser
      | [<'Ident c>] -> c
      | [<'Term m; 'Dot; p = parse_path>] -> m ^ "." ^ p

    let parse str =
	let toks = parse_trad str in
(*        print_endline (Syntax.stringizer toks);*)
	parse (Syntax.of_list toks)


    let rec list_make x = function
      | 0 -> []
      | n -> x::list_make x (n-1)

    let toks_concat sep = function
      | [] -> assert false
      | x::l -> x @ List.fold_right (fun x res -> sep @ x @ res) l []

    let build_tuple vs =
      match vs with
      | [] -> [Star]
      | _ -> toks_concat [PP_tilda; Star; PP_tilda] vs

    let build_fun (larg,lres) =
      let res = if lres = [] then [] else build_tuple (List.concat lres) in
      let args = if larg = [] then [[]] else List.map build_tuple larg in
      toks_concat [PP_tilda; Minus; GT; PP_tilda] (args @ [res])

    let rec print_trad : Syntax.t_stream -> bool * bool * Syntax.t_list = parser  (* closed under AIK, simple (needs no parentheses, toks *)
      | [<'Dot>] -> false, false, [Dot]
      | [<closed, simple, vs = print_tuple>] -> closed, simple, build_tuple vs
      | [<closed, f = print_fun>] -> closed, false, f
    and print_fun = parser
      | [<'Colon; 'LeftAcc; (larg,lres) = print_fun_suite>] -> true, build_fun (larg,lres)
      | [<'LeftAcc; (larg,lres) = print_fun_suite>] -> false, build_fun (larg,lres)
    and print_fun_suite = parser
      | [<'RightAcc>] -> ([],[])
      | [<'Comma; (res,lvs) = print_nelt; (larg,lres) = print_fun_suite>] ->
	  if res then (larg, lvs @ lres) else (lvs @ larg,lres)
      | [<(res,lvs) = print_nelt; (larg,lres) = print_fun_suite>] ->
	  if res then (larg, lvs @ lres) else (lvs @ larg,lres)
    and print_nelt = parser
      | [<'Nat n; (res,vs) = print_elt>] -> (res,list_make vs n)
      | [<(res,vs) = print_elt>] -> (res,[vs])
    and print_elt = parser
      | [<'LT; _, _, vs = print_tuple>] -> (false,vs)
      | [<'GT; _, _, vs = print_tuple>] -> (true,vs)
    and print_tuple = parser
      | [<'SemiColon; 'LeftBra; simple, vs = print_tuple_suite>] -> true, simple, vs
      | [<'LeftBra; simple, vs = print_tuple_suite>] -> false, simple, vs
    and print_tuple_suite = parser
      | [<'RightBra>] -> false, []
      | [<'Comma; simple, vs1 = print_nval; _, vs2 = print_tuple_suite>] -> simple && vs2=[], vs1 @ vs2
      | [<simple, vs1 = print_nval; _, vs2 = print_tuple_suite>] -> simple && vs2=[], vs1 @vs2
    and print_nval = parser
      | [<'Nat n; simple, v = print_val>] -> simple && n=1, list_make v n
      | [<simple, v = print_val>] -> simple, [v]
    and print_val = parser
      | [<'BackQuote; v = Var.parse>] -> true, BackQuote :: Var.print v
      | [<'Term c; 'LeftPar; simple, xs = print_val_suite>] ->
	  let args =
	    match xs with
	    | [] -> []
	    | [x] when simple -> x @ [PP_tilda]
	    | _ -> LeftPar :: toks_concat [Comma] xs @ [RightPar; PP_tilda] in
	  true, args @ Syntax.list_of_stream (Syntax.from_string c)
      | [<'LeftPar; _, _, x = print_trad; 'RightPar>] ->
	  true (* or false, irrelevant *), LeftPar :: x @ [RightPar]
    and print_val_suite = parser
      | [<'RightPar>] -> false, []
      | [<'Comma; _, simple, x = print_trad; _, xs = print_val_suite>] -> simple, x::xs
      | [<_, simple, x = print_trad; _, xs = print_val_suite>] -> simple, x::xs

    let print f =
      let toks0 = print f in
(*      print_endline (Syntax.stringizer toks0);*)
      let closed, simple, toks = print_trad (Syntax.of_list toks0) in
      if closed then Colon :: PP_tilda :: toks else toks

  end
