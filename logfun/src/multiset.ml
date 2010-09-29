
open Logic

module type PARAM =
  sig
    val toks_sep : Token.t list
    val toks_begin : Token.t list
    val toks_end : Token.t list
    val toks_empty : Token.t list
    val no_begin_end_when_singleton : bool
  end

module Make (Param : PARAM) (E : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let e = E.props () in
	  {(no_props "Multiset") with
	   df = e.df;
	   st' = e.st';
	   po_entails = e.po_entails;
	   cs_entails = e.cs_entails;
	   cp'_entails = reqand [e.cp'_entails; e.sg'];
	   cp_top = isok;
	   cs_bot = isok;
	   cs_conj = isok;
	   cp_conj = isok;
	   cs_disj = isok;
	   cp_disj = isok;
	 })

    type t = (int * E.t) list

    let feat f = List.for_all (fun (n,e) -> E.feat e) f

    let desc f = List.for_all (fun (n,e) -> E.desc e) f

    let isvalue _ = false

    let rec insert_nelt : int * E.t -> t -> t =
      fun (n,e) -> function
	| [] -> [(n,e)]
	| (n',e')::f ->
	    if E.entails e e' & E.entails e' e
	    then (n+n',e')::f
	    else (n',e')::insert_nelt (n,e) f

    open Param
    let (++) toks p = Syntax.parse_tokens_and toks p
    let (~~) toks = Syntax.parse_tokens toks

    let rec parse = parser
      | [<f = toks_begin ++ parse_suite_elt>] -> List.rev f
      | [<f = parse_singleton>] -> f
      |	[<_ = ~~ toks_empty>] -> []
    and parse_suite_elt = parser
      |	[<ne = parse_nelt; f = parse_suite_sep>] -> insert_nelt ne f
      | [<_ = ~~ toks_end>] -> []
    and parse_suite_sep = parser
      | [<f = toks_sep ++ parse_suite_elt>] -> f
      | [<_ = ~~ toks_end>] -> []
    and parse_singleton str =
      if Param.no_begin_end_when_singleton
      then match str with parser [<ne = parse_nelt>] -> [ne]
      else raise Stream.Failure
    and parse_nelt = parser
      | [<'Token.Nat n; e = E.parse>] -> (n,e)
      | [<e = E.parse>] -> (1,e)

    let rec print = function
      | [] -> toks_empty
      | [ne] when Param.no_begin_end_when_singleton -> print_nelt ne
      | f -> toks_begin @ print_suite f
    and print_suite = function
      |	[] -> toks_end
      | [ne] -> print_nelt ne @ toks_end
      |	ne::f -> print_nelt ne @ toks_sep @ Token.PP_tilda :: print_suite f
    and print_nelt = function
      | (1,e) -> E.print e
      | (n,e) -> Token.Nat n :: Token.PP_tilda :: E.print e


    let top () = []

    let rec entails_loop ress = function
      | [] -> true
      | (0,y)::[] -> true
      | (0,y)::(n',y')::nys ->
	  let ress' = List.map (fun (_,n,x) -> (E.entails x y', n, x)) ress in
	  entails_loop ress' ((n',y')::nys)
      | (n,y)::nys ->
	  (* in ress, the booleans indicates which elements are subsumed by y *)
	  List.exists
	    (fun ress' -> entails_loop ress' ((n-1,y)::nys))
	    (entails_ress_next ress)
    and entails_ress_next = function
      | [] -> []
      | (sub,n,x)::l ->
	  assert (n>0);
	  let res1 = List.map (fun l' -> (sub,n,x)::l') (entails_ress_next l) in
	  if sub then
	    if n=1
	    then l :: res1
	    else ((sub,n-1,x)::l) :: res1
	  else res1

    let rec entails f g =
      match g with
      | [] -> true
      | (n,y)::g' ->
	  let ress = List.map (fun (n,x) -> assert (n>0); (E.entails x y, n, x)) f in
	  entails_loop ress g

    let share f g = true

    let rec features = function
      | [] -> LSet.singleton (true,[])
      |	(n,e)::f ->
	  LSet.add
	    (true,[])
	    (List.fold_right
	       (fun (vis,e') res ->
		 Common.fold_for
		   (fun i res' -> LSet.add (vis,[(i,e')]) res')
		   1 n res)
	       ((*(true,e) ::*) E.features e)
	       (features f))

    let terms f =
      List.fold_left
	(fun res (n,e) -> E.terms e @ res)
	[]
	f

    let axiom f g_opt =
      match f, g_opt with
      | [(n,e)], None -> E.axiom e None
      | [(n,e)], Some [(n',e')] when n>=n' -> E.axiom e (Some e')
      | _, _ -> []

(* conjunction to be proved consistent and complete
    let conj_nelt ld (n,e) =
      let ld' =
	Common.mapfilter
	  (fun (m,d) ->
	    try Some (max m n, E.conj d e)
	    with Not_found ->
	      if n >= m && E.entails e d
	      then None
	      else Some (m,d))
	  ld in
      if entails ld' [(n,e)]
      then ld'
      else (n,e)::ld

    let conj ld le =
      List.fold_left
	(fun res (n,e) -> conj_nelt res (n,e))
	ld
	le
*)


    let add_nelt ld (n,e) =
      let ld' =
	Common.mapfilter
	  (fun (m,d) ->
	    try Some (max m n, E.add d e)
	    with Not_found ->
	      if n >= m && E.entails e d
	      then None
	      else Some (m,d))
	  ld in
      if entails ld' [(n,e)]
      then ld'
      else (n,e)::ld

    let add ld = function
      | [(n,e)] -> add_nelt ld (n,e)
      | _ -> raise Not_found

    let sub_nelt ld (n,e) =
      Common.mapfilter
	(fun (m,d) ->
	  try Some (m, E.sub d e)
	  with Not_found ->
	    if m >= n && E.entails d e
	    then if n-1 > 0 then Some (n-1, d) else None
	    else Some (m,d))
	ld

    let sub ld = function
      | [(n,e)] -> sub_nelt ld (n,e)
      | _ -> raise Not_found

  end

