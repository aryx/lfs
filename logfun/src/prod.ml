(** The product of the domains and languages of 2 sub-logics. *)

open Logic

module type PARAM =
  sig
    val toks_pre : Syntax.t_list
    val toks_sep : Syntax.t_list
    val toks_suf : Syntax.t_list
    val toks_bot : Syntax.t_list
  end

module Make (Param : PARAM) (L1 : T) (L2 : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let l1 = L1.props () in
	  let l2 = L2.props () in
	  {(no_props "Prod") with
	   df = reqand [l1.df; l2.df];
	   st' = reqand [l1.st'; l2.st'];
	   sg' = reqand [l1.sg'; l2.sg'];
	   po_entails = reqand [l1.po_entails; l2.po_entails];
	   cs_entails = reqand [l1.cs_entails; l1.cs_bot; l2.cs_entails; l2.cs_bot];
	   cp_entails = reqand [l1.cp_entails; l1.reduced_bot; l2.cp_entails; l2.reduced_bot];
	   cp'_entails = reqand [l1.cp'_entails; l1.reduced_bot'; l2.cp'_entails; l2.reduced_bot'];
	   cp_top = reqand [l1.cp_top; l2.cp_top];
	   cs_bot = isok;
	   defst_conj = reqand [l1.defst_conj; l2.defst_conj];
	   cs_conj = reqand [l1.cs_conj; l2.cs_conj];
	   cp_conj = reqand [l1.cp_conj; l2.cp_conj];
	   cs_disj = reqand [l1.cs_conj; l1.cs_disj; l2.cs_conj; l2.cs_disj];
	   cp_disj = isok;
	   reduced_bot = reqand [l1.reduced_bot; l2.reduced_bot];
	   reduced_bot' = reqand [l1.reduced_bot'; l2.reduced_bot'];
	   reduced_right = reqand
	     [l1.cs_entails; l1.cp_entails; l1.cp_top; l1.cs_bot; l1.defst_conj; l1.cp_conj; l1.reduced_right;
	      l2.cs_entails; l2.cp_entails; l2.cp_top; l2.cs_bot; l2.defst_conj; l2.cp_conj; l2.reduced_right];
	 })

    type t = Bot | Other of L1.t * L2.t

    let desc = function
      | Bot -> false
      | Other (x1,x2) -> L1.desc x1 && L2.desc x2

    let feat = function
      | Bot -> false
      | Other (x1,x2) -> L1.feat x1 && L2.feat x2

    let compare f g =
      match f, g with
      | Bot, Bot -> 0
      | Bot, _ -> -1
      | _, Bot -> 1
      | Other (x1,x2), Other (y1,y2) -> Common.compare_pair (L1.compare, L2.compare) (x1,x2) (y1,y2)

    let isvalue = function
      | Bot -> false
      | Other (x1,x2) -> L1.isvalue x1 && L2.isvalue x2

    let parse = parser
      | [<x1 = Syntax.parse_tokens_and Param.toks_pre L1.parse;
          x2 = Syntax.parse_tokens_and Param.toks_sep L2.parse ?? "Syntax error in the second part of a pair, after: " ^ Syntax.stringizer (L1.print x1);
	  _ = Syntax.parse_tokens Param.toks_suf ?? "Syntax error: missing end of a pair: " ^ Syntax.stringizer Param.toks_suf
         >] -> Other (x1, x2)
      | [<_ = Syntax.parse_tokens Param.toks_bot>] -> Bot

    let print = function
      | Bot -> Param.toks_bot
      | Other (x1,x2) ->
	  Param.toks_pre @ L1.print x1 @ Param.toks_sep @ L2.print x2 @ Param.toks_suf

    let top () = Other (L1.top (), L2.top ())

    let bot () = Bot

    let entails f g =
      match f, g with
      | Bot, _ -> true
      | Other (x1,x2), Bot ->
	  ((try L1.entails x1 (L1.bot ()) with Not_found -> false) ||
	  (try L2.entails x2 (L2.bot ()) with Not_found -> false))
      | Other (x1,x2), Other (y1,y2) ->
	  L1.entails x1 y1 && L2.entails x2 y2

(*    let share (x1,x2) (y1,y2) = L1.share x1 y1 & L2.share x2 y2 *)

    let conj f g =
      match f, g with
      | Bot, _
      | _, Bot -> Bot
      | Other (x1,x2), Other (y1,y2) -> Other (L1.conj x1 y1, L2.conj x2 y2)

    let disj f g =
      match f, g with
      | Bot, _ -> [g]
      | _, Bot -> [f]
      | Other (x1,x2), Other (y1,y2) ->
	  let res = [Other (x1,x2); Other (y1,y2)] in
	  let res =
	    try
	      let z1 = L1.conj x1 y1 in
	      List.fold_left
		(fun res z2 -> Other (z1,z2)::res)
		res
		(L2.disj x2 y2)
	    with Not_found -> res in
	  let res =
	    try
	      let z2 = L2.conj x2 y2 in
	      List.fold_left
		(fun res z1 -> Other (z1,z2)::res)
		res
		(L1.disj x1 y1)
	    with Not_found -> res in
	  res

    let add f g =
      match f, g with
      | Bot, _ -> Bot
      | _, Bot -> Bot
      | Other (x1,x2), Other (y1,y2) -> Other (L1.add x1 y1, L2.add x2 y2)

    let sub f g =
      match f, g with
      | Bot, _ -> raise Not_found
      | _, Bot -> f
      | Other (x1,x2), Other (y1,y2) ->
	  if entails f g then
	    try Other (x1, L2.sub x2 y2)
	    with Not_found -> Other (L1.sub x1 y1, x2)
	  else f

    let features = function
      | Bot -> LSet.empty ()
      | Other (x1,x2) ->
	  let lf1 = match L1.features x1 with [] -> [(true,x1)] | l -> l in
	  let lf2 = match L2.features x2 with [] -> [(true,x2)] | l -> l in
	  List.fold_left
	    (fun res1 (vis1,f1) ->
              List.fold_left
		(fun res2 (vis2,f2) ->
		  LSet.add (vis1 && vis2, Other (f1,f2)) res2
		) res1 lf2
	    ) (LSet.empty ()) lf1

    let terms = function
      | Bot -> []
      | Other (f1,f2) -> (L1.terms f1) @ (L2.terms f2)

    let axiom f g_opt =
      match f, g_opt with
      | Bot, _
      | _, Some Bot -> LSet.empty ()
      | Other (x1,x2), _ ->
	  let y1_opt, y2_opt =
	    match g_opt with
	    | None -> None, None
	    | Some Bot -> assert false
	    | Some (Other (y1,y2)) -> Some y1, Some y2 in
	  let t1 = L1.axiom x1 y1_opt in
	  let t2 = L2.axiom x2 y2_opt in
	  LSet.union t1 t2

    let gen f g hs =
      match f, g with
      | Bot, _
      | _, Bot -> LSet.empty ()
      | Other (f1,f2), Other (g1,g2) ->
	  let hs = Common.mapfilter (function Bot -> None | Other (h1,h2) -> Some (h1,h2)) hs in
	  if hs=[]
	  then
            let xs1 = L1.gen f1 g1 [] in
            let xs2 = if xs1 = [] then [] else L2.gen f2 g2 [] in
            List.fold_right
	      (fun x1 res ->
		List.fold_right
		  (fun x2 res' ->
		    LSet.add (Other (x1,x2)) res'
		  ) xs2 res
	      ) xs1 []
	  else
            let hs1, hs2 = List.split hs in
            let hs1, hs2 = LSet.of_list hs1, LSet.of_list hs2 in
            let xs1 =
              List.fold_left
		(fun res x2 ->
		  List.fold_left
		    (fun res' h1 ->
		      if List.exists (fun (h1,h2) -> entails (Other (h1,h2)) (Other (h1,x2))) hs
		      then res
		      else LSet.add (Other (h1,x2)) res
		    ) res hs1
		) (LSet.empty ()) (L2.gen f2 g2 hs2) in
            let xs2 =
              List.fold_left
		(fun res x1 ->
		  List.fold_left
		    (fun res' h2 ->
		      if List.exists (fun (h1,h2) -> entails (Other (h1,h2)) (Other (x1,h2))) hs
		      then res
		      else LSet.add (Other (x1,h2)) res
		    ) res hs2
		) (LSet.empty ()) (L1.gen f1 g1 hs1) in
            LSet.union xs1 xs2


    let simpl = function
      | Bot -> [<>]
      | Other (x1,x2) ->
	  [<Common.stream_map (fun x1' -> Other (x1',x2)) (L1.simpl x1);
            Common.stream_map (fun x2' -> Other (x1,x2')) (L2.simpl x2)>]

  end
