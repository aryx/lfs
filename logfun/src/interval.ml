(** Intervals over values from the sub-logic. *)

open Logic

module type PARAM =
  sig
    val toks_equal : Syntax.t_list
    val toks_pre : Syntax.t_list
    val toks_sep : Syntax.t_list
    val toks_suf : Syntax.t_list
  end

module Make (Param : PARAM) (Val : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let v = Val.props () in
	  {(no_props "Interval") with
	   df = v.df;
	   st = reqand [v.st; v.cs_le_min; v.cs_le_max];
	   st' = reqand [v.st'; v.cs_le_min; v.cs_le_max];
	   sg' = v.sg';
	   po_entails = reqand [v.po_le_min; v.po_le_max];
	   cs_entails = reqand [v.cs_le_min; v.cs_le_max];
	   cp_entails = reqand [v.cs_le_min; v.cs_le_max; v.cp_le_min; v.cp_le_max];
	   cp'_entails = reqand [v.cs_le_min; v.cs_le_max; v.cp_le_min; v.cp_le_max];
	   cp_top = isok;
	   cs_bot = isok;
	   cs_conj = isok;
	   cp_conj = isok;
	   cs_disj = v.cs_le_overlap;
	   cp_disj = reqand [v.cs_le_min; v.cs_le_max];
	   reduced_bot = reqand [v.st; v.cs_le_min; v.cs_le_max];
	   reduced_bot' = reqand [v.st'; v.cs_le_min; v.cs_le_max];
	   reduced_right = reqand [v.total_le; v.cs_le_min; v.cs_le_max; v.cp_le_min; v.cp_le_max; v.cp_le_overlap];
	 })

    type t = Val.t * Val.t  (* invariant: in (a,b) one has (Val.le_min a b && Val.le_max a b) *)

    let desc (a,b) = a=b && Val.desc a (* in order to get property df *)

    let feat (a,b) = Val.feat a && Val.feat b

    let isvalue (a,b) = a=b && Val.isvalue a

    let compare f g =
      Common.compare_pair (Val.compare, Val.compare) f g

    let entails (a1,b1) (a2,b2) = Val.le_min a2 a1 && Val.le_max b1 b2

    let disj (a1,b1) (a2,b2) =
      if Val.le_min a1 a2 then
	if Val.le_max b2 b1 then
	  [(a1,b1)]
	else if Val.le_max b1 b2 && Val.le_overlap a2 b1 then
	  [(a1,b2)]
	else
	  [(a1,b1); (a2,b2)]
      else if Val.le_min a2 a1 then
	if Val.le_max b1 b2 then
	  [(a2,b2)]
	else if Val.le_max b2 b1 && Val.le_overlap a1 b2 then
	  [(a2,b1)]
	else
	  [(a1,b1); (a2,b2)]
      else
	[(a1,b1); (a2,b2)]

    let features (a,b) =
      if a=b
      then LSet.of_list (List.map (fun (vis,x) -> vis,(x,x)) (Val.features a))
      else LSet.empty ()

    let gen (f1,f2) (g1,g2) hs =
      let x1 = if Val.le_min f1 g1 then f1 else g1 in
      let x2 = if Val.le_max f2 g2 then g2 else f2 in
      if List.exists (fun h -> entails h (x1,x2)) hs
      then LSet.empty ()
      else LSet.singleton (x1,x2)

    let parse = parser
      |	[<a = Syntax.parse_tokens_and Param.toks_equal Val.parse>] -> (a,a)
      |	[<a = Syntax.parse_tokens_and Param.toks_pre Val.parse; b = Syntax.parse_tokens_and Param.toks_sep Val.parse ?? "Syntax error: value expected in second part of onterval"; _ = Syntax.parse_tokens Param.toks_suf ?? "Syntax error: missing end of interval">] ->
	  if Val.le_min a b && Val.le_max a b
	  then (a,b)
	  else raise (Stream.Error "Syntax error: inconsistent interval")

    let print (a,b) =
      if a = b
      then Param.toks_equal @ Val.print a
      else Param.toks_pre @ Val.print a @ Param.toks_sep @ Val.print b @ Param.toks_suf

    let simpl (a,b) =
      if a=b
      then
        Common.stream_map (fun a' -> (a',a')) (Val.simpl a)
      else
        [<'(a,a); '(b,b);
          Common.stream_map (fun a' -> (a',b)) (Val.simpl a);
          Common.stream_map (fun b' -> (a,b')) (Val.simpl b)>]

  end
