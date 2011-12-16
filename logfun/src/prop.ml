(** Boolean closure of a reduced logic. *)

open Logic

module type PARAM =
  sig
    val toks_all : Syntax.t_list
    val toks_none : Syntax.t_list
    val toks_and : Syntax.t_list
    val toks_or : Syntax.t_list
    val toks_not : Syntax.t_list
    val toks_except : Syntax.t_list
    val toks_implies : Syntax.t_list
    val desc_atom : bool  (* whether descriptors are restricted to descriptor atoms *)
  end

module Make (Param : PARAM) (A : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let a = A.props () in
	  {(no_props "Prop") with
	   df = a.df;
	   st' = if Param.desc_atom then a.st' else requires "Prop" "st'";
	   sg' = if Param.desc_atom then a.sg' else requires "Prop" "sg'";
	   cs_entails = reqand [a.cs_entails; a.cp_conj; a.cs_disj; a.cp_top; a.cs_bot];
	   cp_entails = reqand [a.cs_conj; a.cp_disj; a.reduced];
	   cp'_entails = reqand [a.cs_conj; a.cp_disj; a.reduced'];
	   cp_top = isok;
	   cs_bot = isok;
	   defst_conj = isok;
	   cs_conj = isok;
	   cp_conj = isok;
	   cs_disj = isok;
	   cp_disj = isok;
	   reduced = reqand [a.cs_conj; a.cp_disj; a.reduced];
	   reduced' = reqand [a.cs_conj; a.cp_disj; a.reduced'];
	   reduced_top = reqand [a.cs_conj; a.cp_disj; a.reduced]; (* = reduced *)
	   reduced_bot = reqand [a.cs_conj; a.cp_disj; a.reduced]; (* = reduced *)
	   reduced_right = reqand [a.cs_conj; a.cp_disj; a.reduced]; (* = reduced *)
	 })


    type t =
      | All
      |	Non
      |	Not of t
      |	And of t * t
      |	Or of t * t
      |	Atom of A.t


(* classification of formulas for the subsomption *)
    type cat = T | F | Alpha of t * t | Beta of t * t | Pos of A.t | Neg of A.t

    let rec classif : t -> cat = function
      | All -> T
      |	Non -> F
      |	Not (All) -> F
      |	Not (Non) -> T
      |	Not (Not q) -> classif q
      |	Not (And (q,q')) -> Beta (Not q,Not q')
      |	Not (Or (q,q')) -> Alpha (Not q,Not q')
      |	Not (Atom a) -> Neg a
      |	And (q,q') -> Alpha (q,q')
      |	Or (q,q') -> Beta (q,q')
      |	Atom a -> Pos a

    let rec feat = function
      | All
      | Non -> true
      | Not p -> feat p
      | And (p, q) -> feat p && feat q
      | Or (p, q) -> feat p && feat q
      | Atom a -> A.feat a

    let rec desc p =
      if Param.desc_atom then
	match p with
	| Atom a -> A.desc a
	| _ -> false
      else
	let (all,some) = desc_aux p in
	all && some
    and desc_aux p =
      match classif p with
      | T -> (true, false)
      | F -> (true, true)
      | Alpha (a1,a2) ->
	  let (all1,some1) = desc_aux a1 in
	  let (all2,some2) = desc_aux a2 in
	  (all1 && all2, some1 || some2)
      | Beta (a1,a2) ->
	  let (all1,some1) = desc_aux a1 in
	  let (all2,some2) = desc_aux a2 in
	  (all1 && all2, some1 && some2)
      | Pos x -> (A.desc x || A.feat x, A.desc x)
      | Neg x -> (A.feat x, false)


    let wrong_prop s = "Syntax error: '(', '" ^ Syntax.stringizer Param.toks_not ^ "', '" ^ Syntax.stringizer Param.toks_all ^ "', '" ^ Syntax.stringizer Param.toks_none ^ "' or atom expected after '" ^ s ^ "'"
    let wrong_term s = "Syntax error: '(', '" ^ Syntax.stringizer Param.toks_not ^ "' or atom expected after '" ^ s ^ "'"
    let wrong_fact s = "Syntax error: '(', '" ^ Syntax.stringizer Param.toks_not ^ "' or atom expected after '" ^ s ^ "'"

    let rec parse : Syntax.t_stream -> t = parser
      | [<_ = Syntax.parse_tokens Param.toks_all>] -> All
      | [<_ = Syntax.parse_tokens Param.toks_none>] -> Non
      | [<q = parse_term; f = parse_suite>] -> f q
    and parse_suite : Syntax.t_stream -> (t -> t) = parser
      | [<q = Syntax.parse_tokens_and Param.toks_and parse>] -> (fun q' -> And (q', q))
      | [<q = Syntax.parse_tokens_and Param.toks_except parse_fact>] -> (fun q' -> And (q', Not q))
      | [<>] -> (fun q' -> q')
    and parse_term : Syntax.t_stream -> t = parser
      | [<q = parse_fact; f = parse_term_suite>] -> f q
    and parse_term_suite : Syntax.t_stream -> (t -> t) = parser
      | [<q = Syntax.parse_tokens_and Param.toks_or parse_term>] -> (fun q' -> Or (q', q))
      | [<q = Syntax.parse_tokens_and Param.toks_implies parse_term>] -> (fun q' -> Or (Not q',q))
      | [<>] -> (fun q' -> q')
    and parse_fact : Syntax.t_stream -> t = parser
      | [<'Token.LeftPar; q = parse ?? wrong_prop "("; 'Token.RightPar ?? "Syntax error: missing ')' after proposition">] -> q
      | [<q = Syntax.parse_tokens_and Param.toks_not parse_fact>] -> Not q
      | [<a = A.parse>] -> Atom a

    let rec print p =
      print2 3 p
    and print2 prec = function
      | All -> Param.toks_all
      |	Non -> Param.toks_none
      |	And (q1,q2) -> Syntax.add_par prec prec_and
	    (print2 prec_and q1 @ Token.PP_tilda :: Param.toks_and @ Token.PP_tilda :: print2 prec_and q2)
      | Or (Not q1,q2) -> Syntax.add_par prec prec_or
	    (print2 prec_or q1 @ Token.PP_tilda :: Param.toks_implies @ Token.PP_tilda :: print2 prec_or q2)
      |	Or (q1,q2) -> Syntax.add_par prec prec_or
	    (print2 prec_or q1 @ Token.PP_tilda :: Param.toks_or @ Token.PP_tilda :: print2 prec_or q2)
      |	Not q -> Param.toks_not @ Token.PP_tilda :: print2 prec_not q
      | Atom a -> A.print a
    and prec_and = 3
    and prec_or = 2
    and prec_not = 1

(* simplification of a formula : ex., And (All, X) -> X*)
    let rec simplify : t -> t = function
      | All -> All
      |	Non -> Non
      |	Not q -> (match simplify q with
	| All -> Non
	| Non -> All
	| q' -> Not q')
      |	And (q1,q2) -> (match simplify q1, simplify q2 with
	| All, q2' -> q2'
	| q1', All -> q1'
	| Non, _
	| _, Non -> Non
	| q1', q2' -> And (q1',q2'))
      |	Or (q1,q2) -> (match simplify q1, simplify q2 with
	| All, _
	| _, All -> All
	| Non, q2' -> q2'
	| q1', Non -> q1'
	| q1', q2' -> Or (q1',q2'))
      | Atom a when (try a = A.bot () with Not_found -> false) -> Non
      |	Atom a when (try a = A.top () with Not_found -> false) -> All
      |	q -> q

(* fold on formulas as propositions *)
    let rec fold ~all ~none ~n ~a ~o ~atom = function
      | All -> all
      |	Non -> none
      |	Not q -> n (fold ~all ~none ~n ~a ~o ~atom q)
      |	And (q,q') -> a (fold ~all ~none ~n ~a ~o ~atom q) (fold ~all ~none ~n ~a ~o ~atom q')
      |	Or (q,q') -> o (fold ~all ~none ~n ~a ~o ~atom q) (fold ~all ~none ~n ~a ~o ~atom q')
      |	Atom x -> atom x


    let top () = All

    let bot () = Non

(* theory (axioms) *)
(* --------------- *)

    let theory : t list ref = ref []
    let h_entails : (t*t, bool option) Hashtbl.t = Hashtbl.create 100

    let axiom f g_opt = (* BEWARE: this definition is not compatible for use in logic cache *)
      ( match g_opt with
      | None -> theory := f::!theory
      | Some g -> theory := Or (Not f, g)::!theory);
      Hashtbl.clear h_entails;
      LSet.empty ()


(* entailment *)
(* ---------- *)

(* thm implements sequents of LeanTaP as revisited by Fitting *)

    exception Proved

    type delta_pos = A.t list

    let delta_pos_init = []

    module M = Map.Make(struct type t = int LSet.t let compare = Pervasives.compare end)

    type delta_neg = int * (A.t LSet.t) M.t
	  (* [(s,l)] in the map means l-atoms result from disjunctions over generator atoms whose index is in [s] *)
	  (* the int is the number of generators so far *)

    let delta_neg_init = (0, M.empty)


    let axiom_pos : A.t -> delta_neg -> bool =
      fun pos (cpt,map_negs) ->
	try
	  if try A.entails pos (A.bot ()) with Not_found -> false then raise Proved;
	  M.iter
	    (fun _ negs ->
	      if List.exists (A.entails pos) negs then raise Proved)
	    map_negs;
	  false
	with Proved -> true

    let axiom_neg : delta_pos -> A.t -> bool =
      fun delta_pos neg ->
	List.exists (fun pos -> A.entails pos neg) (try A.top ()::delta_pos with Not_found -> delta_pos)

(*
    let axiom_pos pos l_neg = List.exists (fun neg -> A.entails pos neg) (try A.bot ()::l_neg with Not_found -> l_neg)

    let axiom_neg l_pos neg = List.exists (fun pos -> A.entails pos neg) (try A.top ()::l_pos with Not_found -> l_pos)
*)

    let rec thm : delta_pos -> delta_neg -> t list -> bool =
      fun delta_pos delta_neg -> function
	| [] -> false
	| x::gamma -> (match classif x with
	  | T -> true
	  | F -> thm delta_pos delta_neg gamma
	  | Beta (x1,x2) -> thm delta_pos delta_neg (x1::x2::gamma)
	  | Alpha (x1,x2) -> thm delta_pos delta_neg (x1::gamma) & thm delta_pos delta_neg (x2::gamma)
	  | Neg a ->
	      let a', delta_pos' = (* a' is a specialization of a, and delta_pos' is a subset of delta_pos *)
		Common.fold_while
		  (fun (a,delta_pos) ->
		    let x,l,n =
		      List.fold_left
			(fun (x,l,n) p ->
			  try (A.conj x p,l,n+1)
			  with Not_found -> (x,p::l,n))
			(a,[],0)
			delta_pos in
		    if n=0 then None else Some (x,l))
		  (a,delta_pos) in
	      axiom_pos a' delta_neg or thm (a'::delta_pos') delta_neg gamma
	  | Pos a ->
	      let (cpt,negs) = delta_neg in
	      try
		let _, negs' =
		  Common.fold_while
		    (fun (todo_negs, negs) ->
		      match todo_negs with
		      | [] -> None (* exit while *)
		      | (s,current)::others ->
			  if (try LSet.mem current (M.find s negs) with Not_found -> false) then (* current has already been handled *)
			    Some (others,negs)
			  else if axiom_neg delta_pos current then
			    raise Proved
			  else
			    let todo_negs' = (* extending todo_negs with new disjuncts from current *)
			      M.fold
				(fun s' l' todo_negs' ->
				  if LSet.is_empty (LSet.inter s s')
				  then
				    let s'' = LSet.union s s' in
				    List.fold_left
				      (fun todo_negs'' x ->
					List.map (fun d -> (s'',d)) (A.disj current x) @ todo_negs'')
				      todo_negs'
				      l'
				  else todo_negs')
				negs
				todo_negs in
			    let negs' =
			      let l = try M.find s negs with Not_found -> LSet.empty () in
			      M.add s (LSet.add current l) negs in
			    Some (todo_negs', negs')
		    ) (* while *)
		    ([(LSet.singleton (cpt+1), a)], negs) in
		(* so, as Proved has not been raised, we continue the proof *)
		thm delta_pos (cpt+1,negs') gamma
	      with Proved -> true)

    let entails q q' =
      match q, q' with
      | Atom a, Atom b when !theory = [] ->
	  A.entails a b (* shorthand *)
      | _, _ ->
	  thm delta_pos_init delta_neg_init (Not q :: q' :: List.map (fun x -> Not x) !theory)

(* deprecated
    let atom_conj_list atoms =
      let atom_conj a b =
	if A.entails a b then a
	else if A.entails b a then b
	else A.conj a b in
      List.fold_left
	(fun l a ->
	  let a', l' = (* a' is a specialization of a, and delta_pos' is a subset of delta_pos *)
	    Common.fold_while
	      (fun (a,delta_pos) ->
		let x,l,n =
		  List.fold_left
		    (fun (x,l,n) p ->
		      try (atom_conj x p,l,n+1)
		      with Not_found -> (x,p::l,n))
		    (a,[],0)
		    delta_pos in
		if n=0 then None else Some (x,l))
	      (a,l) in
	  a'::l')
	[]
	atoms

    let rec conj p q =
      let rec flat = function
	| [] -> []
	| All::l -> l
	| And (p,q)::l -> flat (p::q::l)
	| Not (Or (p,q))::l -> flat (Not p::Not q::l)
	| Not (Not p)::l -> flat (p::l)
	| p::l -> p::flat l in
      let atoms, props =
	List.fold_left
	  (fun (atoms,props) -> function
	    | Atom a -> a::atoms, props
	    | p -> atoms, p::props
	  ) ([],[]) (flat [p; q]) in
      let atoms' = atom_conj_list atoms in
      simplify
	(List.fold_right
	   (fun x res -> And (x,res))
	   (List.map (fun a -> Atom a) atoms' @ props)
	   All)
*)

    let conj p q =
(*
      if p = All then q
      else if q = All then p
      else if p = Non or q = Non then Non
      else And (p,q)
*)
      match p, q with
      | Atom a, Atom b ->
	  (try Atom (A.conj a b)
	  with Not_found -> And (p,q))
      | _ ->
	  if entails p q then p
	  else if entails q p then q
	  else And (p, q)

    let disj p q =
(*
      if p = All or q = All then [All]
      else if p = Non then [q]
      else if q = Non then [p]
      else [Or (p,q)]
*)
      match p, q with
      | Atom a, Atom b ->
	  Or (p,q)::List.map (fun x -> Atom x) (A.disj a b)
      | _ ->
	  if entails p q then [q]
	  else if entails q p then [p]
	  else [Or (p, q)]

    let not = function
      | All -> Non
      | Non -> All
      | Not p -> p
      | p -> Not p

  end
