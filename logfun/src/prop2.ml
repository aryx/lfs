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
      |	Not of t
      |	And of t LSet.t
      |	Or of t LSet.t
      |	Atom of A.t
      | T of string (* Term string *)


(* classification of formulas for the subsomption *)
    type cat = Alpha of t list | Beta of t list | Pos of A.t | Neg of A.t | TPos of string | TNeg of string

    let rec classif : t -> cat = function 
      |	Not (Not q) -> classif q
      |	Not (And l) -> Beta (List.map (fun x -> Not x) l)
      |	Not (Or l) -> Alpha (List.map (fun x -> Not x) l)
      |	Not (Atom a) -> Neg a
      | Not (T t) -> TNeg t
      |	And l -> Alpha l
      |	Or l -> Beta l
      |	Atom a -> Pos a
      | T t -> TPos t

(* internal operations*)

    let intern_conj p q =
      let p' =
	match p with
	| Not (Or l) -> And (List.fold_left (fun res x -> LSet.add (Not x) res) (LSet.empty ()) l)
	| _ -> p in
      let q' =
	match q with
	| Not (Or l) -> And (List.fold_left (fun res x -> LSet.add (Not x) res) (LSet.empty ()) l)
	| _ -> q in
      match p', q' with
      | And [], And l2 -> q'
      | And l1, And [] -> p'
      | And l1, And l2 -> And (LSet.union l1 l2)
      | And l1, _ -> And (LSet.add q l1)
      | _, And l2 -> And (LSet.add p l2)
      | _, _ -> And (LSet.of_list [p;q])


    let intern_disj p q =
      let p' =
	match p with
	| Not (And l) -> Or (List.fold_left (fun res x -> LSet.add (Not x) res) (LSet.empty ()) l)
	| _ -> p in
      let q' =
	match q with
	| Not (And l) -> Or (List.fold_left (fun res x -> LSet.add (Not x) res) (LSet.empty ()) l)
	| _ -> q in
      match p', q' with
      | Or [], Or l2 -> q'
      | Or l1, Or [] -> p'
      | Or l1, Or l2 -> Or (LSet.union l1 l2)
      | Or l1, _ -> Or (LSet.add q l1)
      | _, Or l2 -> Or (LSet.add p l2)
      | _, _ -> Or (LSet.of_list [p;q])

    let intern_disj_l = function
      | [] -> Or []
      | [x] -> x
      | l -> Or (LSet.of_list l)

    let intern_neg = function
      | Not x -> x
      | p -> Not p


(* fold on formulas as propositions *)
    let rec fold ~n ~a ~o ~atom ~term = function
      |	Not q -> n (fold ~n ~a ~o ~atom ~term q)
      |	And l -> a (List.map (fold ~n ~a ~o ~atom ~term) l)
      |	Or l -> o (List.map (fold ~n ~a ~o ~atom ~term) l)
      |	Atom x -> atom x
      | T t -> term t


(* logical operations *)
(* ------------------ *)

(* theory (axioms) *)

    let necs : (string, t) Hashtbl.t = Hashtbl.create 10
    let sufs : (string, t) Hashtbl.t = Hashtbl.create 10

    let axiom f g_opt = (* BEWARE: this definition is not compatible for use in logic cache *)
      ( match f, g_opt with
      | T t, None -> Hashtbl.remove necs t; Hashtbl.remove sufs t
      | T t, Some nec -> Hashtbl.replace necs t nec
      | suf, Some (T t) -> Hashtbl.replace sufs t suf
      | _, _ -> ());
      LSet.empty ()


    let rec feat = function
      | Not p -> feat p
      | And l -> List.for_all feat l
      | Or l -> List.for_all feat l
      | Atom a -> A.feat a
      | T t -> true (*try feat (Hashtbl.find theory t) with Not_found -> true*)

    let rec desc p =
      let (all,some) = desc_aux p in
      all && some
    and desc_aux p =
      match classif p with
      | Alpha l ->
	  List.fold_left
	    (fun (all,some) p ->
	      let (allp,somep) = desc_aux p in
	      (all && allp, some || somep))
	    (true, false) l
      | Beta l ->
	  List.fold_left
	    (fun (all,some) p ->
	      let (allp,somep) = desc_aux p in
	      (all && allp, some && somep))
	    (true,true) l
      | Pos x -> (A.desc x || A.feat x, A.desc x)
      | Neg x -> (A.feat x, false)
      | TPos t -> (true,true)
(* t = pos and def
	  let (all,some) =
	    try
	      let def = Hashtbl.find theory t in
	      desc_aux def
	    with Not_found -> (true,false) in (* default def is all = and [] *)
	  (true && all, true || some)
*)
      | TNeg t ->  (true,false)
(* not t = neg or not def
	  let (all,some) =
	    try
	      let def = Hashtbl.find theory t in
	      desc_aux (intern_neg def)
	    with Not_found -> (true,true) in (* default def is non = or [] *)
	  (true && all, false && some)
*)

    let top () = And []

    let bot () = Or []



(* entailment *)
(* ---------- *)

(* thm implements sequents of LeanTaP as revisited by Fitting, and generalized on atoms *)

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


    let rec thm : string list -> string list -> delta_pos -> delta_neg -> t list -> bool =
      fun term_pos term_neg delta_pos delta_neg -> function
	| [] -> false
	| x::gamma -> (match classif x with
	  | Beta l -> thm term_pos term_neg delta_pos delta_neg (l @ gamma)
	  | Alpha l -> List.for_all (fun x -> thm term_pos term_neg delta_pos delta_neg (x::gamma)) l
	  | TNeg t ->
	      if List.mem t term_neg
	      then true
	      else
		let gamma' =
		  try Not (Hashtbl.find necs t)::gamma
		  with Not_found -> gamma in
		thm (t::term_pos) term_neg delta_pos delta_neg gamma'
	  | TPos t ->
	      if List.mem t term_pos
	      then true
	      else
		let gamma' =
		  try Hashtbl.find sufs t::gamma
		  with Not_found -> gamma in
		thm term_pos (t::term_neg) delta_pos delta_neg gamma'
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
	      axiom_pos a' delta_neg or thm term_pos term_neg (a'::delta_pos') delta_neg gamma
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
		thm term_pos term_neg delta_pos (cpt+1,negs') gamma
	      with Proved -> true)


    let entails q q' =
      match q, q' with
      | Atom a, Atom b ->
	  A.entails a b (* shorthand *)
      | _, _ ->
	  thm [] [] delta_pos_init delta_neg_init [Not q; q']


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
	  with Not_found -> intern_conj p q)
      | _ ->
	  if entails p q then p
	  else if entails q p then q
	  else intern_conj p q

    let disj p q =
(*
      if p = All or q = All then [All]
      else if p = Non then [q]
      else if q = Non then [p]
      else [Or (p,q)]
*)
      match p, q with
      | Atom a, Atom b ->
	  [intern_disj_l (List.map (fun x -> Atom x) (A.disj a b))]
      | _ ->
	  if entails p q then [q]
	  else if entails q p then [p]
	  else [intern_disj p q]

    let neg = function
      | Not p -> p
      | p -> Not p


    let wrong_prop s = "Syntax error: '(', '" ^ Syntax.stringizer Param.toks_not ^ "', '" ^ Syntax.stringizer Param.toks_all ^ "', '" ^ Syntax.stringizer Param.toks_none ^ "' or atom expected after '" ^ s ^ "'"
    let wrong_term s = "Syntax error: '(', '" ^ Syntax.stringizer Param.toks_not ^ "' or atom expected after '" ^ s ^ "'"
    let wrong_fact s = "Syntax error: '(', '" ^ Syntax.stringizer Param.toks_not ^ "' or atom expected after '" ^ s ^ "'"

    let rec parse : Syntax.t_stream -> t = parser
      | [<_ = Syntax.parse_tokens Param.toks_all>] -> And []
      | [<_ = Syntax.parse_tokens Param.toks_none>] -> Or []
      | [<q = parse_term; f = parse_suite>] -> f q
    and parse_suite : Syntax.t_stream -> (t -> t) = parser
      | [<q = Syntax.parse_tokens_and Param.toks_and parse>] -> (fun q' -> intern_conj q' q)
      | [<q = Syntax.parse_tokens_and Param.toks_except parse_fact>] -> (fun q' -> intern_conj q' (intern_neg q))
      | [<>] -> (fun q' -> q')
    and parse_term : Syntax.t_stream -> t = parser
      | [<q = parse_fact; f = parse_term_suite>] -> f q
    and parse_term_suite : Syntax.t_stream -> (t -> t) = parser
      | [<q = Syntax.parse_tokens_and Param.toks_or parse_term>] -> (fun q' -> intern_disj q' q)
      | [<q = Syntax.parse_tokens_and Param.toks_implies parse_term>] -> (fun q' -> intern_disj (intern_neg q') q)
      | [<>] -> (fun q' -> q')
    and parse_fact : Syntax.t_stream -> t = parser
      | [<'Token.LeftPar; q = parse ?? wrong_prop "("; 'Token.RightPar ?? "Syntax error: missing ')' after proposition">] -> q
      | [<q = Syntax.parse_tokens_and Param.toks_not parse_fact>] -> intern_neg q
      | [<'Token.Term t>] -> T t
      | [<a = A.parse>] -> Atom a

    let rec print p =
      print2 3 p
    and print2 prec = function
      | And [] -> Param.toks_all
      | And [x] -> print2 prec_and x
      |	And (x::l) -> Syntax.add_par prec prec_and
	    (print2 prec_and x @ Token.PP_tilda :: Param.toks_and @ Token.PP_tilda :: print2 prec_and (And l))
      |	Or [] -> Param.toks_none
      | Or [x] -> print2 prec_or x
      | Or (Not x::l) -> Syntax.add_par prec prec_or
	    (print2 prec_or x @ Token.PP_tilda :: Param.toks_implies @ Token.PP_tilda :: print2 prec_or (Or l))
      |	Or (x::l) -> Syntax.add_par prec prec_or
	    (print2 prec_or x @ Token.PP_tilda :: Param.toks_or @ Token.PP_tilda :: print2 prec_or (Or l))
      |	Not q -> Param.toks_not @ Token.PP_tilda :: print2 prec_not q
      | Atom a -> A.print a
      | T t -> [Token.Term t]
    and prec_and = 3
    and prec_or = 2
    and prec_not = 1

  end
