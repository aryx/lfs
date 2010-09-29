
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
	  {(no_props "Enum") with
	   df = e.df;
	   st' = e.st';
	   sg' = e.sg';
	   cs_entails = reqand [e.cs_entails; e.cp_top; e.cs_bot; e.cs_disj];
	   cp_entails = e.reduced_right;
	   cp'_entails = e.reduced_right;
	   cp_top = e.cp_top;
	   cs_bot = isok;
	   defst_conj = isok;
	   cs_conj = isok;
	   cp_conj = isok;
	   cs_disj = isok;
	   cp_disj = isok;
	   reduced_right = e.reduced_right;
	   reduced_bot = e.reduced_bot;
	   reduced_bot' = e.reduced_bot';
	   reduced_top = e.reduced_right;
	 })

    type t = E.t list

    let feat f = List.for_all E.feat f

    let desc = function
      | [d] -> E.desc d
      | _ -> false

    let isvalue = function
      | [e] -> E.isvalue e
      | _ -> false

    let rec compare f g =
      match f, g with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | x::xs, y::ys ->
	  let c = E.compare x y in
	  if c = 0
	  then compare xs ys
	  else c

    module S = Set.Make (struct type t = E.t let compare = E.compare end)

    let disj_saturation : t -> t =
      let rec aux gen acc =
	if S.is_empty gen
	then S.elements acc
	else
	  let x = S.choose gen in
	  let gen', acc' =
	    S.fold
	      (fun e (gen', acc') ->
		List.fold_left
		  (fun (gen'', acc'') d ->
		    if S.mem d acc''
		    then (gen'', acc'')
		    else (S.add d gen'', acc'')
		  )
		  (gen', acc') (E.disj x e)
	      )
	      acc (gen, acc) in
	  aux (S.remove x gen') (S.add x acc') in
      fun l ->
	aux (List.fold_right S.add l S.empty) S.empty

	
    open Param
    let (++) toks p = Syntax.parse_tokens_and toks p
    let (~~) toks = Syntax.parse_tokens toks

    let rec parse = parser
      | [<f = toks_begin ++ parse_suite_elt>] -> S.elements f
      | [<f = parse_singleton>] -> S.elements f
      |	[<_ = ~~ toks_empty>] -> []
    and parse_suite_elt = parser
      |	[<e = parse_elt; f = parse_suite_sep>] -> S.add e f
      | [<_ = ~~ toks_end>] -> S.empty
    and parse_suite_sep = parser
      | [<f = toks_sep ++ parse_suite_elt>] -> f
      | [<_ = ~~ toks_end>] -> S.empty
    and parse_singleton str =
      if no_begin_end_when_singleton
      then match str with parser [<e = parse_elt>] -> S.add e S.empty
      else raise Stream.Failure
    and parse_elt = parser
      | [<e = E.parse>] -> e

    let rec print = function
      | [] -> toks_empty
      | [e] when no_begin_end_when_singleton -> print_elt e
      | f -> toks_begin @ print_suite f
    and print_suite = function
      |	[] -> toks_end
      | [e] -> print_elt e @ toks_end
      |	e::f -> print_elt e @ toks_sep @ Token.PP_tilda :: print_suite f
    and print_elt = function
      | e -> E.print e

    let top () = [E.top ()]

    let bot () = []

    let entails f g =
      let g_sat = disj_saturation g in
      List.for_all
	(fun x ->
	  (try E.entails x (E.bot ()) with Not_found -> false) ||
	  List.exists
	    (fun y ->
	      (try E.entails (E.top ()) y with Not_found -> false) ||
	      E.entails x y)
	    g_sat)
	f

    let disj f g = [List.sort E.compare (disj_saturation (Common.merge_max E.entails f g))]

    let features = function
      | [d] ->
	  List.map
	    (fun (vis,x) -> (vis,[x]))
	    (E.features d)
      | _ -> []

    let terms f =
      List.fold_left
	(fun res e -> E.terms e @ res)
	[]
	f

    let axiom f g_opt =
      match f, g_opt with
      | [e], None -> E.axiom e None
      | [e], Some [e'] -> E.axiom e (Some e')
      | _, _ -> []

    let add ld le =
      match ld, le with
      | [d], [e] -> [E.add d e]
      | _ -> raise Not_found

    let sub ld le =
      match ld with
      | [d] -> [List.fold_left E.sub d le]
      | _ -> raise Not_found


  end
