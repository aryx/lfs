
open Logic

module type PARAM =
  sig
    val formulas : (int * (Syntax.t_list * int list)) list
	(** list of formulas, each formula being in the form (id>0,(toks,l_succ)). *)
  end

module Make (Param : PARAM) =
  struct

    type formula = {toks : Syntax.t_list; entailed : int LSet.t}

    let ht_formulas : (int,formula) Hashtbl.t =
      let ht = Hashtbl.create (List.length Param.formulas) in
      let dico_succ = List.map (fun (id,(_,l)) -> (id, LSet.of_list l)) Param.formulas in
      ignore (Common.fold_while
	(fun (l_id_done, dico_succ_rest) ->
	  if dico_succ_rest = []
	  then None
	  else
	    let entries, others =
	      List.partition
		(fun (id,succ) -> LSet.contains l_id_done succ)
		dico_succ_rest in
	    if entries = []
	    then failwith "Taxo: error while building the logic: a cycle has been found"
	    else try
	      List.iter
		(fun (id,succ) ->
		  Hashtbl.add ht id
		    { toks = fst (List.assoc id Param.formulas);
		      entailed = LSet.add id (LSet.union_r (List.map (fun s -> (Hashtbl.find ht s).entailed) succ))
		    })
		entries;
	      let l_id_done_new = List.fold_left (fun res (id,_) -> LSet.add id res) l_id_done entries in
	      Some (l_id_done_new, others)
	    with Not_found -> failwith "Taxo: error while building the logic: a formula has no given syntax")
	(LSet.empty (), dico_succ));
      ht

    (* set of non-descriptor formulas *)
    let non_desc : int LSet.t =
      List.fold_left
	(fun res (id,(_,l)) -> LSet.union res (LSet.of_list l))
	(LSet.empty ())
	Param.formulas

    type tree = Leaf of Syntax.t_list * int | Node of (Token.t, tree) Hashtbl.t

    let tree_init = Node (Hashtbl.create 10)

    let rec tree_add t toks id =
      match t, toks with
      | Leaf _, []
      | Node _, [] -> failwith "Taxo: error while building the logic: a formula is a prefix of another"
      | Leaf (toks0,id0), tok::toks' ->
	  ( match toks0 with
	  | [] -> failwith "Taxo: error while building the logic: a formula is a prefix of another"
	  | tok0::toks0' ->
	      let ht0 = Hashtbl.create 10 in
	      Hashtbl.add ht0 tok0 (Leaf (toks0',id0));
	      let t0 = Node ht0 in
	      tree_add t0 toks id)
      | Node ht, tok::toks' ->
	  (try
	    let t' = Hashtbl.find ht tok in
	    Hashtbl.replace ht tok (tree_add t' toks' id)
	  with Not_found ->
	    Hashtbl.add ht tok (Leaf (toks',id)));
	  Node ht

    let tree_id : tree =
      List.fold_left
	(fun t (id,(toks,_)) -> tree_add t toks id)
	tree_init
	Param.formulas

(* ---------------------------- *)

    include Default

    let props () =
      {(no_props "Taxo") with
       st' = isok;
       sg' = isok;
       df = isok;
       po_entails = isok;
       cs_entails = isok;
       cp_entails = isok;
       cp'_entails = isok;
       cp_top = isok;
       cs_bot = isok;
       defst_conj = isok;
       cs_conj = isok;
       cp_conj = isok;
       cs_disj = isok;
       cp_disj = isok;
       reduced = isok;
       reduced_bot = isok;
       reduced' = isok;
       reduced_bot' = isok}

    type t = int

    let feat f = true

    let desc f = not (LSet.mem f non_desc)

    let rec parse_aux t str =
      match t with
      | Leaf (toks,id) ->
	  Syntax.parse_tokens toks str;
	  id
      | Node ht ->
	  ( match Stream.peek str with
	  | None -> raise Stream.Failure
	  | Some tok ->
	      try
		let t' = Hashtbl.find ht tok in
		Stream.junk str;
		parse_aux t' str
	      with Not_found ->
		raise Stream.Failure)

    let parse str =
      parse_aux tree_id str

    let print id =
      (Hashtbl.find ht_formulas id).toks

    let entails f g =
      LSet.mem g (Hashtbl.find ht_formulas f).entailed

    let features f =
      List.map (fun id -> (true,id)) (Hashtbl.find ht_formulas f).entailed

  end

