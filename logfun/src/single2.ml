(** Ensures property sg' by epistemic closure AIK. *)

open Logic

module type PARAM =
  sig
    val toks_certain : Syntax.t_list
    val toks_possible : Syntax.t_list
    val toks_begin : Syntax.t_list
    val toks_sep : Syntax.t_list
    val toks_alt : Syntax.t_list
    val toks_end : Syntax.t_list
  end

module Make (Param : PARAM) (L : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let l = L.props () in
	  {(no_props "Single2") with
	   df = isok;
	   st' = isok;
	   sg' = isok;
	   cs_entails = l.cs_entails;
	   cp'_entails = reqand [l.df; l.cp'_entails];
	   cp_top = l.cp_top;
	   cs_bot = isok;
	   cs_conj = isok;
	   cp_conj = isok;
           cs_disj = isok;
	   cp_disj = isok;
	 })

    type t = C of L.t | P of L.t | O of L.t * L.t list

    let desc = function
      | O (k,ns) -> List.for_all L.desc (k::ns)
      | _ -> false

    let feat = function
      | C x -> L.feat x
      | P x -> L.feat x
      | O (k,ns) -> List.for_all L.desc (k::ns)

    let isvalue = function
      | O _ -> true
      | _ -> false

    let parse = parser
      | [<x = Syntax.parse_tokens_and Param.toks_certain L.parse>] -> C x
      | [<x = Syntax.parse_tokens_and Param.toks_possible L.parse>] -> P x
      | [<k = Syntax.parse_tokens_and Param.toks_begin L.parse; str>] ->
	  match str with parser
	  | [<_ = Syntax.parse_tokens Param.toks_sep;
	      n = L.parse;
	      ns = Syntax.parse_star (Syntax.parse_tokens_and Param.toks_alt L.parse);
	      _ = Syntax.parse_tokens Param.toks_end>] -> O (k,n::ns)
	  | [<_ = Syntax.parse_tokens Param.toks_end>] -> O (k,[])

    let print = function
      | C x -> Param.toks_certain @ L.print x
      | P x -> Param.toks_possible @ L.print x
      | O (k,[]) -> Param.toks_begin @ L.print k @ Param.toks_end
      | O (k,n::ns) ->
	  Param.toks_begin @
	  L.print k @
	  Param.toks_sep @ L.print n @
	  List.concat (List.map (fun n -> Param.toks_alt @ L.print n) ns) @
	  Param.toks_end

    let top () = C (L.top ())

    let entails f g =
      let l_equiv x y = L.entails x y && L.entails y x in
      match f, g with
      | C x, C y -> L.entails x y
      | C x, P y -> L.entails x y
      | P x, P y -> L.entails x y
      | O (k,_), C y -> L.entails k y
      | O (k,[]), P y -> L.entails k y
      | O (k,ns), P y -> List.exists
	    (fun n ->
	      try L.entails (L.conj k n) y
	      with Not_found -> L.entails k y || L.entails n y)
	    ns
      | O (k1,ns1), O (k2,ns2) ->
	  l_equiv k1 k2 &&
	  List.for_all (fun n1 ->
	    List.exists (fun n2 ->
	      l_equiv n1 n2) ns2) ns1 &&
	  List.for_all (fun n2 ->
	    List.exists (fun n1 ->
	      l_equiv n1 n2) ns1) ns2
      | _ -> false

    let conj f g =
      match f, g with
      | C x, C y -> C (L.conj x y)
      | C x, P y -> P (L.conj x y)
      | P x, C y -> P (L.conj x y)
      | _, _ -> raise Not_found

  end

