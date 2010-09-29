(** Ensures property sg' by epistemic closure AIK. *)

open Logic

module type PARAM =
  sig
    val toks_K : Syntax.t_list
    val toks_O : Syntax.t_list
    val toks_O_end : Syntax.t_list
  end

module Make (Param : PARAM) (L : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let l = L.props () in
	  {(no_props "Single") with
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

    type t = K of L.t | O of L.t

    let desc = function
      | O d -> L.desc d
      | _ -> false

    let feat = function
      | K x -> L.feat x
      | O d -> L.desc d

    let isvalue = function
      | O _ -> true
      | _ -> false

    let parse = parser
      | [<x = Syntax.parse_tokens_and Param.toks_K L.parse>] -> K x
      | [<d = Syntax.parse_tokens_and Param.toks_O L.parse; _ = Syntax.parse_tokens Param.toks_O_end>] -> O d

    let print = function
      | K x -> Param.toks_K @ L.print x
      | O d -> Param.toks_O @ L.print d @ Param.toks_O_end

    let top () = K (L.top ())

    let entails f g =
      match f, g with
      | K x, K y
      | O x, K y -> L.entails x y
      | O x, O y -> L.entails x y && L.entails y x
      | _ -> false

    let features = function
      | K x -> List.map (fun (vis,x) -> (vis,K x)) (L.features x)
      | O d -> List.map (fun (vis,x) -> (vis,K x)) (L.features d)

    let terms = function
      | K x -> L.terms x
      | O x -> L.terms x

    let axiom f g_opt =
      match f, g_opt with
      | K x, None
      | O x, None -> L.axiom x None
      | K x, Some (K y)
      | O x, Some (K y) -> L.axiom x (Some y)
      | _ -> LSet.empty ()

  end

