(** Union of the domains and languages of 2 sub-logics. *)

open Logic

module type PARAM = sig
  val toks_a1 : Syntax.t_list
  val toks_a2 : Syntax.t_list
  val toks_bot : Syntax.t_list
end

module Make (Param : PARAM) (A1 : T) (A2 : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let a1 = A1.props () in
	  let a2 = A2.props () in
	  {(no_props "Sum") with
	   df = reqand [a1.df; a2.df];
	   st' = reqand [a1.st'; a2.st'];
	   sg' = reqand [a1.sg'; a2.sg'];
	   po_entails = reqand [a1.po_entails; a2.po_entails];
	   cs_entails = reqand [a1.cs_entails; a1.cs_bot; a2.cs_entails; a2.cs_bot];
	   cp_entails = reqand [a1.cp_entails; a1.reduced_bot; a2.cp_entails; a2.reduced_bot];
	   cp'_entails = reqand [a1.cp'_entails; a1.reduced_bot'; a2.cp'_entails; a2.reduced_bot'];
	   cp_top = isok;
	   cs_bot = isok;
	   defst_conj = reqand [a1.defst_conj; a2.defst_conj];
	   cs_conj = reqand [a1.cs_conj; a2.cs_conj];
	   cp_conj = reqand [a1.cp_conj; a2.cp_conj];
	   cs_disj = reqand [a1.cs_disj; a2.cs_disj];
	   cp_disj = reqand [a1.cp_disj; a2.cp_disj];
	   reduced = reqand [a1.reduced; a2.reduced];
	   reduced_top = reqand [a1.reduced_top; a2.reduced_top];
	   reduced_bot = reqand [a1.reduced_bot; a2.reduced_bot];
	   reduced_bot' = reqand [a1.reduced_bot'; a2.reduced_bot'];
	   reduced_right = reqand [a1.reduced_right; a2.reduced_right];
	 })

    type t = A1 of A1.t | A2 of A2.t | Bot

    let feat = function
      | A1 x -> A1.feat x
      | A2 x -> A2.feat x
      | Bot -> false

    let desc = function
      | A1 x -> A1.desc x
      | A2 x -> A2.desc x
      | Bot -> false

    let compare f g =
      match f, g with
      | A1 _, A2 _ -> -1
      | A2 _, A1 _ -> 1
      | A1 x, A1 y -> A1.compare x y
      | A2 x, A2 y -> A2.compare x y
      | Bot, Bot -> 0
      | Bot, _ -> 1
      | _, Bot -> -1

    let isvalue = function
      | A1 x -> A1.isvalue x
      | A2 x -> A2.isvalue x
      | Bot -> false

    let entails f g =
      match (f,g) with
      | A1 x, A1 y -> A1.entails x y
      | A2 x, A2 y -> A2.entails x y
      | A1 x, _ -> (try A1.entails x (A1.bot ()) with Not_found -> false)
      | A2 x, _ -> (try A2.entails x (A2.bot ()) with Not_found -> false)
      | Bot, _ -> true

(*
    let share f g =
      match (f,g) with
      | _, Top
      | Top, _ -> true
      | A1 x, A1 y -> A1.share x y
      | A2 x, A2 y -> A2.share x y
      | _, _ -> false
*)

    let bot () = Bot

    let conj f g =
      match (f,g) with
      | A1 x, A1 y -> A1 (A1.conj x y)
      | A2 x, A2 y -> A2 (A2.conj x y)
      | _, _ -> Bot

    let disj f g =
      match f, g with
      | A1 x, A1 y -> List.map (fun z -> A1 z) (A1.disj x y)
      | A2 x, A2 y -> List.map (fun z -> A2 z) (A2.disj x y)
      | Bot, Bot -> []
      | Bot, _ -> [g]
      | _, Bot -> [f]
      | _, _ -> [f; g]

    let add f g =
      match (f,g) with
      | A1 x, A1 y -> A1 (A1.add x y)
      | A2 x, A2 y -> A2 (A2.add x y)
      | _, _ -> raise Not_found

    let features f =
      match f with
      | A1 x -> List.map (fun (vis,x) -> vis,A1 x) (A1.features x)
      | A2 x -> List.map (fun (vis,x) -> vis,A2 x) (A2.features x)
      | Bot -> []

    let terms = function
      | A1 x -> A1.terms x
      | A2 x -> A2.terms x
      | Bot -> []

    let axiom f g_opt =
      match f, g_opt with
      | A1 x, None -> A1.axiom x None
      | A1 x, Some (A1 y) -> A1.axiom x (Some y)
      | A2 x, None -> A2.axiom x None
      | A2 x, Some (A2 y) -> A2.axiom x (Some y)
      | _, _ -> LSet.empty ()

    let gen f g hs =
      match (f,g) with
      | A1 x, A1 y ->
         List.map (fun t -> A1 t) (A1.gen x y (List.fold_right (fun h zs -> match (A1 x,h) with A1 x, A1 z -> z::zs | _, _ -> zs) hs []))
      | A2 x, A2 y ->
         List.map (fun t -> A2 t) (A2.gen x y (List.fold_right (fun h zs -> match (A2 x,h) with A2 x, A2 z -> z::zs | _, _ -> zs) hs []))
      | _, _ -> []

    let parse = parser
      | [<x = Syntax.parse_tokens_and Param.toks_a1 A1.parse>] -> A1 x
      | [<x = Syntax.parse_tokens_and Param.toks_a2 A2.parse>] -> A2 x
      | [<_ = Syntax.parse_tokens Param.toks_bot>] -> Bot

(*
    let parse str =
      match Param.toks_top with
      | None -> parse2 str
      | Some [] -> (match str with parser
	| [<f = parse2>] -> f
	| [<>] -> Top)
      | Some toks -> (match str with parser
	| [<_ = Syntax.parse_tokens toks>] -> Top
	| [<f = parse2>] -> f)
*)

    let print = function
      | A1 x -> Param.toks_a1 @ A1.print x
      | A2 x -> Param.toks_a2 @ A2.print x
      | Bot -> Param.toks_bot

    let simpl = function
      | A1 x -> Common.stream_map (fun x -> A1 x) (A1.simpl x)
      | A2 x -> Common.stream_map (fun x -> A2 x) (A2.simpl x)
      | Bot -> [<>]

  end
