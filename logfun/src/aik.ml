(** Closed World Assumption with the Levesque's logic All I Know. *)

open Logic

module type PARAM =
  sig
    val toks_K : Syntax.t_list
    val toks_O : Syntax.t_list
    val toks_bot : Syntax.t_list
  end

module Make (Param : PARAM) (L : T) =
  struct
    include Default

    let unique = isok

    let props =
      fixpoint
	(fun () ->
	  let l = L.props () in
	  {(no_props "Aik") with
	   st' = isok;
	   sg' = isok;
	   po_entails = l.po_entails;
	   cs_entails = l.cs_entails;
	   cp_entails = l.cp_entails;
	   cp'_entails = l.cp'_entails;
	   cp_top = l.cp_top;
	   cs_bot = isok;
	   cs_conj = reqand [unique; l.cs_entails; l.cp'_entails; l.cs_conj];
	   cp_conj = reqand [unique; l.cs_entails; l.cp'_entails; l.cp_conj];
           cs_disj = isok;
	   cp_disj = isok;
	   reduced' = reqand [unique; l.cp'_entails];
	 })

    type t = K of L.t | O of L.t | Bot

    let desc = function
      | O _ -> true
      | _ -> false

    let feat = function
      | K _ -> true
      | _ -> false

    let isvalue = function
      | O _ -> true
      | _ -> false

    let parse = parser
      | [<_ = Syntax.parse_tokens Param.toks_bot>] -> Bot
      | [<x = Syntax.parse_tokens_and Param.toks_K L.parse>] -> K x
      | [<d = Syntax.parse_tokens_and Param.toks_O L.parse>] -> O d

    let print = function
      | Bot -> Param.toks_bot
      | K x -> Param.toks_K @ L.print x
      | O d -> Param.toks_O @ L.print d

    let top () = K (L.top ())

    let bot () = Bot

    let entails f g =
      match f, g with
      | Bot, _ -> true
      | K x, K y -> L.entails x y
      | O d, K x -> L.entails d x
      | _, _ -> false

    let conj f g =
      match f, g with
      | Bot, _ -> Bot
      | _, Bot -> Bot
      | O _, _ -> if entails f g then f else Bot
      | _, O _ -> if entails g f then g else Bot
      | K x, K y -> K (L.conj x y)

  end

