(** Add bounds to a logic. *)

open Logic

module type PARAM =
  sig
    val toks_min : Syntax.t_list
    val toks_max : Syntax.t_list
    val toks_other : Syntax.t_list
    val toks_bot : Syntax.t_list
  end

module Make (Param : PARAM) (X : T) =
  struct
    module Sum1 =
      Sum.Make
	(struct
	  let toks_a1 = Param.toks_other
	  let toks_a2 = []
	  let toks_bot = Param.toks_bot
	end)
    module Sum2 =
      Sum.Make
	(struct
	  let toks_a1 = []
	  let toks_a2 = []
	  let toks_bot = Param.toks_bot
	end)
    module UnitMin =
      Unit.Make
	(struct
	  let toks = Param.toks_min
	end)
    module UnitMax =
      Unit.Make
	(struct
	  let toks = Param.toks_max
	end)
    module S2 = Sum2(UnitMin)(UnitMax)
    module S1 = Sum1(X)(S2)

    include S1

    let props =
      fixpoint
	(fun () ->
	  let x = X.props () in
	  let m = S1.props () in
	  {m with
	   po_le_min = x.po_le_min;
	   cs_le_min = reqand [x.cs_le_min; x.st];
	   cp_le_min = reqand [x.cp_le_min; x.st];
	   po_le_max = x.po_le_max;
	   cs_le_max = reqand [x.cs_le_max; x.st];
	   cp_le_max = reqand [x.cp_le_max; x.st];
	 })

    let le_min f g =
      match f, g with
      | S1.A2 (S2.A1 ()), _ -> true (* Min, _ *)
      | _, S1.A2 (S2.A2 ()) -> true (* _, Max *)
      | S1.A1 x, S1.A1 y -> X.le_min x y
      | _, S1.Bot -> true
      | _, S1.A2 S2.Bot -> true
      | _, _ -> false

    let le_max f g =
      match f, g with
      | S1.A2 (S2.A1 ()), _ -> true (* Min, _ *)
      | _, S1.A2 (S2.A2 ()) -> true (* _, Max *)
      | S1.A1 x, S1.A1 y -> X.le_max x y
      | S1.Bot, _ -> true
      | S1.A2 S2.Bot, _ -> true
      | _, _ -> false


  end
