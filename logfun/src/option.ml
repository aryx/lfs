
open Logic

module type PARAM =
  sig
    val toks_some : Syntax.t_list
    val toks_none : Syntax.t_list
    val toks_bot : Syntax.t_list
  end

module Make (Param : PARAM) (X : T) =
  struct
    module UnitNone = 
      Unit.Make
	(struct
	  let toks = Param.toks_none
	end)
    module MySum =
      Sum.Make
	(struct
	  let toks_a1 = Param.toks_some
	  let toks_a2 = []
	  let toks_bot = Param.toks_bot
	end)
    module M = MySum(X)(UnitNone)

    include M
  end

