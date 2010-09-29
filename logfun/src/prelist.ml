open Token
open Logic

module type PARAM =
  sig
    val toks_top : Syntax.t_list
    val toks_nil : Syntax.t_list
    val toks_sep : Syntax.t_list
    val toks_bot : Syntax.t_list
    val feat_top : bool
  end

module Make (Param : PARAM) (X : T) =
  struct
    module MyOption =
      Option.Make
	(struct
	  let toks_none = Param.toks_nil
	  let toks_some = []
	  let toks_bot = Param.toks_bot
	end)
    module MyProd =
      Prod.Make
	(struct
	  let toks_pre = []
	  let toks_sep = Param.toks_sep
	  let toks_suf = []
	  let toks_bot = Param.toks_bot
	end)
    module MyTop =
      Top.Make
	(struct
	  let toks_top = Param.toks_top
	  let feat_top = false
	end)

    module MyTopOption (X : T) =
      struct
	module O = MyOption(X)
	module T = MyTop(O)

	include T

	let features = function
	  | T.Other (O.A2 _) as f -> features f  (* O.A2 = Option.None *)
	  | f -> if Param.feat_top then (true,T.Top)::features f else features f
      end


    module rec Prelist : T = MyTopOption (MyProd(X)(Prelist))

    include Prelist

  end
