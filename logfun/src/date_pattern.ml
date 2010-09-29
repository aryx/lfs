
open Token
open Logic

module type PARAM =
  sig
    val toks_any : Syntax.t_list
    val toks_sep_interval : Syntax.t_list
    val toks_sep_dmy : Syntax.t_list
  end

module Make (Param : PARAM) =
  struct

    module MyAtom =
      Atom.Make
	(struct
	  let names _ = true
	end)

    module MyInt = Int.Make

    module MyInterval =
      Openinterval.Make
	(struct
	  let toks_bound = []
	  let toks_equal = []
	  let toks_pre = []
	  let toks_sep = Param.toks_sep_interval
	  let toks_suf = []
	end)

    module MyEnum =
      Enum.Make
	(struct
	  let toks_sep = [Comma]
	  let toks_begin = [LeftAcc]
	  let toks_end = [RightAcc]
	  let toks_empty = [LeftAcc; RightAcc]
	  let no_begin_end_when_singleton = true
	end)

    module MyProd =
      Prod.Make
	(struct
	  let toks_pre = []
	  let toks_sep = Param.toks_sep_dmy
	  let toks_suf = []
	  let toks_bot = [Sharp]
	end)

    module MyTop =
      Top.Make
	(struct
	  let toks_top = Param.toks_any
	  let feat_top = true
	end)

    module Day = MyTop (MyEnum (MyInterval(MyInt)))
    module Month = MyTop (MyEnum(MyAtom))
    module Year = MyTop (MyEnum (MyInterval(MyInt)))

    module Date = MyProd(Day)(MyProd(Month)(Year))

    include Date
  end

module ForYoann =
  Make
    (struct
      let toks_any = [Star]
      let toks_sep_interval = [Dot; Dot]
      let toks_sep_dmy = [Minus; Minus]
    end)
