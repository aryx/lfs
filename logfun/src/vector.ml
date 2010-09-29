open Token
open Logic

module type PARAM =
  sig
    val toks_sep : Token.t list
    val toks_begin : Token.t list
    val toks_end : Token.t list
    val toks_empty : Token.t list
    val toks_bot : Token.t list
  end

module Make (Param : PARAM) (X : T) =
  struct
    module MyOption =
      Option.Make
	(struct
	  let toks_none = Param.toks_end
	  let toks_some = Param.toks_sep
	  let toks_bot = Param.toks_bot
	end)
    module MyProd =
      Prod.Make
	(struct
	  let toks_pre = []
	  let toks_sep = []
	  let toks_suf = []
	  let toks_bot = Param.toks_bot
	end)
    module rec Tuple : T = MyOption(MyProd(X)(Tuple))

    include Tuple

    let rec parse = parser
      | [<_ = Syntax.parse_tokens Param.toks_begin; f = parse2>] -> f
      | [<_ = Syntax.parse_tokens Param.toks_empty>] -> Tuple.parse (Syntax.of_list Param.toks_end)
      | [<_ = Syntax.parse_tokens Param.toks_bot>] -> Tuple.parse (Syntax.of_list Param.toks_bot)
    and parse2 str =
      if Param.toks_end = []
      then match str with parser
      | [<x = X.parse; xs = Tuple.parse>] -> Tuple.parse (Syntax.of_list (Param.toks_sep @ X.print x @ Tuple.print xs))
      | [<_ = Syntax.parse_tokens Param.toks_end>] -> Tuple.parse (Syntax.of_list Param.toks_end)
      else match str with parser
      | [<_ = Syntax.parse_tokens Param.toks_end>] -> Tuple.parse (Syntax.of_list Param.toks_end)
      | [<x = X.parse; xs = Tuple.parse>] -> Tuple.parse (Syntax.of_list (Param.toks_sep @ X.print x @ Tuple.print xs))

    let print f =
      match Syntax.of_list (Tuple.print f) with parser
      | [<_ = Syntax.parse_tokens Param.toks_end>] -> Param.toks_empty
      | [<_ = Syntax.parse_tokens Param.toks_sep; str>] -> Param.toks_begin @ Syntax.list_of_stream str
      | [<_ = Syntax.parse_tokens Param.toks_bot>] -> Param.toks_bot

  end
