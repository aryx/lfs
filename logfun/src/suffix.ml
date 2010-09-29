

module type PARAM =
  sig
    val toks : Syntax.t_list
  end

module Make (Param : PARAM) (L : Logic.T) : Logic.T =
  struct
    include L

    let parse = parser
      | [<f = L.parse; _ = Syntax.parse_tokens Param.toks>] -> f

    let print f =
      L.print f @ Param.toks

  end
