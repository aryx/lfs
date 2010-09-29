
open Logic

module type PARAM =
  sig
    val toks : Syntax.t_list
  end

module Make (Param : PARAM) (L : T) =
  struct
    module M = Id(L)
    include M

    let parse = parser
      | [<f = Syntax.parse_tokens_and Param.toks parse>] -> f

    let print f =
      Param.toks @ print f

  end
