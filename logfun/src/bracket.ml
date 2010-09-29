
open Logic

module type PARAM =
  sig
    val toks_left : Syntax.t_list
    val toks_right : Syntax.t_list
  end

module Make (Param : PARAM) (L : T) =
  struct
    module M = Id(L)
    include M

    let parse = parser
      | [<f = Syntax.parse_tokens_and Param.toks_left L.parse; _ = Syntax.parse_tokens Param.toks_right>] -> f

    let print f =
      Param.toks_left @ L.print f @ Param.toks_right

  end
