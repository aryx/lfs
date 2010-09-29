(** Float numbers. *)

open Logic

module Make =
  struct
    include Default

    let props () =
      {(no_props "Float") with
       df = isok;
       st = isok;
       st' = isok;
       sg' = isok;
       po_entails = isok;
       cs_entails = isok;
       cp_entails = isok;
       cp'_entails = isok;
       cp_top = isok;
       cs_bot = isok;
       cs_conj = isok;
       cp_conj = isok;
       cs_disj = isok;
       cp_disj = isok;
       po_le_min = isok;
       cs_le_min = isok;
       cp_le_min = isok;
       po_le_max = isok;
       cs_le_max = isok;
       cp_le_max = isok;
       reduced_right = isok;
       reduced_bot = isok;
     }

    type t = float

    let isvalue _ = true

    let entails y1 y2 = y1 = y2

    let le_min = (<=)

    let le_max = (<=)

    open Token

    let parse = parser
      |	[<(f,_) = Syntax.parse_float>] -> f

    let rec print = function
      |	f -> Syntax.toks_of_float (f, Token.prec_of_sfloat (string_of_float f))

  end
