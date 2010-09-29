(** Integer numbers. *)

open Logic

module Make =
  struct
    include Default

    let props () =
      {(no_props "Int") with
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
       total_le = isok;
       po_le_min = isok;
       cs_le_min = isok;
       cp_le_min = isok;
       po_le_max = isok;
       cs_le_max = isok;
       cp_le_max = isok;
       cs_le_overlap = isok;
       cp_le_overlap = isok;
       reduced_right = isok;
       reduced_bot = isok;
       reduced_bot' = isok;
       reduced_top = isok;
     }

    type t = int

    let isvalue _ = true

    let parse = parser
      |	[<y = Syntax.parse_int>] -> y

    let rec print = function
      |	y -> Syntax.toks_of_int y

    let entails y1 y2 = y1=y2

(*    let share y1 y2 = y1=y2 *)

    let le_min = (<=)

    let le_max = (<=)

    let le_overlap x y = x <= y + 1

  end 
