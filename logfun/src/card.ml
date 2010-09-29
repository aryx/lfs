(** Cardinals. *)

open Token
open Logic

module Make =
  struct
    include Default

    let props () =
      {(no_props "Card") with
       df = isok;
       st = isok;
       st' = isok;
       po_entails = isok;
       cs_entails = isok;
       cp_entails = isok;
       cp'_entails = isok;
       cp_top = isok;
       cs_bot = isok;
       defst_conj = isok;
       cs_conj = isok;
       cp_conj = isok;
       cs_le_min = isok;
       cp_le_min = isok;
       cs_le_max = isok;
       cp_le_max = isok;
       reduced = isok;
       reduced' = isok;
       reduced_top = isok;
       reduced_bot = isok;
       reduced_right = isok;
     }

    type t = int

    let isvalue _ = false

    let parse = parser
      |	[<'Nat y>] -> y

    let rec print = function
      |	y -> [Nat y]

    let entails y1 y2 = y1 >= y2

    let top () = 0

    let conj x y = max x y

    let disj x y = [min x y]

  end 
