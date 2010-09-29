(** The minimal logic. *)

open Logic

module type PARAM =
  sig
    val toks : Syntax.t_list
  end

module Make (Param : PARAM) =
  struct
    include Default

    let props () =
      {(no_props "Unit") with
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
       reduced = isok;
       reduced_right = isok;
       reduced_bot = isok;
       reduced_bot' = isok;
       reduced_top = isok;
       reduced' = isok;
     }

    type t = unit

    let isvalue () = true

    let parse = Syntax.parse_tokens Param.toks

    let print () = Param.toks

    let top () = ()

    let entails () () = true

    let features () = LSet.singleton (true,())

  end

