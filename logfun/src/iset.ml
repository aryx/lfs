
open Logic

module Make (E : T) =
  struct
    module M = Id(E)
    include M

    let props =
      fixpoint
	(fun () ->
	  let e = E.props () in
	  {e with
	   cs_conj = isok;
	   cp_conj = isok;
	   reduced = e.reduced_right;
	   reduced' = e.reduced_right;
	 })

    let conj f g = raise Not_found

  end
