(** Ensures property df by using identifiers. *)

open Logic

module type PARAM =
  sig
    val toks_any : Syntax.t_list
    val toks_this : Syntax.t_list
  end

module Make (Param : PARAM) (L : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let l = L.props () in
	  {(no_props "Id") with
	   df = isok;
	   st = l.st;
	   st' = l.st';
	   sg' = l.sg';
	   cs_entails = l.cs_entails;
	   cp'_entails = l.cp'_entails;
	   cp_top = l.cp_top;
	   cs_bot = l.cs_bot;
	   cs_conj = isok;
	   cp_conj = isok;
           cs_disj = isok;
	   cp_disj = isok;
	 })

    type t = Any of L.t | This of int * L.t

    let desc = function
      | This (n,d) -> L.desc d
      | _ -> false

    let feat = function
      | Any x -> L.feat x
      | This (n,d) -> L.desc d

    let isvalue = function
      | This _ -> true
      | _ -> false

    let cpt = ref 0 (* counter for producing unique descriptors *)
    let parse = parser
      | [<x = Syntax.parse_tokens_and Param.toks_any L.parse>] -> Any x
      | [<d = Syntax.parse_tokens_and Param.toks_this L.parse>] -> incr cpt; This (!cpt, d)

    let print = function
      | Any x -> Param.toks_any @ L.print x
      | This (n,d) -> Param.toks_this @ L.print d

    let top () = Any (L.top ())

    let bot () = Any (L.bot ())

    let entails f g =
      match f, g with
      | Any x, Any y -> L.entails x y
      | This (n,x), Any y -> L.entails x y
      | This (n,x), This (m,y) -> n = m
      | _ -> false

  end

