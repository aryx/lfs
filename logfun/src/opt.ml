(** Make optional the use of a sub-logic formula. *)

open Logic

module type PARAM =
  sig
    val toks_none : Syntax.t_list (** List of tokens used to represent the 'void' case. *)
    val toks_some : Syntax.t_list
  end

module Make (Param : PARAM) (A : T) =
  struct
    include Default

    (* Opt is equivalent to Sum_1 *)
    let props =
      fixpoint
	(fun () ->
	  let propsA = A.props () in
	  {(no_props "Opt") with
	   st' = propsA.st';
	   cs_entails = propsA.cs_entails;
	   cp'_entails = propsA.cp'_entails;
	   cp_top = isok})

    type t = A.t option

    let desc = function
      | None -> false
      | Some x -> A.desc x

    let feat = function
      | None -> true
      | Some x -> A.feat x

    let compare f g =
      match f, g with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some x, Some y -> A.compare x y

    let isvalue = function
      | None -> false
      | Some x -> A.isvalue x

    let top () = None

    let bot () = Some (A.bot ())

    let entails f g = match f, g with
      _, None -> true
    | None, Some _ -> false
    | Some x, Some y -> A.entails x y

    let share f g =
      match f, g with
      | None, _
      | _, None -> true
      | Some x, Some y -> A.share x y

    let conj f g = match f, g with
      f, None -> f
    | None, g -> g
    | Some x, Some y -> Some (A.conj x y)

    let add f g = match f, g with
    | f, None -> f
    | None, g -> g
    | Some x, Some y -> Some (A.add x y)


    let sub f g = match (f,g) with
      None, Some _ -> None
    | Some x, Some y -> Some (A.sub x y)
    | _, _ -> raise Not_found

    let features = function
	None -> [true,None]
      |	Some x -> (true,None)::List.map (fun (vis,x) -> vis,Some x) (A.features x)

    let axiom f g_opt =
      match f, g_opt with
      | Some x, None -> A.axiom x None
      | Some x, Some (None) -> A.axiom x None
      | Some x, Some (Some y) -> A.axiom x (Some y)
      | _, _ -> LSet.empty ()

    let gen f g hs =
    if false & hs = [] then [None]
    else match (f,g) with
    | Some x, Some y ->
         let zs = Common.mapfilter (function x -> x) hs in
         List.fold_left (fun res x -> LSet.add (Some x) res) (LSet.empty ()) (A.gen x y zs)
    | _, _ -> [None]

    let parse str =
      if Param.toks_none = []
      then match str with parser
      | [<x = Syntax.parse_tokens_and Param.toks_some A.parse>] -> Some x
      | [<>] -> None
      else match str with parser
      |	[<_ = Syntax.parse_tokens Param.toks_none>] -> None
      | [<x = Syntax.parse_tokens_and Param.toks_some A.parse>] -> Some x

    let print = function
      | None -> Param.toks_none
      |	Some x -> Param.toks_some @ A.print x

    let simpl = function
      | None -> [<>]
      | Some x -> [<'None; Common.stream_map (fun a -> Some a) (A.simpl x)>]

  end
