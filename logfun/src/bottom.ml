(** Add a top to a logic *)

open Logic

module type PARAM =
  sig
    val toks_bot : Syntax.t_list (** List of tokens used to represent the bottom. *)
  end

module Make (Param : PARAM) (X : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let x = X.props () in
	  {(no_props "Bottom") with
	   df = x.df;
	   st' = x.st';
	   sg' = x.sg';
	   (*po_entails = x.po_entails;*)
	   cs_entails = x.cs_entails;
	   cp'_entails = x.cp'_entails;
	   cp_top = x.cp_top;
	   cs_bot = isok;
	   defst_conj = x.defst_conj;
	   cs_conj = x.cs_conj;
	   cp_conj = reqand [x.cp_conj; x.sg'; x.cp'_entails];
	   cs_disj = isok;
	   cp_disj = isok;
	   reduced' = reqand [x.df; x.sg'; x.cs_entails; x.cp'_entails];
	   reduced_bot' = reqand [x.df; x.sg'; x.cs_entails; x.cp'_entails];
	 })

    type t = Other of X.t | Bot

    let desc = function
      | Other x -> X.desc x
      | Bot -> false

    let feat = function
      | Other x -> X.feat x
      | Bot -> false

    let compare f g =
      match f, g with
      | Bot, Bot -> 0
      | Bot, Other _ -> 1
      | Other _, Bot -> -1
      | Other x, Other y -> X.compare x y

    let isvalue = function
      | Other x -> X.isvalue x
      | Bot -> false

    let top () = Other (X.top ())

    let bot () = Bot

    let entails f g =
      match f, g with
      | Bot, _ -> true
      | Other x, Other y -> X.entails x y
      | _ -> false

    let conj f g =
      match f, g with
      | Bot, _
      | _, Bot -> Bot
      | Other x, Other y ->
	  if X.desc x && X.feat y && not (X.entails x y) then Bot
	  else if X.desc y && X.feat x && not (X.entails y x) then Bot
	  else Other (X.conj x y)

    let add f g =
      match f, g with
      | Other x, Other y -> Other (X.add x y)
      | Bot, _
      | _, Bot -> Bot

    let sub f g =
      match f, g with
      | Other x, Other y -> Other (X.sub x y)
      | _, _ -> raise Not_found

    let rec features = function
      | Other x -> List.map (fun (vis,x) -> (vis,Other x)) (X.features x)
      | Bot -> LSet.empty ()

    let terms = function
      | Other x -> X.terms x
      | Bot -> []

    let axiom f g_opt =
      match f, g_opt with
      | Other x, None -> X.axiom x None
      | Other x, Some (Other y) -> X.axiom x (Some y)
      | _, _ -> LSet.empty ()

    let parse str =
      if Param.toks_bot = []
      then match str with parser
      | [<x = X.parse>] -> Other x
      | [<>] -> Bot
      else match str with parser
      |	[<_ = Syntax.parse_tokens Param.toks_bot>] -> Bot
      | [<x = X.parse>] -> Other x

    let print = function
      |	Other x -> X.print x
      | Bot -> Param.toks_bot

    let simpl = function
      | Other x -> [<Common.stream_map (fun a -> Other a) (X.simpl x)>]
      | Bot -> [<>]

  end
