(** Add a top to a logic *)

open Logic

module type PARAM =
  sig
    val toks_top : Syntax.t_list (** List of tokens used to represent the top. *)
    val feat_top : bool
  end

module Make (Param : PARAM) (X : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let x = X.props () in
	  {(no_props "Top") with
	   df = x.df;
	   st = x.st;
	   st' = x.st';
	   sg' = x.sg';
	   po_entails = x.po_entails;
	   cs_entails = reqand [x.cs_entails; x.cp_top];
	   cp_entails = reqand [x.cp_entails; x.reduced_top];
	   cp'_entails = x.cp'_entails;
	   cp_top = isok;
	   cs_bot = x.cs_bot;
	   defst_conj = x.defst_conj;
	   cs_conj = x.cs_conj;
	   cp_conj = x.cp_conj;
	   cs_disj = x.cs_disj;
	   cp_disj = x.cp_disj;
	   total_le = x.total_le;
	   cs_le_min = reqand [x.cs_le_min; x.cs_entails; x.cp_top];
	   cp_le_min = reqand [x.cp_le_min; x.reduced_top];
	   cs_le_max = reqand [x.cs_le_max; x.cs_entails; x.cp_top];
	   cp_le_max = reqand [x.cp_le_max; x.reduced_top];
	   cs_le_overlap = x.cs_le_overlap;
	   cp_le_overlap = x.cp_le_overlap;
	   reduced = x.reduced;
	   reduced' = x.reduced';
	   reduced_top = x.reduced_top;
	   reduced_bot = x.reduced_bot;
	   reduced_bot' = x.reduced_bot';
	   reduced_right = x.reduced_right;
	 })

    type t = Top | Other of X.t

    let desc = function
      | Top -> false
      | Other x -> X.desc x

    let feat = function
      | Top -> true
      | Other x -> X.feat x

    let compare f g =
      match f, g with
      | Top, Top -> 0
      | Top, Other _ -> -1
      | Other _, Top -> 1
      | Other x, Other y -> X.compare x y

    let isvalue = function
      | Top -> false
      | Other x -> X.isvalue x

    let top () = Top

    let bot () = Other (X.bot ())

    let entails f g =
      match f, g with
      | _, Top -> true
      | Top, Other y -> (try X.entails (X.top ()) y with _ -> false)
      | Other x, Other y -> X.entails x y

    let conj f g =
      match f, g with
      | Top, _ -> g
      | _, Top -> f
      | Other x, Other y -> Other (X.conj x y)

    let disj f g =
      match f, g with
      | Top, _
      | _, Top -> [Top]
      | Other x, Other y -> List.map (fun z -> Other z) (X.disj x y)

    let le_min f g =
      match f, g with
      | Top, _ -> true
      | Other x, Top -> (try X.entails (X.top ()) x with Not_found -> false)
      | Other x, Other y -> (try X.le_min x y with Not_found -> false)

    let le_max f g =
      match f, g with
      | _, Top -> true
      | Top, Other y -> (try X.entails (X.top ()) y with Not_found -> false)
      | Other x, Other y -> (try X.le_max x y with Not_found -> false)

    let le_overlap f g =
      match f, g with
      | Top, _
      | _, Top -> true
      | Other x, Other y -> try X.le_overlap x y with Not_found -> false

    let add f g =
      match f, g with
      | Top, _ -> g
      | _, Top -> f
      | Other x, Other y -> Other (X.add x y)

    let sub f g =
      match f, g with
      | Top, _ -> Top
      | Other x, Other y -> Other (X.sub x y)
      | _, _ -> raise Not_found

    let rec features = function
      | Top -> if Param.feat_top then [(true,Top)] else []
      | Other x -> LSet.union (features Top) (List.map (fun (vis,x) -> (vis,Other x)) (X.features x))

    let terms = function
      | Top -> []
      | Other x -> X.terms x

    let axiom f g_opt =
      match f, g_opt with
      | Other x, None -> X.axiom x None
      | Other x, Some (Top) -> X.axiom x None
      | Other x, Some (Other y) -> X.axiom x (Some y)
      | _, _ -> LSet.empty ()

    let parse str =
      if Param.toks_top = []
      then match str with parser
      | [<x = X.parse>] -> Other x
      | [<>] -> Top
      else match str with parser
      |	[<_ = Syntax.parse_tokens Param.toks_top>] -> Top
      | [<x = X.parse>] -> Other x

    let print = function
      | Top -> Param.toks_top
      |	Other x -> X.print x

    let simpl = function
      | Top -> [<>]
      | Other x -> [<'Top; Common.stream_map (fun a -> Other a) (X.simpl x)>]

  end
