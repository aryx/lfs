(** Conjunctions of formulas from the sub-logic. *)

module type PARAM =
  sig
    val top : bool  (** Set to [true] if this functor is embedded in another 'Conj' occurence. *)
    val sep : Token.t option  (** Token to be used as the conjunction connector (optional). *)
    val lgg : bool  (** Set to [true] if "lggs" is the required implementation of [gen]. *)
  end

module Make (Param : PARAM) (E : Logic.T) =
  struct
    include Logic.Default

    type t = E.t list

    let insert_elt : E.t -> t -> t =
      fun e d ->
        if List.exists (fun e' -> E.entails e' e) d
        then d
        else LSet.add e (List.filter (fun e' -> not (E.entails e e')) d)

    let insert_elt_r : E.t list -> t -> t =
      fun es d ->
        List.fold_left (fun res e -> insert_elt e res) d es

    let rec conj_elt : E.t -> t -> t =
      fun e d -> conj_elt' [] e d
    and conj_elt' d e = function
	[] -> LSet.add e d
      |	e'::d' ->
	  try let e'' = E.conj e e' in conj_elt e'' (d' @ d)
	  with Not_found -> conj_elt' (LSet.add e' d) e d'

    let conj_elt_r : E.t list -> t -> t =
      fun es d ->
        List.fold_left (fun res e -> conj_elt e res) d es

    let rec add_elt : E.t -> t -> t =
      fun s d ->
	let added, d' = List.fold_left
	    (fun (added,d') x ->
	      try (true, conj_elt (E.add x s) d')
	      with Not_found -> (added,conj_elt x d')) (false,[]) d in
	if added then d' else LSet.add s d'

    let rec sub_elt : E.t -> t -> t =
      fun s d -> List.fold_left
	  (fun d' x ->
	    try conj_elt (E.sub x s) d'
	    with Not_found -> d') [] d

(* public functions *)

    let rec parse = parser
      |	[<'Token.Star>] -> []
      |	[<e = E.parse; f = (match Param.sep with None -> parse_suite0 | Some tok -> parse_suite1 tok)>] ->
	  List.rev (conj_elt e f)
    and parse_suite0 = parser
	[<e = E.parse; f = parse_suite0>] -> conj_elt e f
      | [<>] -> []
    and parse_suite1 tok = parser
	[<'t when t=tok;
          e = E.parse ?? "Syntax error: element expected in conjunction, after: " ^ Syntax.stringizer [tok];
          f = parse_suite1 tok
         >] -> conj_elt e f
      | [<>] -> []

    let rec print = function
	[] -> [Token.Star]
      |	e::f ->
          E.print e @
          ( match Param.sep with
          | None -> print_suite0 f
          | Some tok -> print_suite1 (if Param.top then [Token.PP_tilda; tok; Token.PP_tilda] else [Token.PP_tilda; tok; Token.PP_tilda]) f)
    and print_suite0 = function
	[] -> []
      |	e::f -> Token.PP_tilda::(E.print e @ print_suite0 f)
    and print_suite1 sep = function
	[] -> []
      |	e::f -> sep @ E.print e @ print_suite1 sep f

    let rec simpl f =
      [<simpl2 [] f; simpl3 [] f>]
    and simpl2 acc = function
      | [] -> [<>]
      | e::f -> [<'(acc@f); simpl2 (acc@[e]) f>]
    and simpl3 acc = function
      | [] -> [<>]
      | e::f -> [<Common.stream_map (fun e' -> acc@(e'::f)) (E.simpl e); simpl3 (acc@[e]) f>]

    let top () = []

    let bot () = [E.bot ()]

    let rec entails f g =
      List.for_all (fun e2 ->
        List.exists (fun e1 ->
          E.entails e1 e2
        ) f
      ) g

    let conj f g = List.fold_left (fun h e2 -> conj_elt e2 h) f g

    let add f g = conj f g

    let sub f g =
      List.fold_left (fun d' s -> sub_elt s d') f g

    let rec features f = features2 f
    and features2 =
      function
      | [] -> []
      |	e::f -> List.map (fun (vis,e') -> vis,[e']) (E.features e) @ features2 f

    let axiom f g_opt =
      match f, g_opt with
      | [ef], None -> E.axiom ef None
      | [ef], Some [eg] -> E.axiom ef (Some eg)
      | _, _ -> LSet.empty () (* to be completed *)

    let rec gen_spec f g hs =
      if Param.top then gen2 f g hs else gen2 f g hs
    and gen2 f g hs =
      let zs =
        List.fold_left (fun res h ->
          insert_elt_r h res
        ) (LSet.empty ()) hs in
      let xs1 =
        List.fold_left (fun res x ->
          List.fold_left (fun res' y ->
            let ts = Logic.satgen E.entails E.gen x y (lazy zs) (List.filter (fun z -> E.entails x z & E.entails y z) zs) in
            List.fold_left (fun res'' t -> LSet.add [t] res'') res' ts
          ) res g
        ) (LSet.empty ()) f in
      if xs1 <> [] or Param.top
      then xs1
      else (* conjunctions of upper bounds *)
        match hs with
        | [] -> []
        | h::[] -> []
        | h1::h2::hs ->
            let l1, l2 = List.length h1, List.length h2 in
            let size, smallest_hs = (* selection of smallest upper bounds among hs, but at least 2 *)
              List.fold_left (fun (size,shs) h ->
                let l = List.length h in
                if l < size then (l,(l,h)::List.filter (fun (l',sh) -> l' = size) shs)
                else if l = size then (size,(l,h)::List.filter (fun (l',sh) -> l' = size) shs)
                else (size,shs)
              ) (min l1 l2, if l1 <= l2 then [(l1,h1);(l2,h2)] else [(l2,h2);(l1,h1)]) hs in
            let _, xs2 = (* smallest conjunctions of smallest upper bounds *)
              Common.fold_pair
                (fun (_,h1) (_,h2) (s,xs) ->
                   let x = conj h1 h2 in
                   let l = List.length x in
                   if l < s then (l, LSet.singleton x)
                   else if l = s then (s, LSet.add x xs)
                   else (s, xs)
                ) smallest_hs (2*size, LSet.empty ()) in
            xs2

    let gen_gen f g hs =
      (* assumes that E.gen returns lggs *)
      let zs' =
        List.fold_left (fun res x ->
          List.fold_left (fun res' y ->
            insert_elt_r (E.gen x y []) res'
          ) res g
        ) (LSet.empty ()) f in
      if List.exists (fun h -> entails h zs') hs
      then []
      else [zs']

    let gen =
      if Param.lgg
      then gen_gen
      else gen_spec

  end

