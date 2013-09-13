(** Intervals over values from the sub-logic. *)

open Logic

module type PARAM =
  sig
    val verbose : bool
	(** When set to false the syntax of intervals is simply 'a..b'.
	   Otherwise it can be 'in a..b', '= a', '> a', '< a'. *)
  end

module Make (Param : PARAM) (Val : T) =
  struct
    include Default

    let props =
      fixpoint
	(fun () ->
	  let v = Val.props () in
	  {(no_props "Interv") with
	   st' = reqand [v.st'; v.cs_le_max];
	   cs_entails = reqand [v.cs_le_min; v.cs_le_max];
	   cp_entails = reqand [v.cp_le_min; v.cp_le_max];
	   cp'_entails = reqand [v.cp_le_min; v.cp_le_max];
	   cp_top = isok;
	   cs_bot = isok;
	 })

    type bound = NegInf | PosInf | Val of Val.t

    let compare_bound x y =
      match x, y with
      | NegInf, NegInf -> 0
      | NegInf, _ -> -1
      | _, NegInf -> 1
      | PosInf, PosInf -> 0
      | _, PosInf -> -1
      | PosInf, _ -> 1
      | Val a, Val b -> Val.compare a b


    let bound_le_min a b =
      match a, b with
      | _, PosInf -> true
      | PosInf, _ -> false
      | NegInf, _ -> true
      | _, NegInf -> false
      | Val x, Val y -> Val.le_min x y

    let bound_le_max a b =
      match a, b with
      | _, PosInf -> true
      | PosInf, _ -> false
      | NegInf, _ -> true
      | _, NegInf -> false
      | Val x, Val y -> Val.le_max x y

    let max_of_min a b =
      if bound_le_min a b then b else a

    let min_of_max a b =
      if bound_le_max a b then a else b

    let max_of_max a b =
      if bound_le_max a b then b else a

    let min_of_min a b =
      if bound_le_min a b then a else b

(*
    let compare_bound a b =
      match a,b with
      | NegInf, NegInf
      | PosInf, PosInf -> 0
      | Val i, Val j -> Val.compare i j
      | a, b ->
	  if a=NegInf or b=PosInf
	  then -1
	  else 1

    let max_of_min a b =
      let res = compare_bound a b in
      if res < 0 then b
      else if res > 0 then a
      else match a, b with
      |	Val x, Val y ->
	  if Val.entails x y then Val x
	  else if Val.entails y x then Val y
	  else failwith "Bug in the logic argument of Interv: compare x y = 0 ==> x |= y or y |= x."
      |	_ -> a

    let min_of_max a b =
      let res = compare_bound a b in
      if res < 0 then a
      else if res > 0 then b
      else match a, b with
      |	Val x, Val y ->
	  if Val.entails x y then Val x
	  else if Val.entails y x then Val y
	  else failwith "Bug in the logic argument of Interv: compare x y = 0 ==> x |= y or y |= x."
      |	_ -> a

    let max_of_max a b =
      let res = compare_bound a b in
      if res < 0 then b
      else if res > 0 then a
      else match a, b with
      |	Val x, Val y ->
	  if Val.entails x y then Val y
	  else if Val.entails y x then Val x
	  else failwith "Bug in the logic argument of Interv: compare x y = 0 ==> x |= y or y |= x."
      |	_ -> a

    let min_of_min a b =
      let res = compare_bound a b in
      if res < 0 then a
      else if res > 0 then b
      else match a, b with
      |	Val x, Val y ->
	  if Val.entails x y then Val y
	  else if Val.entails y x then Val x
	  else failwith "Bug in the logic argument of Interv: compare x y = 0 ==> x |= y or y |= x."
      |	_ -> a
*)

    type t = bound * bound  (* invariant: in (a,b) one has (compare_bound a b <= 0) *)

    let compare f g =
      Common.compare_pair (compare_bound, compare_bound) f g

    let isvalue = function
      | Val x, Val y -> Val.isvalue x & x=y
      | _ -> false

    let top () = (NegInf,PosInf)

(*
    let entails_bound a b = match a, b with
    | Val x, Val y -> Val.entails x y
    | _ -> true

    let entails (a,b) (c,d) =
      let c1 = compare_bound c a in
      (c1 < 0 or (c1 = 0 & entails_bound a c)) &
      let c2 = compare_bound b d in
      (c2 < 0 or (c2 = 0 & entails_bound b d))
*)

    let entails f g =
      match f, g with
      | _, (NegInf,PosInf) -> true
      | (Val a1, Val b1), (Val a2, Val b2) -> Val.le_min a2 a1 & Val.le_max b1 b2
      | (Val a1, _), (Val a2, PosInf) -> Val.le_min a2 a1
      | (_, Val b1), (NegInf, Val b2) -> Val.le_max b1 b2
      | _, _ -> false

(*
    let conj (a,b) (c,d) =
      let e = max_of_min a c in
      let f = min_of_max b d in
      if compare_bound e f <= 0 then (e,f) else raise Not_found
*)

    let features (a,b) =
      (true,(NegInf,PosInf)) ::
      match (a,b) with
      | Val x, Val y when x=y -> List.map (fun (vis,x) -> vis,(Val x,Val x)) (Val.features x)
      | _ -> [true,(a,b)]

    let gen (f1,f2) (g1,g2) hs =
      let x1, x2 = min_of_min f1 g1, max_of_max f2 g2 in
      if List.exists (fun h -> entails h (x1,x2)) hs
      then LSet.empty ()
      else LSet.of_list [(*(x1,PosInf);(NegInf,x2);*)(x1,x2)]

    open Token

    let parse_interval =
      parser
	| [<a = Val.parse; s>] -> begin match s with parser
	  | [<'Dot; 'Dot>] -> begin match s with parser
	    | [<b = Val.parse>] ->
		if Val.compare a b <= 0 then (Val a,Val b)
		else raise (Stream.Error "Syntax error: inconsistent interval")
	    | [<>] -> (Val a,PosInf) end
	  | [<>] -> (Val a,Val a) end
	| [<'Dot; 'Dot; s>] -> begin match s with parser
	  | [<b = Val.parse>] -> (NegInf,Val b)
	  | [<>] -> (NegInf,PosInf) end
	| [<'Ident "min">] -> (NegInf,NegInf)
	| [<'Ident "max">] -> (PosInf,PosInf)

    let parse_verbose = parser
      |	[<'Equal; str>] -> (match str with parser
           | [<x = Val.parse>] -> (Val x,Val x)
           | [<'Ident "max">] -> (PosInf,PosInf)
           | [<'Ident "min">] -> (NegInf,NegInf)
           | [<>] -> raise (Stream.Error "Syntax error: value expected after '='"))
      |	[<'LT; x = Val.parse ?? "Syntax error: value expected after '<'">] -> (NegInf,Val x)
      |	[<'GT; x = Val.parse ?? "Syntax error: value expected after '>'">] -> (Val x, PosInf)
      |	[<'Ident "in"; a,b = parse_interval ?? "Syntax error: value interval expected after 'in'">] -> (a,b)

    let parse str =
      if Param.verbose
      then parse_verbose str
      else parse_interval str

    let print_interval = function
      | (NegInf,PosInf) -> [Dot; Dot]
      | (NegInf,NegInf) -> [Ident "min"]
      | (PosInf,PosInf) -> [Ident "max"]
      | (NegInf,Val b) -> Dot :: Dot :: PP_tilda :: Val.print b
      | (Val a,PosInf) -> Val.print a @ [PP_tilda; Dot; Dot]
      | (Val a,Val b) ->
	  if a = b then Val.print a
	  else Val.print a @ [PP_tilda; Dot; Dot; PP_tilda] @ Val.print b
      | _ -> failwith "Bug in Interval.print: invalid interval"

    let print_verbose = function
      | (NegInf,PosInf) -> [PP_tilda; Ident "in"; PP_tilda; Dot; Dot]
      | (NegInf,NegInf) -> [PP_tilda; Equal; PP_tilda; Ident "min"]
      | (PosInf,PosInf) -> [PP_tilda; Equal; PP_tilda; Ident "max"]
      | (NegInf,Val b) -> PP_tilda :: LT :: PP_tilda :: Val.print b
      | (Val a,PosInf) -> PP_tilda :: GT :: PP_tilda :: Val.print a
      | (Val a,Val b) ->
	  if a = b then PP_tilda :: Equal :: PP_tilda :: Val.print a
	  else PP_tilda :: Ident "in" :: PP_tilda :: Val.print a @ [PP_tilda; Dot; Dot; PP_tilda] @ Val.print b
      | _ -> failwith "Bug in Interval.print: invalid interval"

    let print f =
      if Param.verbose
      then print_verbose f
      else print_interval f

    let simpl = function
      | (NegInf,Val b) ->
          [<'(Val b,Val b); Common.stream_map (fun b' -> (NegInf,Val b')) (Val.simpl b)>]
      | (Val a,PosInf) ->
          [<'(Val a,Val a); Common.stream_map (fun a' -> (Val a',PosInf)) (Val.simpl a)>]
      | (Val a,Val b) ->
          if a=b
          then
            Common.stream_map (fun a' -> (Val a',Val a')) (Val.simpl a)
          else
            [<'(Val a,Val a); '(Val b,Val b);
              '(Val a,PosInf); '(NegInf,Val b);
              Common.stream_map (fun a' -> (Val a',Val b)) (Val.simpl a);
              Common.stream_map (fun b' -> (Val a,Val b')) (Val.simpl b)>]
      | _ -> [<>]

  end
