(** Segments over values from the sub-logic. *)

(** Signature required for the sub-logic of values. *)
module type LOG = 
  sig
    include Logic.T
    module Diff : Logic.T
    val diff : t -> t -> Diff.t
  end

module Make (A : LOG) =
  struct
    include Logic.Default

(* private definitions *)

    module Pos = Openinterval.DotDot(A)  (* interval for Start and End of segments *)
    module Len = Openinterval.DotDot(A.Diff)  (* interval for length of segments *)

    type bound = NegInf | PosInf | Val of A.t
    type interv = bound * bound  (* invariant: in (a,b) one has (compare a b <= 0) *)
    type t = Is of interv | Contains of interv | In of interv
           | Start of Pos.t | End of Pos.t | Length of Len.t


    let compare_bound a b = match a,b with
    | NegInf, NegInf
    | PosInf, PosInf -> 0
    | Val i, Val j -> A.compare i j
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
	  if A.entails x y then Val x
	  else if A.entails y x then Val y
	  else failwith "Bug in the logic argument of Interv: compare x y = 0 ==> x |= y or y |= x."
      |	_ -> a
	
    let min_of_max a b =
      let res = compare_bound a b in
      if res < 0 then a
      else if res > 0 then b
      else match a, b with
      |	Val x, Val y ->
	  if A.entails x y then Val x
	  else if A.entails y x then Val y
	  else failwith "Bug in the logic argument of Interv: compare x y = 0 ==> x |= y or y |= x."
      |	_ -> a
	
    let max_of_max a b =
      let res = compare_bound a b in
      if res < 0 then b
      else if res > 0 then a
      else match a, b with
      |	Val x, Val y ->
	  if A.entails x y then Val y
	  else if A.entails y x then Val x
	  else failwith "Bug in the logic argument of Interv: compare x y = 0 ==> x |= y or y |= x."
      |	_ -> a
	
    let min_of_min a b =
      let res = compare_bound a b in
      if res < 0 then a
      else if res > 0 then b
      else match a, b with
      |	Val x, Val y ->
	  if A.entails x y then Val y
	  else if A.entails y x then Val x
	  else failwith "Bug in the logic argument of Interv: compare x y = 0 ==> x |= y or y |= x."
      |	_ -> a

    
    let get_pos tok_comp a = match tok_comp, a with
    | Token.GT, NegInf -> Pos.top ()
    | _, NegInf -> Pos.parse (Syntax.of_list [Token.Equal; Token.Ident "min"])
    | Token.LT, PosInf -> Pos.top ()
    | _, PosInf -> Pos.parse (Syntax.of_list [Token.Equal; Token.Ident "max"])
    | _, Val x -> Pos.parse (Syntax.of_list (tok_comp :: A.print x))

    let get_len tok_comp a b = match tok_comp, a, b with
    | Token.LT, NegInf, _ -> Len.top ()
    | Token.LT, _, PosInf -> Len.top ()
    | _, NegInf, _ -> Len.parse (Syntax.of_list [Token.Equal; Token.Ident "max"])
    | _, _, PosInf -> Len.parse (Syntax.of_list [Token.Equal; Token.Ident "max"])
    | _, Val x, Val y -> Len.parse (Syntax.of_list (tok_comp :: A.Diff.print (A.diff y x)))
    | _, _, _ -> failwith "Bug in Segment.get_len: invalid segment (a,b)"

    let get_start = function
    | Is (a,b) -> get_pos Token.Equal a
    | Contains (a,b) -> get_pos Token.LT a
    | In (a,b) -> get_pos Token.GT a
    | Start s -> s
    | End e -> Pos.top ()
    | Length l -> Pos.top ()

    let get_end = function
    | Is (a,b) -> get_pos Token.Equal b
    | Contains (a,b) -> get_pos Token.GT b
    | In (a,b) -> get_pos Token.LT b
    | Start s -> Pos.top ()
    | End e -> e
    | Length l -> Pos.top ()

    let get_length = function
    | Is (a,b) -> get_len Token.Equal a b
    | Contains (a,b) -> get_len Token.GT a b
    | In (a,b) -> get_len Token.LT a b
    | Start s -> Len.top ()
    | End e -> Len.top ()
    | Length l -> l

(* public definitions *)

    let entails_bound a b = match a, b with
    | Val x, Val y -> A.entails x y
    | _ -> true

    let entails_segment (a,b) (c,d) =
      let c1 = compare_bound c a in
      (c1 < 0 or (c1 = 0 & entails_bound a c)) &
      let c2 = compare_bound b d in
      (c2 < 0 or (c2 = 0 & entails_bound b d))

    let rec entails f g = match f, g with
    | Is i, Is j -> entails_segment i j & entails_segment j i
    | Is i, Contains j -> entails_segment j i
    | Is i, In j -> entails_segment i j
    | Is i, Start s -> Pos.entails (get_start f) s
    | Is i, End e -> Pos.entails (get_end f) e
    | Is i, Length l -> Len.entails (get_length f) l
    | Contains i, Contains j -> entails_segment j i
    | In i, In j -> entails_segment i j
    | Start s, Start s' -> Pos.entails s s'
    | End e, End e' -> Pos.entails e e'
    | Length l, Length l' -> Len.entails l l'
    | _, _ -> false

    let conj f g = match f, g with
    | Is _, _ when entails f g -> f
    | Contains (a,b), Contains (c,d) ->
        let e = min_of_min a c in
        let f = max_of_max b d in
        if compare_bound e f <= 0 then Contains (e,f) else raise Not_found
    | In (a,b), In (c,d) ->
        let e = max_of_min a c in
        let f = min_of_max b d in
        if compare_bound e f <= 0 then In (e,f) else raise Not_found
    | Start s, Start s' -> Start (Pos.conj s s')
    | End e, End e' -> End (Pos.conj e e')
    | Length l, Length l' -> Length (Len.conj l l')
    | _, _ -> raise Not_found

    let add f g = conj f g  (* draft version *)

    let sub f g = if entails f g then raise Not_found else f  (* draft version *)

    let features_segment (a,b) =
      LSet.union
        ( match a with
        | Val x ->
            List.fold_left
              (fun res (vis,x') -> LSet.add (vis,In (Val x',PosInf)) res)
              (LSet.empty ()) (A.features x)
        | _ -> LSet.empty ())
        ( match b with
        | Val x ->
            List.fold_left
              (fun res (vis,x') -> LSet.add (vis,In (NegInf,Val x')) res)
              (LSet.empty ()) (A.features x)
        | _ -> LSet.empty ())

    let rec features = function
    | Is i ->
       (*Is i :: *) (* Contains i :: *) (true,In (NegInf,PosInf)) ::
       (* features_segment i @ *)
       features (Start (get_start (Is i))) @
       features (End (get_end (Is i))) @ 
       features (Length (get_length (Is i)))
    | Contains i -> []
    | In i -> [true,In (NegInf,PosInf)]
    | Start s -> List.map (fun (vis,x) -> vis,Start x) (Pos.features s)
    | End e -> List.map (fun (vis,x) -> vis,End x) (Pos.features e)
    | Length l -> List.map (fun (vis,x) -> vis,Length x) (Len.features l)

(* to be implemented
    let gen f g hs =
      let x1, x2 = min_of_min f1 g1, max_of_max f2 g2 in
      if List.exists (fun h -> entails h (x1,x2)) hs
      then LSet.empty ()
      else LSet.of_list [(*(x1,PosInf);(NegInf,x2);*)(x1,x2)]
*)

    open Token

    let parse_bounds sep =
      parser
	| [<a = A.parse; s>] -> begin match s with parser
	  | [<'tok when tok=sep>] -> begin match s with parser
	      [<b = A.parse>] ->
		if A.compare a b <= 0 then (Val a,Val b)
		else raise (Stream.Error "Syntax error: inconsistent interval")
	    | [<>] -> (Val a,PosInf) end
	  | [<>] -> (Val a,Val a) end
	| [<'tok when tok=sep; s>] -> begin match s with parser
	  | [<b = A.parse>] -> (NegInf,Val b)
	  | [<>] -> (NegInf,PosInf) end

    let print_bounds sep = function
      | (NegInf,PosInf) -> [sep]
      | (NegInf,Val b) -> sep :: A.print b
      | (Val a,PosInf) -> A.print a @ [sep]
      | (Val a, Val b) -> A.print a @ sep :: A.print b
      | _ -> failwith "Bug in Segment.print_bounds: invalid interval"

    let parse_segment = parser
      | [<'LeftBra;
          i = parse_bounds Comma ?? "Syntax error after '['";
          'RightBra ?? "Syntax error: missing ']' in segment, after: " ^ Syntax.stringizer (LeftBra::print_bounds Comma i)
         >] -> i

    let print_segment i = LeftBra :: print_bounds Comma i @ [RightBra]

    let parse = parser
      | [<'Ident "is"; i = parse_segment>] -> Is i
      | [<'Ident "contains"; i = parse_segment>] -> Contains i
      | [<'Ident "c"; i = parse_segment>] -> Contains i
      | [<'Ident "in"; i = parse_segment>] -> In i
      | [<'Ident "start"; s = Pos.parse ?? "Syntax error after 'start'">] -> Start s
      | [<'Ident "end"; e = Pos.parse ?? "Syntax error after 'end'">] -> End e
      | [<'Ident s when s.[0] = 'l'; l = Len.parse ?? "Syntax error after '" ^ s ^ "'">] -> Length l

    let print = function
      | Is i -> Ident "is" :: PP_tilda :: print_segment i
      | Contains i -> Ident "contains" :: PP_tilda :: print_segment i
      | In i -> Ident "in" :: PP_tilda :: print_segment i
      | Start s -> Ident "start" :: PP_tilda :: Pos.print s
      | End e -> Ident "end" :: PP_tilda :: Pos.print e
      | Length l -> Ident "length" :: PP_tilda :: Len.print l

  end
