(** The secondary structures of proteins: alpha-helices, beta-sheets, coiled-coils.

   Each secondary structure is represented by the type of the structure ('a','b','c'),
   and a length interval.

*)

type len = int * int (** The type of secondary structure length interval. *)
type ss = Alpha | Beta | Coil (** The type secondary structure types. *)

module type PARAM =
  sig
    val gen_cond : ss -> len -> bool (** Filter on the secondary structures. Used to control the operation [gen]. *)
  end

module Make (Param : PARAM) =
  struct
    include Logic.Default

    type t = ss * len

    open Token

    let parse_len ss = parser
      | [<'LeftPar;
          'Nat a ?? "Syntax error: integer expected in secondary structure element, after: " ^ Syntax.stringizer [Ident ss; LeftPar];
          str
         >] -> (match str with parser
            | [<'Comma;
                'Nat b ?? "Syntax error: integer expected in secondary structure element, after: " ^ Syntax.stringizer [Ident ss; LeftPar; Nat a; Comma];
                'RightPar ?? "Syntax error: missing ')', after: " ^ Syntax.stringizer [Ident ss; LeftPar; Nat a; Comma; Nat b]
               >] ->
                if a = 0 or a > b
                then raise (Stream.Error ("Syntax error: invalid interval in secondary structure element: " ^ Syntax.stringizer [Ident ss; LeftPar; Nat a; Comma; Nat b; RightPar]))
                else (a,b)
	    | [<'RightPar>] -> (a,a)
            | [<>] -> raise (Stream.Error ("Syntax error: ')' or ',' expected after: " ^ Syntax.stringizer [Ident ss; LeftPar; Nat a])))
      | [<str>] ->
          let a = ref 1 in
          while Stream.peek str = Some (Ident ss) do Stream.junk str; incr a done;
          (!a,!a)

    let parse = parser
      | [<'Ident "a"; len = parse_len "a">] -> Alpha, len
      | [<'Ident "b"; len = parse_len "b">] -> Beta, len
      | [<'Ident "c"; len = parse_len "c">] -> Coil, len

    let parse_compact = parser
      | [<'Ident s>] ->
          let res = ref [] in
          let l = String.length s in
          for i = l - 1 downto 0 do
            res := Ident (String.sub s i 1) :: !res
          done;
          !res

    let print_len (a,b) =
      if a=b
      then
        if a=1
	then []
	else [LeftPar; Nat a; RightPar]
      else [LeftPar; Nat a; Comma; Nat b; RightPar]

    let print = function
      | Alpha, len -> Ident "a" :: print_len len
      | Beta, len -> Ident "b" :: print_len len
      | Coil, len -> Ident "c" :: print_len len

    let entails (x1,(a1,b1)) (x2,(a2,b2)) =
      x1=x2 & a2 <= a1 & b1 <= b2

    let conj (x1,(a1,b1)) (x2,(a2,b2)) =
      let a3,b3 = max a1 a2, min b1 b2 in
      if x1=x2 & a3 <= b3
      then x1, (a3,b3)
      else raise Not_found

    let add x y = y

    let sub x y = if entails x y then raise Not_found else x

    let features x = []

    let gen (x1,(a1,b1)) (x2,(a2,b2)) hs =
      if x1=x2
      then
	let a3, b3 = min a1 a2, max b1 b2 in
        let z = x1, (a3,b3) in
        if Param.gen_cond x1 (a3,b3) (* b3/a3 <= 3 *) & not (List.exists (fun h -> entails h z) hs)
        then LSet.singleton z
        else LSet.empty ()
      else LSet.empty ()

  end
