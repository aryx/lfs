(** The classic atoms found in most logics (propositional logic, predicate logic, etc.). *)

open Logic

module type PARAM =
  sig
    val names : string -> bool (** The predicate for admissible names for atoms. *)
  end

module Make (Param : PARAM) : Logic.T =
  struct
    include Default

    let props () =
      {(no_props "Atom") with
       df = isok;
       st' = isok;
       sg' = isok;
       po_entails = isok;
       cs_entails = isok;
       cp_entails = isok;
       cp'_entails = isok;
       cp_top = isok;
       cs_bot = isok;
       defst_conj = isok;
       cs_conj = isok;
       cp_conj = isok;
       cs_disj = isok;
       cp_disj = isok;
       reduced_right = isok;
       reduced_bot = isok;
       reduced_bot' = isok;
     }

    type t = Attr of string | Term of string

    let isvalue f = true

    let parse = parser
      |	[<'Token.Term n
	  when Param.names n
	      & not (List.mem n Syntax.keywords)>] -> Term n
      |	[<'Token.Ident a
	  when Param.names a
	      & not (List.mem a Syntax.keywords)>] -> Attr a

    let parse_compact = parser
      | [<'Token.String s>] ->
          let res = ref [] in
          let l = String.length s in
          for i = l - 1 downto 0 do
            match s.[i] with
            | 'a' .. 'z' -> res := Token.Ident (String.sub s i 1) :: !res
            | 'A' .. 'Z' -> res := Token.Term (String.sub s i 1) :: !res
            | ' ' | '\t' | '\n' -> res := Token.Term "_" :: !res
            | _ -> ()
          done;
          !res

    let print = function
      | Attr a -> [Token.Ident a]
      |	Term name -> [Token.Term name]


    let rec entails a b = a=b

    let conj a b =
      if a=b then a else raise Not_found

    let disj a b =
      if a=b then [a] else [a; b]

    let features a = LSet.singleton (true,a)

    let rec gen y d gs =
      if y=d & not (List.exists (fun x -> x = y) gs)
      then LSet.singleton y
      else LSet.empty ()
  end

