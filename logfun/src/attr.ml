(** Like the logic module [Atom] plus the possibility to order atoms in a taxonomy.

   The taxonomy links are handled by the module [Term].

*)

open Logic

module type PARAM = sig
  val toks_top : Syntax.t_list option (* None is no top is wanted *)
  val names : string -> bool (** The predicate for admissible names for atoms. *)
end

module Make (Param : PARAM) : Logic.T =
  struct
    include Default

    let props () =
      {(no_props "Attr") with
       st' = isok;
       df = isok;
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
       reduced = isok;
       reduced' = isok}

    type t = int
	  (* 0 for top, <0 for attrs and >0 for terms *)
	  (* use Term facilities *)

(*    type t = Top | Attr of string | Term of string *)

    let get_string = function
      | 0 -> ""
      | n -> Term.id2s n
(*
      | Top -> ""
      | Attr s -> s
      | Term s -> s
*)

    let compare f g = Pervasives.compare (String.lowercase (get_string f)) (String.lowercase (get_string g))

    let isvalue f =
      f < 0
(*
    let isvalue = function
      | Top -> false
      | Attr _ -> true
      | Term t -> false (*Term.get_sufs t = []*)
*)

    let parse = parser
      | [<_ = Syntax.parse_tokens_opt Param.toks_top>] -> 0
      |	[<'Token.Term n
	  when not (List.mem n Syntax.keywords)>] -> Term.term2id n
      |	[<'Token.Ident a
	  when Param.names a
	      & not (List.mem a Syntax.keywords)>] -> Term.ident2id a

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

    let print f =
      if f = 0 then (match Param.toks_top with Some toks -> toks | None -> invalid_arg "Attr.print: unexpected Top")
      else if f < 0 then [Token.Ident (Term.id2s f)]
      else [Token.Term (Term.id2s f)]

(*    let mpt l = List.map (fun toks -> parse (Syntax.of_list toks)) l *)

    let top () = match Param.toks_top with Some _ -> 0 | None -> raise Not_found

    let rec entails f g =
      g = 0 ||
      f = g ||
      (f > 0 && g > 0 && (
       List.exists (fun g' -> entails f g') (Term.get_sufs g) ||
       List.exists (fun f' -> entails f' g) (Term.get_necs f)))

(*
    let rec entails a b = match a,b with
    | _, Top -> true
    | Top, _ -> false
    | Attr x, Attr y -> x = y
    | Term m, Term n -> m = n or
	List.exists (fun b' -> entails a b') (mpt (Term.get_sufs n)) or
	List.exists (fun a' -> entails a' b) (mpt (Term.get_necs m))
    | _, _ -> false
*)

(*
    let share a b =
      entails a b or
      entails b a or
      match a, b with
      | Term m, _ -> List.exists (fun a' -> share a' b) (mpt (Term.get_sufs m))
      | _, Term n -> List.exists (fun b' -> share a b') (mpt (Term.get_sufs n))
      | _, _ -> false
*)

(*
    let conj a b =
      if entails a b then a
      else if entails b a then b
      else raise Not_found
*)

    let rec features f =
      let top_feat = match Param.toks_top with Some _ -> [(true,0)] | None -> [] in
      if f = 0 then top_feat
      else if f < 0 then top_feat @ [(true,f)]
      else top_feat @ List.fold_left (fun res c -> LSet.union res (features c)) [true,f] (Term.get_necs f)

    let terms id =
      if id > 0
      then [Term.id2s id]
      else []

    let axiom a b_opt =
      if a > 0 then
	match (if b_opt = Some 0 then None else b_opt) with
	| None ->
	    List.iter
	      (fun nec -> Term.del_nec a nec)
	      (Term.get_necs a);
	    LSet.singleton (Term.id2s a)
	| Some b ->
	    if b > 0 then begin
	      if entails a b
	      then begin
		List.iter
		  (fun nec -> Term.del_nec a nec)
		  (List.filter
		     (fun c -> entails c b)
		     (Term.get_necs a));
		Term.add_nec a b end
	      else begin
		List.iter
		  (fun nec -> Term.del_nec a nec)
		  (List.filter
		     (fun c -> entails b c)
		     (Term.get_necs a));
		Term.add_nec a b end;
	      LSet.singleton (Term.id2s a) end
	    else LSet.empty ()
      else LSet.empty ()


    let rec gen y d gs =
      List.fold_left
        (fun res x ->
           if List.exists (fun g -> entails g x) res
           then res
           else LSet.add x (List.filter (fun g -> not (entails x g)) res))
        gs
        (LSet.inter (List.map snd (features y)) (List.map snd (features d)))

(*
      List.filter
        (fun x -> not (List.exists (fun g -> entails g x) gs))
        (LSet.inter (features y) (features d))
*)

(*
    let rec gen2 b c al =
        let xs = List.filter
          (fun x -> not (List.exists (fun a -> entails a x) al))
          ( match b with
          | Attr _ -> []
          | Term n -> mpt (Term.get_necs n)) in
        List.fold_right
          (fun x res ->
             if List.mem x res then res
             else if entails c x then LSet.add x res
             else LSet.union res (gen2 x c al))
          xs
          []
    and gen b c al =
      if entails c b then [b]
      else if entails b c then [c]
      else gen2 b c al
*)
  end

