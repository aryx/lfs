(** Nucleotidic elements, and their various classes. *)

module Make =
  struct
    include Logic.Default

    type t = string LSet.t

    let all_nt = LSet.of_list ["a";"c";"g";"t"]

    let class_of_string s =
      let res = ref (LSet.empty ()) in
      for i = String.length s - 1 downto 0 do
        res := LSet.add (String.sub s i 1) !res
      done;
      !res

    let r = class_of_string "ag"
    let y = class_of_string "ct"
    let w = class_of_string "at"
    let s = class_of_string "cg"
    let m = class_of_string "ac"
    let k = class_of_string "gt"
    let h = class_of_string "act"
    let b = class_of_string "cgt"
    let v = class_of_string "acg"
    let d = class_of_string "agt"
    let n = class_of_string "acgt"

    let all_classes =
      List.map (fun nt -> nt, LSet.singleton nt) all_nt @
      ["r",r; "y",y; "w",w; "s",s; "m",m; "k",k; "h",h; "b",b; "v",v; "d",d; "n",n]

    open Token

    let parse = parser
      | [<'Ident s>] ->
          try List.assoc s all_classes
          with Not_found -> raise (Stream.Error ("Syntax error: invalid IUPAC nucleotide code: " ^ s))

    let parse_compact = parser
      | [<'Ident s>] ->
          let res = ref [] in
          let l = String.length s in
          for i = l - 1 downto 0 do
            res := Ident (String.sub s i 1) :: !res
          done;
          !res

    let print cl =
      let s, _ = List.find (fun (_,cl') -> cl' = cl) all_classes in
      [Ident s]

    let top () = n

    let entails a b = LSet.contains b a

    let conj a b = LSet.inter a b

    let add a b = conj a b

    let sub a b =
       let c = LSet.subtract a b in
       if LSet.is_empty c then raise Not_found else c

    let features a = []

    let gen a b cs =
      try
        let _, x = List.find (fun (_,c) -> LSet.contains c (LSet.union a b)) all_classes in
        if List.exists (fun c -> entails c x) cs
        then LSet.empty ()
        else LSet.singleton x
      with Not_found -> LSet.empty ()

  end
