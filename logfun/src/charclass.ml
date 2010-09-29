(** The characters and various classes (letters, uppercase vowels, punctuation, etc.). *)

module Make =
  struct
    include Logic.Default

    type t = Token.t LSet.t

    open Token

    let token_of_char =
      function
      | 'a' .. 'z' as c -> Token.Ident (String.make 1 c)
      | 'A' .. 'Z' as c -> Token.Term (String.make 1 c)
      | '0' .. '9' as c -> Token.Nat (Char.code c - Char.code '0')
      | '\'' -> Token.Quote
      | '-' -> Token.Minus
      | '.' -> Token.Dot
      | ',' -> Token.Comma
      | ';' -> Token.SemiColon
      | ':' -> Token.Colon
      | '?' -> Token.Interro
      | '!' -> Token.Exclam
      | '^' -> Token.Hat
      | '$' -> Token.Dollar
      | _ -> Token.Term "_"

    let class_of_string s =
      let res = ref (LSet.empty ()) in
      for i = String.length s - 1 downto 0 do
        res := LSet.add (token_of_char s.[i]) !res
      done;
      !res

    let upvow = class_of_string "AEIOU"
    let upcons = class_of_string "BCDFGHJKLMNPQRSTVWXYZ"
    let lowvow = class_of_string "aeiou"
    let lowcons = class_of_string "bcdfghjklmnpqrstvwxyz"
    let up = LSet.union upvow upcons
    let low = LSet.union lowvow lowcons
    let vow = LSet.union upvow lowvow
    let cons = LSet.union upcons lowcons
    let letter = LSet.union up low
    let digit = class_of_string "0123456789"
    let char = LSet.union (class_of_string "'.-") (LSet.union letter digit)
    let punct = class_of_string ".,;:?!"
    let sep = LSet.union punct (class_of_string "_^$")
    let any = LSet.union char sep

    let all_classes =
      List.map (fun t -> Syntax.stringizer [t], LSet.singleton t) any @
      (Common.fold_for
         (fun i res ->
            let s_up = String.make 1 (Char.chr i) in
            let s = s_up ^ String.lowercase s_up in
            (s,class_of_string s)::res)
         (Char.code 'A') (Char.code 'Z') []) @
      ["upvow", upvow; "upcons", upcons; "lowvow", lowvow; "lowcons", lowcons;
       "vow", vow; "cons", cons;  (*"up", up; "low", low;*)
       "letter", letter; "digit", digit; "char", char; "punct", punct; "sep", sep; "any", any]

    let parse =
      let class_of_char c = LSet.singleton (token_of_char c) in
      parser
      | [<'Ident cl when List.mem_assoc cl all_classes>] -> (try List.assoc cl all_classes with Not_found -> any)
      | [<'Term cl when List.mem_assoc cl all_classes>] -> (try List.assoc cl all_classes with Not_found -> any)
      | [<'Nat n when n >= 0 & n < 10>] -> class_of_char (string_of_int n).[0]
      | [<'Quote>] -> class_of_char '\''
      | [<'Minus>] -> class_of_char '-'
      | [<'Dot>] -> class_of_char '.'
      | [<'Comma>] -> class_of_char ','
      | [<'SemiColon>] -> class_of_char ';'
      | [<'Colon>] -> class_of_char ':'
      | [<'Interro>] -> class_of_char '?'
      | [<'Exclam>] -> class_of_char '!'
      | [<'Hat>] -> class_of_char '^'
      | [<'Dollar>] -> class_of_char '$'

    let parse_compact = parser
      | [<'String s>] ->
          let res = ref [] in
          let l = String.length s in
          for i = l - 1 downto 0 do
            res := token_of_char s.[i] :: !res
          done;
          !res

    let print cl =
      let s, cl = List.find (fun (_,cl') -> cl' = cl) all_classes in
      match cl with
      | [] -> assert false
      | [tok] -> [tok]
      | _ -> [Ident s]

    let simpl f =
      if f = upvow then [<'vow (*; 'up;*)>]
      else if f = upcons then [<'cons (*; 'up*)>]
      else if f = lowvow then [<'vow (*; 'low*)>]
      else if f = lowcons then [<'cons (*; 'low*)>]
      else [<>]


    let top () = any

    let bot () = raise Not_found

    let entails f g = LSet.contains g f

    let conj f g = LSet.inter f g

    let add f g = conj f g

    let sub f g =
       let h = LSet.subtract f g in
       if LSet.is_empty h then raise Not_found else h

    let features f = []

    let gen f g hs =
      try
        let _, x = List.find (fun (_,c) -> LSet.contains c (LSet.union f g)) all_classes in
        if List.exists (fun h -> entails h x) hs
        then LSet.empty ()
        else LSet.singleton x
      with Not_found -> LSet.empty ()

  end
