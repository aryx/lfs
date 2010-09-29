

module Make =
  struct
    include Logic.Default

    let constants = [
      "tro";"do";"pro";"po";
      "la";"da";"ja";"ha";"tra";
      "ajn"; "en"; "sen"; "tamen"; "jen"; "kun"; "nun";
      "kaj"; "plej"; "tuj";
      "ne";"ke";"de";"se";"tre";"je";"cxe";
      "tri";"mi";"vi";"li";"sxi";"ni";"ili";"oni";"si";"pri";"gxi";"pli";"malpli";"cxi";
      "al";"el";"krom";"jam";
      "unu";"du";"cxu";"plu";"nu";"jes";
      "oj"; "ho"; "ha"; "ve"; "hu"; "he"; "aj"
    ]

    let patterns =
      [ "n", ("n$", ["ajn"; "en"; "sen"; "tamen"; "jen"; "kun"; "nun"])
      ; "j", ("jn?$", ["kaj"; "plej"; "tuj"; "ajn"])
      ; "o", ("oj?n?$", ["tro"; "do"])
      ; "a", ("aj?n?$", ["la"; "da"; "ja"; "kaj"; "ajn"; "tra"])
      ; "i", ("i$", ["pri"; "pli"; "malpli"; "cxi"; "tri"])
      ; "e", ("en?$", ["ne"; "ke"; "se"; "de"; "cxe"; "je"; "en"; "sen"; "jen"])
      ; "as", ("as$", [])
      ; "is", ("is$", [])
      ; "os", ("os$", [])
      ; "us", ("us$", ["jxus"])
      ; "u", ("u$", ["cxu"; "plu"; "nu"; "unu"; "du"; "kiu"; "iu"; "tiu"; "cxiu"; "neniu"])
      ; "ki", ("^ki[aeou]", [])
      ; "xi", ("^i[aeou]", [])
      ; "ti", ("^ti[aeou]", [])
      ; "cxi", ("^cxi[aeou]", [])
      ; "neni", ("^neni[aeou]", [])
      ; "mi", ("^mia?j?n?$", [])
      ; "vi", ("^via?j?n?$", [])
      ; "li", ("^lia?j?n?$", [])
      ; "sxi", ("^sxia?j?n?$", [])
      ; "gxi", ("^gxia?j?n?$", [])
      ; "oni", ("^onia?j?n?$", [])
      ; "si", ("^sia?j?n?$", [])
      ; "ni", ("^nia?j?n?$", [])
      ; "ili", ("^ilia?j?n?$", [])
      ; "nu", ("^nu$", ["nun"])
      ]

    let prefix : string -> int -> string =
      fun w n ->
	try String.sub w 0 n
	with _ -> ""
	  
    let suffix : string -> int -> string =
      fun w n ->
	try String.sub w (String.length w - n) n
	with _ -> ""



    let split_finoj : string -> string * string list =
      fun v ->
	let l = String.length v in
	let suf1 = suffix v 1 in
	if l <> 2 && List.mem suf1 ["a"; "e"; "i"; "o"; "u"] then (prefix v (l-1), [suf1])
	else
	  let suf2 = suffix v 2 in
	  if l <> 3 && List.mem suf2 ["as"; "is"; "os"; "us"] then (prefix v (l-2), [suf2])
	  else if l <> 3 && List.mem suf2 ["aj"; "oj"; "uj"] then (prefix v (l-2), [String.sub suf2 0 1; "j"])
	  else if l <> 3 && List.mem suf2 ["an"; "on"; "un"; "en"] then (prefix v (l-2), [String.sub suf2 0 1; "n"])
	  else if (l = 3 || l = 4) && List.mem suf2 ["in"] then (prefix v (l-1), ["n"])
	  else
	    let suf3 = suffix v 3 in
	    if l <> 4 && List.mem suf3 ["ajn"; "ojn"; "ujn"] then (prefix v (l-3), [String.sub suf3 0 1; "j"; "n"])
	    else (v, [])

    let pred_pre : int -> string -> int -> string list -> bool =
      fun npre w lw lpre ->
	lw - npre <> 1 &&  (* le radical n'a pas 1 seul caractere *)
	let w' = suffix w (lw - npre) in
	(w' = "" || List.exists (fun c -> String.contains w' c) ['a';'e';'i';'o';'u']) &&  (* le radical a au moins une voyelle s'il est non vide *)
	List.mem (prefix w npre) lpre  (* le prefix a du sens *)

    let rec split_pres : string list * string -> string list * string =
      fun (pres, w) -> 
	let l = String.length w in
	if pred_pre 6 w l ["antaux";"ekster"]
	then split_pres (pres @ [prefix w 6], suffix w (l-6))
	else if pred_pre 5 w l ["retro";"super";"inter";"trans"]
	then split_pres (pres @ [prefix w 5], suffix w (l-5))
	else if pred_pre 4 w l ["post";"gxis";"neni"]
	then split_pres (pres @ [prefix w 4], suffix w (l-4))
	else if pred_pre 3 w l ["dis";"eks";"for";"mal";"mis";"pra";"sub";"sur";"cxe";"tra";"dum";"kun";"pri";"sen";"sin";"mem";"sam"]
	then split_pres (pres @ [prefix w 3], suffix w (l-3))
	else if pred_pre 2 w l ["bo";"ek";"fi";"ge";"re";"al";"en";"el";"ne";"cxi";"ti";"ki"]
	then split_pres (pres @ [prefix w 2], suffix w (l-2))
	else pres, w

    let pred_suf : int -> string -> int -> string list -> bool =
      fun nsuf w lw lsuf ->
	lw - nsuf > 1 &&  (* le radical n'a pas 1 seul caractere *)
	let w' = prefix w (lw - nsuf) in
	(w' = "" || List.exists (fun c -> String.contains w' c) ['a';'e';'i';'o';'u']) &&  (* le radical a au moins une voyelle *)
	List.mem (suffix w nsuf) lsuf  (* le suffixe a du sens *)

    let rec split_sufs : string * string list -> string * string list = 
      fun (w, sufs) -> 
	let l = String.length w in
	if pred_suf 4 w l ["estr"]
	then split_sufs (prefix w (l-4), suffix w 4 :: sufs)
	else if pred_suf 3 w l ["acx";"ajx";"cxj";"ebl";"end";"igx";"ind";"ing";"ism";"ist";"obl";"int";"ant";"ont"]
	then split_sufs (prefix w (l-3), suffix w 3 :: sufs)
	else if pred_suf 2 w l ["ad";"an";"ar";"ec";"eg";"ej";"em";"er";"et";"id";"ig";"il";"in";"iv";"nj";"on";"op";"uj";"ul";"um";"it";"at";"ot"]
	then split_sufs (prefix w (l-2), suffix w 2 :: sufs)
	else w, sufs

    let split : string -> string list =
      fun v ->
	if List.mem v constants
	then [v]
	else
	  let v1, finoj = split_finoj v in
	  let v2, sufs = split_sufs (v1,[]) in
	  let pres, radiko = split_pres ([],v2) in
	  pres @ (if radiko = "" then [] else [radiko]) @ sufs @ finoj



    type t = string list

    let isvalue v = false

    open Token

    let parse = parser
      | [<'Ident n>] -> split (String.lowercase n)

    let print = function
      | l -> [Ident (String.concat "" l)]

    let entails l1 l2 =
      let l1' = List.filter (fun x1 -> List.mem x1 l2) l1 in
      l1' = l2

    let features l =
      (true, l) ::
      (true, List.filter (fun x -> not (List.mem x ["j"; "n"])) l) ::
      List.map (fun x -> (true,[x])) l

(*
    let str_match s re =
      try Str.search_forward (Str.regexp re) s 0; true
      with Not_found -> false

    let entails a b =
      not (List.mem_assoc a patterns) &&
      try
	let pat, excs = List.assoc b patterns in
	not (List.mem a excs) &&
	str_match a pat
      with Not_found ->
	let re = Str.regexp "^\\(.+[aou]\\)\\(j?n?\\)$" in
	if Str.string_match re a 0
	then
	  let radiko_a = Str.matched_group 1 a in
	  let finajxo_a = Str.matched_group 2 a in
	  if Str.string_match re b 0
	  then
	    let radiko_b = Str.matched_group 1 b in
	    let finajxo_b = Str.matched_group 2 b in
	    radiko_a = radiko_b &&
	    str_match finajxo_a finajxo_b
	  else false
	else false

(*
      not (List.mem a constants) && 
      not (List.mem b constants) && 
      not (List.mem_assoc a patterns) &&
      str_match a (try List.assoc b patterns with Not_found -> b)
*)

    let features a =
      (true,a) ::
      if List.mem a constants
      then []
      else List.map (fun (pat,(re,excs)) -> (true,pat)) (List.filter (fun (pat,(re,excs)) -> entails a pat) patterns)
*)

  end
