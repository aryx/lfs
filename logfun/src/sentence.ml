(** Strings and simple patterns ('contains', 'beginswith', word borders, etc.). *)

open Logic

module type PARAM =
  sig
    val normalize_is : string -> string (** Normalization of value strings *)
    val normalize : string -> string (** Normalization before pattern matching. *)
    val separator : char -> bool  (** The class of character separators for word borders. *)
    val words : string -> (bool * (bool * string * bool)) list
      (** [words s] generates a set of patterns from [s]: [(b,s',e)] represents the pattern 'contains \{"s'"\}',
	 where the opening \{ occurs if [b=true], and the closing \} occurs if [e=true] (word borders).
	 Used by [features]. Add [(true,"",true)] to generate the pattern 'is "s"'. *)
  end

module Make (Param : PARAM) =
  struct
    include Default

    let props () =
      {(no_props "Sentence") with
       st = isok;
       st' = isok;
       cs_entails = isok;
       cp_entails = isok;
       cp'_entails = isok;
       cp_top = isok;
       cs_bot = isok;
     }

    type word = bool * string * bool  (* begin of word?, string, end of word? *)
    type t = Is of string | Contains of word | Begins_with of word | Ends_with of word

    let get_string = function
      | Is s -> s
      | Contains (_,s,_) -> s
      | Begins_with (_,s,_) -> s
      | Ends_with (_,s,_) -> s

    let desc = function
      | Is _ -> true
      | _ -> false

    let compare f g = Pervasives.compare (Param.normalize (get_string f)) (Param.normalize (get_string g))

    let isvalue = function
      | Is _ -> true
      | _ -> false

    open Param

    let rec contains : word -> word -> bool =
      fun (b,s,e) (b',s',e') ->
	s' = "" or
	try
          let pos = String.index s s'.[0] in
	  let len = String.length s' in
	  find_pos pos len (b,s,e) (b',s',e')
	with _ -> false
    and find_pos =
      fun pos len (b,s,e) (b',s',e') -> (* assert (s.[pos] = s'.[0]) *)
	if s' = String.sub s pos len
         & (not b' or (if pos=0 then b else separator s.[pos-1]))
	 & (not e' or (if (pos+len)=(String.length s) then e else separator s.[pos+len]))
	then true
	else find_pos (String.index_from s (pos+1) s'.[0]) len (b,s,e) (b',s',e')

    let begins_with : word -> word -> bool =
      fun (b,s,e) (b',s',e') ->
	let len = String.length s' in
	try s' = String.sub s 0 len
	    & (not b' or b)
	    & (not e' or (if len=(String.length s) then e else separator s.[len]))
	with _ -> false

    let ends_with : word -> word -> bool =
      fun (b,s,e) (b',s',e') ->
	let len = String.length s' in
	let pos = String.length s - len in
	try s' = String.sub s pos len
	    & (not b' or (if pos=0 then b else separator s.[pos-1]))
	    & (not e' or e)
	with _ -> false

(*
    let rec words separator s = words2 s 0 "" []
    and words2 s i w ws =
      if i>=String.length s then addword w ws
      else if separator s.[i] then words2 s (i+1) "" (addword w ws)
      else words2 s (i+1) (w ^ String.make 1 s.[i]) ws
    and addword w ws =
      if w = "" or stopword (normalize w) then ws else w::ws
*)

(*****************************)

    let top () = Contains (false,"",false)

    let entails f g =
      let norm_is s = (true,normalize s,true) in
      let norm_word (b,s,e) = (b,normalize s,e) in
      match f,g with
	Is s, Is s' -> s = s'  (* BEWARE: values are equivalent iff they are syntactically equal *)
      | Is s, Contains w' -> contains (norm_is s) (norm_word w')
      | Is s, Begins_with w' -> begins_with (norm_is s) (norm_word w')
      | Is s, Ends_with w' -> ends_with (norm_is s) (norm_word w')
      | Begins_with w, Begins_with w' -> begins_with (norm_word w) (norm_word w')
      | Begins_with w, Contains w' -> contains (norm_word w) (norm_word w')
      | Ends_with w, Ends_with w' -> ends_with (norm_word w) (norm_word w')
      | Ends_with w, Contains w' -> contains (norm_word w) (norm_word w')
      | Contains w, Contains w' -> contains (norm_word w) (norm_word w')
      | _, _ -> false

    let conj f g =
      if entails f g then f
      else if entails g f then g
      else raise Not_found

    let add f g = conj f g

    let sub f g = if entails f g then raise Not_found else f

    let features = function
      | Is s -> (true,(top ()))::List.fold_left
           (fun l -> function
           | (vis,(_,"",_)) -> (vis,(Is s))::l
           | (vis,(b,s',e)) -> (vis,(Contains (b,s',e)))::l
           ) [] (words s)
      | _ -> []
(*
      | Begins_with w -> true,(top ())::vis,(Begins_with w)::[]
      | Ends_with w -> top ()::Ends_with w::[]
      | Contains w -> top ()::Contains w::[]
*)

    let gen f g hs =
      if f=g & not (List.exists (fun h -> entails h f) hs)
      then LSet.singleton f
      else LSet.empty ()

    let parse_prefix = parser
	[<'Token.LeftAcc>] -> true
      |	[<>] -> false

    let parse_suffix = parser
	[<'Token.RightAcc>] -> true
      |	[<>] -> false

    let parse_word = parser
	[<b = parse_prefix; 'Token.String s; e = parse_suffix>] -> (b,s,e)

    let parse =
      let begins_with s s' = begins_with (true,s,true) (false,s',false) in
      let msg s = "Syntax error: string expected after: " ^ s in parser
	| [<'Token.Ident c when begins_with "contains" c; w = parse_word ?? msg c>] -> Contains w
	| [<'Token.Ident b when begins_with "beginswith" b; w = parse_word ?? msg b>] -> Begins_with w
	| [<'Token.Ident e when begins_with "endswith" e; w = parse_word ?? msg e>] -> Ends_with w
	| [<'Token.Ident "is";'Token.String s ?? msg "is">] -> Is (Param.normalize_is s)

    let print_word (b,s,e) =
      (if b then [Token.LeftAcc] else []) @ [Token.String s] @ (if e then [Token.RightAcc] else [])

    let print = function
	Contains w -> Token.Ident "contains"::Token.PP_tilda::print_word w
      | Begins_with w -> Token.Ident "beginswith"::Token.PP_tilda::print_word w
      | Ends_with w -> Token.Ident "endswith"::Token.PP_tilda::print_word w
      | Is s -> [Token.Ident "is"; Token.PP_tilda; Token.String s]

  end
