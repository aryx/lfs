(** Strings and simple patterns ('contains', 'beginswith', etc.). *)

open Token
open Logic

module type PARAM =
  sig
    val normalize_is : string -> string (** Normalization of value strings *)
    val normalize : string -> string (** Normalization before matching. *)
    val words : string -> (bool * string) list
	(** [words s] generates a set of patterns from [s]: [(vis,s')] represents the pattern 'contains "s'"',
	   and [vis] tells whether the pattern is made visible or not.
	   Used by [features]. *)
  end

module Make (Param : PARAM) =
  struct
    include Default

    let props () =
      {(no_props "Substring") with
       df = isok;
       st = isok;
       st' = isok;
       sg' = isok;
       po_entails = isok;
       cs_entails = isok;
       cp_entails = isok;
       cp'_entails = isok;
       cp_top = isok;
       cs_bot = isok;
       cs_conj = isok;
       cp_conj = isok;
       cs_disj = isok;
       cp_disj = isok;
       reduced_right = isok;
       reduced_bot = isok;
       reduced_bot' = isok;
     }

    type t = Is of string | Contains of string | Begins_with of string | Ends_with of string

    let get_string = function
      | Is s -> s
      | Contains s -> s
      | Begins_with s -> s
      | Ends_with s -> s

    let feat _ = true

    let desc _ = true

    let compare f g = Pervasives.compare (Param.normalize (get_string f)) (Param.normalize (get_string g))

    let isvalue = function
      | Is _ -> true
      | _ -> false

    open Param

    let rec contains : string -> string -> bool =
      fun s s' ->
	s' = "" or
	try
          let pos = String.index s s'.[0] in
	  let len = String.length s' in
	  find_pos pos len s s'
	with _ -> false
    and find_pos =
      fun pos len s s' -> (* assert (s.[pos] = s'.[0]) *)
	(s' = String.sub s pos len) ||
	find_pos (String.index_from s (pos+1) s'.[0]) len s s'

    let begins_with : string -> string -> bool =
      fun s s' ->
	let len = String.length s' in
	try s' = String.sub s 0 len
	with _ -> false

    let ends_with : string -> string -> bool =
      fun s s' ->
	let len = String.length s' in
	let pos = String.length s - len in
	try s' = String.sub s pos len
	with _ -> false


(*****************************)

    let top () = Contains ""

    let entails f g =
      match f,g with
      | Is s, Is s' -> s = s'  (* BEWARE: values are equivalent iff they are syntactically equal *)
      | Is s, Begins_with s'
      | Begins_with s, Begins_with s' -> begins_with (normalize s) (normalize s')
      | Is s, Ends_with s'
      | Ends_with s, Ends_with s' -> ends_with (normalize s) (normalize s')
      | _, Contains s' -> contains (normalize (get_string f)) (normalize s')
      | _, _ -> false

    let conj f g =
      if entails f g then f
      else if entails g f then g
      else raise Not_found

    let add f g = conj f g

    let sub f g = if entails f g then raise Not_found else f

    let features = function
      | Is s -> (true, Contains "")::
	  List.map
	    (function
              | (vis,"") -> (vis, Is s)
              | (vis,s') -> (vis, Contains s')
            ) (words s)
      | _ -> []

    let gen f g hs =
      if f=g & not (List.exists (fun h -> entails h f) hs)
      then LSet.singleton f
      else LSet.empty ()

    let parse _ = failwith "Substring: use functor Verb or Star instead of Make"

    let print _ = failwith "Substring: use functor Verb or Star instead of Make"

(*
    let parse =
      let begins_with s s' = begins_with s s' in
      let msg s = "Syntax error: string expected after: " ^ s in parser
	| [<'String s>] -> Contains s
	| [<'Ident c when begins_with "contains" c; 'String s ?? msg c>] -> Contains s
	| [<'Ident b when begins_with "beginswith" b; 'String s ?? msg b>] -> Begins_with s
	| [<'Ident e when begins_with "endswith" e; 'String s ?? msg e>] -> Ends_with s
	| [<'Ident "is"; 'String s ?? msg "is">] -> Is (Param.normalize_is s)

    let print = function
      |	Contains s -> [Ident "contains"; PP_tilda; String s]
      | Begins_with s -> [Ident "beginswith"; PP_tilda; String s]
      | Ends_with s -> [Ident "endswith"; PP_tilda; String s]
      | Is s -> [Ident "is"; PP_tilda; String s]
*)
  end


module Verb (Param : PARAM) =
  struct
    include Make(Param)

    let parse =
      let begins_with s s' = begins_with s s' in
      let msg s = "Syntax error: string expected after: " ^ s in parser
	| [<'Star>] -> Contains ""
	| [<'String s>] -> Contains s
	| [<'Ident c when begins_with "contains" c; 'String s ?? msg c>] -> Contains s
	| [<'Ident b when begins_with "beginswith" b; 'String s ?? msg b>] -> Begins_with s
	| [<'Ident e when begins_with "endswith" e; 'String s ?? msg e>] -> Ends_with s
	| [<'Ident "is"; 'String s ?? msg "is">] -> Is (Param.normalize_is s)

    let print = function
      | Contains "" -> [Star]
      |	Contains s -> [Ident "contains"; PP_tilda; String s]
      | Begins_with s -> [Ident "beginswith"; PP_tilda; String s]
      | Ends_with s -> [Ident "endswith"; PP_tilda; String s]
      | Is s -> [Ident "is"; PP_tilda; String s]

  end


module Star (Param : PARAM) =
  struct
    include Make(Param)

    let rec parse = parser
      | [<'Star; f = parse_after_star>] -> f
      | [<'String s; f = parse_after_string false s>] -> f
      | [<'Ident "is"; 'String s>] -> Is (Param.normalize_is s)
    and parse_after_star = parser
      | [<'String s; f = parse_after_string true s>] -> f
      | [<>] -> Contains ""
    and parse_after_string after_star s = parser
      | [<'Star>] -> if after_star then Contains s else Begins_with s
      | [<>] -> if after_star then Ends_with s else Is (Param.normalize_is s)

    let print = function
      | Contains "" -> [Star]
      | Contains s -> [Star; String s; Star]
      | Begins_with s -> [String s; Star]
      | Ends_with s -> [Star; String s]
      | Is s -> [String s]
  end
