(** Syntax utility functions. *)

(* analyse lexicale générique *)

type t_stream = Token.t Stream.t

type t_list = Token.t list

open Token

(* forbidden idents in syntax of logics *)
let keywords = ["all";"none";"and";"or";"not";"except";"implies"]

let not_PP t = not (Token.is_PP t)
(*
function
  | PP_tilda | PP_space | PP_cut | PP_break _ | PP_newline -> false
  | _ -> true
*)

let rec length = function
  | [] -> 0
  | x::xs -> (if not_PP x then 1 else 0) + length xs

(* deprecated -> moved into logics *)
(*
let rec get_terms : t_list -> string list =
  function
      [] -> []
    | Term name::l -> name::get_terms l
    | _::l -> get_terms l
*)

let from_channel : in_channel -> t_stream =
  fun ch ->
    let lexbuf = Lexing.from_channel ch in
    Stream.from (fun _ ->
      try Some (Lexer.token lexbuf)
      with Eof -> None)

let from_string : string -> t_stream =
  fun s ->
    let lexbuf = Lexing.from_string s in
    Stream.from (fun _ ->
      try Some (Lexer.token lexbuf)
      with Eof -> None)

let rec list_of_stream s =
  try
    let x = Stream.next s in
    x::list_of_stream s
  with _ -> []

let list_of_string s =
  list_of_stream (from_string s)

let rec of_list : t_list -> t_stream =
  fun toks -> Stream.of_list (List.filter not_PP toks)

(* get the string representation of the token *)
type space_of_token = Sep | PonctL | PonctR | Op | Word

open Format

let rec power10 : int -> float =
      function
      | 0 -> 1.
      | p -> if p > 0
             then power10 (p-1) *. 10.
             else power10 (p+1) /. 10.

let term_escaped : string -> string =
  fun s ->
    let s1 = String.escaped s in
    let l1 = String.length s1 in
    let buf = Buffer.create (2 * l1) in
    for i=0 to l1-1 do
      if s1.[i] = '\''
      then Buffer.add_string buf "\\\'"
      else Buffer.add_char buf s1.[i]
    done;
    Buffer.contents buf

let pp_print_token : formatter -> Token.t -> Token.t -> unit =
  fun ff pred -> function
    | BackQuote -> pp_print_string ff "`"
    | Tilda -> pp_print_string ff "~"
    | Exclam -> pp_print_string ff "!"
    | At -> pp_print_string ff "@"
    | Sharp -> pp_print_string ff "#"
    | Dollar -> pp_print_string ff "$"
    | Percent -> pp_print_string ff "%"
    | Hat -> pp_print_string ff "^"
    | Et -> pp_print_string ff "&"
    | Star -> pp_print_string ff "*"
    | LeftPar -> pp_print_string ff "("
    | RightPar -> pp_print_string ff ")"
    | Minus -> pp_print_string ff "-"
    | Plus -> pp_print_string ff "+"
    | Equal -> pp_print_string ff "="
    | LeftAcc -> pp_print_string ff "{"
    | RightAcc -> pp_print_string ff "}"
    | LeftBra -> pp_print_string ff "["
    | RightBra -> pp_print_string ff "]"
    | Pipe -> pp_print_string ff "|"
    | BackSlash -> pp_print_string ff "\\"
    | Slash -> pp_print_string ff "/"
    | Interro -> pp_print_string ff "?"
    | LT -> pp_print_string ff "<"
    | GT -> pp_print_string ff ">"
    | Comma -> pp_print_string ff ","
(*
    | DotDot ->
(*
	( match pred with
	| Int _ | Float _ -> pp_print_string ff " "
	| _ -> ());
*)
	pp_print_string ff ".."
*)
    | Dot ->
(*
	( match pred with
	| Int _ | Float _ -> pp_print_string ff " "
	| _ -> ());
*)
	pp_print_string ff "."
    | Colon -> pp_print_string ff ":"
    | SemiColon -> pp_print_string ff ";"
    | DoubleQuote -> pp_print_string ff "\""
    | Quote -> pp_print_string ff "'"
    | Ident s ->
	( match pred with
	| Ident _ | Term _ -> pp_print_string ff " "
	| _ -> ());
	pp_print_string ff s
    | Nat n ->
	(match pred with
	| Nat _ | Ident _ | Term _ -> pp_print_string ff " "
	| _ -> ());
	pp_print_int ff n
(*
    | Int n ->
	( match pred with
	| Plus | Minus | Int _ | Float _ | Ident _ | Term _ -> pp_print_string ff " "
	| _ -> ());
	pp_print_int ff n
    | Float (f,p) ->
	( match pred with
	| Plus | Minus | Int _ | Float _ | Ident _ | Term _ -> pp_print_string ff " "
	| _ -> ());
        let sm = if f=0. then "" else string_of_int (int_of_float ((abs_float f) *. (power10 (-p)))) in
	let l = String.length sm in
        let e = let x = (p+l) mod 3 in if x >= 0 then x else x+3 in
        let exp e = if e = 0 then "" else "e" ^ string_of_int e in
	let s =
          (if f < 0. then "-" else "") ^
          if e = 1 then
	    if l >= 1 & p+l+2 <> 0 then String.sub sm 0 1 ^ "." ^ String.sub sm 1 (l-1) ^ exp (p+l-1)
	    else "0.00" ^ sm ^ exp (p + l + 2)
          else if e = 2 then
            if l >= 2 & p+l+1 <> 0 then String.sub sm 0 2 ^ "." ^ String.sub sm 2 (l-2) ^ exp (p+l-2)
            else "0.0" ^ sm ^ exp (p + l + 1)
          else
            if l >= 3 & p+l <> 0 then String.sub sm 0 3 ^ "." ^ String.sub sm 3 (l-3) ^ exp (p+l-3)
            else "0." ^ sm ^ exp (p + l) in
	pp_print_string ff s
*)
    | String s ->
(*	pp_print_string ff ("\"" ^ String.escaped s ^ "\"") *)
	pp_print_string ff ("\"" ^ Str.global_replace (Str.regexp "\"") "\\\"" s ^ "\"")
    | Term s ->
	( match pred with
	| Ident _ | Term _ -> pp_print_string ff " "
	| _ -> ());
	let b = ref true in
	(match s.[0] with 'A'..'Z' | '_' -> () | _ -> b:= false);
	if !b then String.iter (function 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> () | _ -> b:= false) s;
	if !b
	then pp_print_string ff s
	else pp_print_string ff ("'" ^ term_escaped s ^ "'")
    | Char c -> pp_print_string ff ("`" ^ Char.escaped c ^ "`")
    | PP_tilda -> pp_print_string ff " "
    | PP_space -> pp_print_space ff ()
    | PP_cut -> pp_print_cut ff ()
    | PP_break (spaces,offset) -> pp_print_break ff spaces offset
    | PP_newline -> pp_print_newline ff ()


let rec pp_print_tokens : formatter -> t_list -> unit =
  fun ff toks -> pp_print_tokens2 ff PP_newline toks
and pp_print_tokens2 ff pred =
  function
    | [] -> ()
    | tok::toks ->
	pp_print_token ff pred tok;
	pp_print_tokens2 ff tok toks

let stringizer : t_list -> string =
  fun toks ->
    pp_print_tokens Format.str_formatter (List.map (fun tok -> if not_PP tok then tok else PP_tilda) toks);
    Format.flush_str_formatter ()

let print_tokens : t_list -> unit =
  fun toks ->
    pp_print_tokens Format.std_formatter toks

let print_string : string -> unit =
  fun s ->
    pp_print_string Format.std_formatter s

let print_int : int -> unit =
  fun n ->
    pp_print_int Format.std_formatter n


(* fonctions generiques pour le printing *)
(* ------------------------------------- *)

(* printing a signed integer to tokens *)
let toks_of_int : int -> t_list =
  fun n ->
    let s = string_of_int n in
    list_of_stream (from_string s)

let toks_of_float : (float * int) -> t_list =
  fun (f,p) ->
    let sm = if f=0. then "" else string_of_int (int_of_float ((abs_float f) *. (power10 (-p)))) in
    let l = String.length sm in
    let e = let x = (p+l) mod 3 in if x >= 0 then x else x+3 in
    let exp e = if e = 0 then "" else "e" ^ string_of_int e in
    let s =
      (if f < 0. then "-" else "") ^
      if e = 1 then
	if l >= 1 & p+l+2 <> 0 then String.sub sm 0 1 ^ "." ^ String.sub sm 1 (l-1) ^ exp (p+l-1)
	else "0.00" ^ sm ^ exp (p + l + 2)
      else if e = 2 then
        if l >= 2 & p+l+1 <> 0 then String.sub sm 0 2 ^ "." ^ String.sub sm 2 (l-2) ^ exp (p+l-2)
        else "0.0" ^ sm ^ exp (p + l + 1)
      else
        if l >= 3 & p+l <> 0 then String.sub sm 0 3 ^ "." ^ String.sub sm 3 (l-3) ^ exp (p+l-3)
        else "0." ^ sm ^ exp (p + l) in
    list_of_stream (from_string s)

(* messages d'erreur syntaxique *)

let error_RightPar = "Syntax error: a closing parenthesis is missing."
let error_RightBra = "Syntax error: a closing bracket is missing."

(* add parenthesis around the given t_list if op (precedence) is above the ctx (precedence) *)

let add_par : int -> int -> t_list -> t_list =
  fun ctx op l -> if op > ctx then LeftPar::(l @ [RightPar]) else l

(* fonctions generiques pour le parsing *)
(* ------------------------------------ *)

(* parsing a signed integer *)
let parse_int : t_stream -> int = parser
  | [<'Nat n>] -> n
  | [<'Plus; 'Nat n>] -> n
  | [<'Minus; 'Nat n>] -> -n

(* parsing a float *)
let rec parse_float : t_stream -> float * int = parser
  | [<s = parse_float0>] -> (float_of_string s, prec_of_sfloat s)
and parse_float0 = parser
  | [<'Nat m1; s = parse_float2>] -> string_of_int m1 ^ s
  | [<sign = parse_float_sign; s = parse_float1>] -> sign ^ s
and parse_float1 = parser
  | [<'Nat m1; s = parse_float2>] -> string_of_int m1 ^ s
  | [<'Dot; s = parse_float3>] -> "." ^ s
and parse_float2 = parser
  | [<'Dot; s = parse_float3>] -> "." ^ s
and parse_float3 = parser
  | [<'Nat m2; s = parse_float4>] -> string_of_int m2 ^ s
  | [<e = parse_float_e; s = parse_float5>] -> e ^ s
  | [<>] -> ""
and parse_float4 = parser
  | [<e = parse_float_e; s = parse_float5>] -> e ^ s
  | [<>] -> ""
and parse_float5 = parser
  | [<'Nat p>] -> string_of_int p
  | [<sign = parse_float_sign; 'Nat p>] -> sign ^ string_of_int p
and parse_float_sign = parser
  | [<'Minus>] -> "-"
  | [<'Plus>] -> ""
and parse_float_e = parser
  | [<'Ident "e">] -> "e"
  | [<'Term "E">] -> "e"


(* parsing of a list of tokens *)
let rec parse_tokens : t_list -> t_stream -> unit =
fun toks str ->
  match List.filter not_PP toks with
  | [] -> (match str with parser [<>] -> ())
  | tok::toks -> (match str with parser [<'t when t = tok; _ = parse_tokens toks ?? "Syntax error: '"^stringizer toks^"' expected after '"^stringizer [tok]^"'">] -> ())

(* parsing of an optional list of tokens *)
let rec parse_tokens_opt : t_list option -> t_stream -> unit =
fun toks_opt str ->
  match toks_opt with
  | None -> raise Stream.Failure
  | Some toks ->
      match List.filter not_PP toks with
      | [] -> (match str with parser [<>] -> ())
      | tok::toks -> (match str with parser [<'t when t = tok; _ = parse_tokens toks ?? "Syntax error: '"^stringizer toks^"' expected after '"^stringizer [tok]^"'">] -> ())

(* parsing of a list of tokens and something else *)
let rec parse_tokens_and : t_list -> (t_stream -> 'a) -> t_stream -> 'a =
fun toks p str ->
  match List.filter not_PP toks with
  | [] -> (match str with parser [<x = p>] -> x)
  | tok::toks -> (match str with parser [<'t when t = tok; x = parse_tokens_and toks p ?? "Syntax error: '"^stringizer toks^"' expected after '"^stringizer [tok]^"'">] -> x)

(* parsing an indefinite number of pattern given some parser *)
let rec parse_star p = parser
  | [<x = p; xs = parse_star p>] -> x::xs
  | [<>] -> []

let parse_plus p = parser
  | [<x = p; xs = parse_star p>] -> x::xs

(* optional parsing *)
let parse_opt : (t_stream -> 'a) -> 'a -> (t_stream -> 'a) =
  fun p e -> parser
    | [<x = p>] -> x
    | [<>] -> e


(* parsing options *)

let wrong_option s n = "Syntax error: option -"^n^" expected instead of -"^s
let wrong_option_name n = "Syntax error in some option: '"^n^"' expected after '-'"

let parse_option_bool n = parser
| [<'Minus; 'Ident s ?? wrong_option_name n>] -> s=n or raise (Stream.Error (wrong_option s n))
| [<>] -> false

let parse_option_int ?(default=0) n = parser
| [<'Minus; 'Ident s when s=n ?? wrong_option_name n; i = parse_int ?? "Syntax error: integer expected after option -"^n>] -> i
| [<>] -> default

let parse_option_float ? (default=0.) n = parser
| [<'Minus; 'Ident s when s=n ?? wrong_option_name n; (x,_) = parse_float ?? "Syntax error: float expected after option -"^n>] -> x
| [<>] -> default

let parse_option_string ?(default="") n = parser
| [<'Minus; 'Ident s when s=n ?? wrong_option_name n; 'String s2 ?? "Syntax error: string expected after opion -"^n>] -> s2
| [<>] -> default


(* parsing of proposition-like language, where operations and atoms are parameterized *)

type 'a spec_prop = {
    all : string; none : string; a : string; an : string; o : string; n :  string;
    taut : 'a; cont : 'a; neg : 'a -> 'a; conj : 'a -> 'a -> 'a; disj : 'a -> 'a -> 'a;
      atom : t_stream -> 'a
  }

let wrong_prop spec s = "Syntax error: '(', '" ^ spec.n ^ "', '" ^ spec.all ^ "', '" ^ spec.none ^ "' or atom expected after '" ^ s ^ "'"
let wrong_term spec s = "Syntax error: '(', '" ^ spec.n ^ "' or atom expected after '" ^ s ^ "'"
let wrong_fact spec s = "Syntax error: '(', '" ^ spec.n ^ "' or atom expected after '" ^ s ^ "'"

let rec parse_prop spec = parser
  | [<'Token.Ident s when s = spec.all >] -> spec.taut
  | [<'Token.Ident s when s = spec.none>] -> spec.cont
  | [<q = parse_term spec; f = parse_suite spec>] -> f q
and parse_suite spec = parser
    [<'Token.Ident s when s = spec.a; q = parse_prop spec ?? wrong_prop spec s>] -> (fun q' -> spec.conj q' q)
  | [<'Token.Ident s when s = spec.an; q = parse_fact spec ?? wrong_fact spec s>] -> (fun q' -> spec.conj q' (spec.neg q))
  | [<>] -> (fun q' -> q')
and parse_term spec = parser
  | [<q = parse_fact spec; f = parse_term_suite spec>] -> f q
and parse_term_suite spec = parser
  | [<'Token.Ident s when s = spec.o; q = parse_term spec ?? wrong_term spec s>] -> (fun q' -> spec.disj q' q)
  | [<>] -> (fun q' -> q')
and parse_fact spec = parser
  | [<'Token.LeftPar; q = parse_prop spec ?? wrong_prop spec "("; 'Token.RightPar ?? "Syntax error: missing ')' after proposition">] -> q
  | [<'Token.Ident s when s = spec.n; q = parse_fact spec ?? wrong_fact spec s>] -> spec.neg q
  | [<a = spec.atom>] -> a

(* generic functions about strings *)
(* ------------------------------- *)

let rec split (normalize, separator, stopword) (s : string) =
      split2 (normalize, separator, stopword) s 0 "" []
and split2 (normalize, separator, stopword) s i w ws =
      if i>=String.length s then addword (normalize, separator, stopword) w ws
      else if separator s.[i] then split2 (normalize, separator, stopword) s (i+1) "" (addword (normalize, separator, stopword) w ws)
      else split2 (normalize, separator, stopword) s (i+1) (w ^ String.make 1 s.[i]) ws
and addword (normalize, separator, stopword) w ws =
      if w = "" or stopword (normalize w) then ws else w::ws

