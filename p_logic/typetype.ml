open Common


type typep =
  | Name of string
  | Poly of string               (* note: the string in poly and freepoly contain the ' and `, more convenient *)
  | Tuple of typep list
  | Application of string * typep (* note: 'a list -> Application (list, 'a) *) (* TODO? could have a 'a and a kind of typep for left too *)

  | Function of typep list * typep
   (* CHANGE? func returning func are not so useful, so special case make code after simpler *)
   (* TODO? could handle fact that sometimes programme make it more explicit via () and not merging *)

  | IsoParam of typep list * bool (* true mean can have more, false mean exact *)
  | FreePoly of string
(*  | PolyPoly of string,   this one support the alpha renaming, otherwise cant search for exactly 'a, and would be bad for fast logic :) *)
  | InOut of typep (* kind of sugar for ? !, but can be too not a func, but a var, and can be nested *)
	           (* kind of gather all that talk-about a type *)

  (*| Or | And | Not   => % could be done via builtin (?|!) *)


let rec string_of_typep = function
  | Name s -> s
  | Poly s -> s
  | Function (es, e2) ->
      "(" ^ (es +> map string_of_typep +> join "|") ^ ")"
       ^ "->" ^
      "(" ^ string_of_typep e2 ^ ")"
  | Tuple xs -> "(" ^ join " * " (xs +> map string_of_typep) ^ ")"
  | Application (s, e2) -> s ^ " " ^ string_of_typep e2

  | IsoParam (xs, b) -> "[" ^ join "," (xs +> map string_of_typep) ^ (if b then ",..." else "") ^ "]"
  | FreePoly s -> s
  | InOut e -> "%" ^ string_of_typep e

let rec is_formula = function
  | Name s -> false
  | Poly s -> false
  | Function (es, e2) -> es +> List.exists is_formula || is_formula e2
  | Tuple xs -> xs +> List.exists is_formula
  | Application (e1, e2) -> is_formula e2

  | IsoParam _ -> true
  | FreePoly s -> true
  | InOut s -> true



let rec invariant x = true

type subst = (string, typep) assoc (* 'a ou `a *)

let normalise prefix x = x
