(* obsolete: now prop_logic.ml use parser combinators so no need lex/yacc
 * and the split of files that force to introduce such a file
 *)

type atom = string
type formula = 
  | Top    (* true *)
  | Bottom (* false  redundant with Not Top *)

  | Or  of formula * formula
  | And of formula * formula
  | Imp of formula * formula
  | Not of formula

  | Atom of atom
