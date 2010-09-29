{
open Pathparser
exception Eof
}
rule token = parse
| '&'            { AND }
| '|'            { OR }
| '!'            { NOT }
| '/'            { SLASH }
| '('            { LPAREN }
| ')'            { RPAREN }
| "."            { DOT }    (* used only by core lfs *)
| ".."           { DOTDOT } (* used only by core lfs *)
| ":"            { DOUBLE }
| ['a'-'z' 'A'-'Z' '_' '0'-'9']+ ':'   
      { ATTR(Lexing.lexeme lexbuf) }                             
(* invariant: exhaustive, negatif of other rules *)
| [^ '&' '|' '!' '(' ')' '/' ':']+ 
      { MISC(Lexing.lexeme lexbuf) } 
| eof            { EOF }  
