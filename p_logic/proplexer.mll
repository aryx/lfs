{
open Propparser
}
let identifier = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z' '0'-'9' '-' '_']*

rule token = parse
  | [' ' '\t' '\n']  { token lexbuf }     (* skip blanks *)
  | "AND"          { AND }
  | "OR"           { OR }
  | "IMP"          { IMP }
  | "NOT"          { NOT }
  | '('            { LPAR }
  | ')'            { RPAR }
  | "TOP"          { TOP }
  | "BOTTOM"       { BOTTOM }
  | identifier     { ATOM(Lexing.lexeme lexbuf) } (* to put after keyword !!!! *)
(*  | ['0'-'9']+     { INT(int_of_string(Lexing.lexeme lexbuf)) } *)
  | eof            { EOF }
