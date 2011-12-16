{
open Typeparser
}
let identifier = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z' '0'-'9' '-' '_']*
let qualified  = ['A'-'Z'] ['a'-'z''A'-'Z' '0'-'9']* '.' identifier

rule token = parse
  | [' ' '\t' '\n']  { token lexbuf }     (* skip blanks *)
  | '('            { LPAR }
  | ')'            { RPAR }
  | "*"            { STAR }
  | "'" identifier { POLY     (Lexing.lexeme lexbuf) }
  | "`" identifier { FREEPOLY (Lexing.lexeme lexbuf) }
  | "->"           { FUNC }
  | "["            { LCRO }
  | "]"            { RCRO }
  | ","            { COMMA }
  | "..."          { ETC }
  | "?"            { IN }
  | "!"            { OUT }
  | "%"            { INOUT }
  | identifier     { ATOM(Lexing.lexeme lexbuf) } (* to put after keyword !!!! *)
  | '?' identifier ':' { token lexbuf } (* cos olabl, but dont handle it *)
  | identifier ':'     { token lexbuf }
  | qualified      { ATOM(Lexing.lexeme lexbuf) }
  | eof            { EOF }
