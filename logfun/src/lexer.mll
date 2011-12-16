{
open Token

let norm_string : string -> string =
  fun s ->
    let r = ref 0 in
    let w = ref 0 in
    let l = String.length s in
    while !r < l do
      let c = s.[!r] in
      if c = '\\'
      then begin
	incr r;
	match s.[!r] with
	  'n' -> s.[!w] <- '\n'
	| 't' -> s.[!w] <- '\t'
	| c' -> s.[!w] <- c' end
      else s.[!w] <- c;
      incr r;
      incr w
    done;
    String.sub s 0 !w;;

}

let skip_char = [' ' '\t' '\n']
let ident_char = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let digit = ['0'-'9']

rule token = parse
| skip_char { token lexbuf }
| "\\\n" { token lexbuf }
| '`' { BackQuote }
| '~' { Tilda }
| '!' { Exclam }
| '@' { At }
| '#' { Sharp }
| '$' { Dollar }
| '%' { Percent }
| '^' { Hat }
| '&' { Et }
| '*' { Star }
| '(' { LeftPar }
| ')' { RightPar }
| '-' { Minus }
| '+' { Plus }
| '=' { Equal }
| '{' { LeftAcc }
| '}' { RightAcc }
| '[' { LeftBra }
| ']' { RightBra }
| '|' { Pipe }
| '\\' { BackSlash }
| '/' { Slash }
| '?' { Interro }
| '<' { LT }
| '>' { GT }
| ',' { Comma }
| '.' { Dot }
| ':' { Colon }
| ';' { SemiColon }
| '"' { DoubleQuote }
| '\'' { Quote }
| ['a'-'z'] ident_char *
    { Ident(Lexing.lexeme lexbuf) }
| ['A'-'Z' '_'] ident_char *
    { Term(Lexing.lexeme lexbuf) }
| digit+
    { Nat(int_of_string(Lexing.lexeme lexbuf)) }
| '`' _ '`'
    { Char((norm_string (Lexing.lexeme lexbuf)).[1]) }
| '"' [^ '\\' '"']* ('\\' _ [^ '\\' '"']*)* '"'
  { let s = Lexing.lexeme lexbuf in String(norm_string (String.sub s 1 (String.length s - 2))) }
| '\'' [^ '\\' '\'']* ('\\' _ [^ '\\' '\'']*)* '\''
  { let s = Lexing.lexeme lexbuf in Term(norm_string (String.sub s 1 (String.length s - 2))) }
| eof { raise Eof }
