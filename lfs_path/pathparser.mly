%{
open Lfs
%}
%token <string> ATTR MISC
%token AND OR NOT SLASH DOTDOT DOT LPAREN RPAREN DOUBLE EOF 

%left OR
%left AND
%nonassoc NOT

%start main             /* the entry point */
%type <Lfs.path_element list> main
%%
main: 
 | SLASH list EOF       { Slash::$2 }
 |       list EOF       { $1 }

list: 
 | el                    { [$1] } 
 | el SLASH list         { $1::$3 }
 | /* empty */ { [] }

el: 
 | DOTDOT    { DotDot }
 | DOT       { Dot }
 | expr      { Element $1 }

expr: 
 | LPAREN expr RPAREN     { $2 }
 /* comment whole lines to allow in attr */   
 | expr AND expr          { And($1, $3) } 

 | expr OR expr           { Or($1, $3) }
 | NOT expr               { Not($2) }
 | ATTR valu              { Single (Prop ($1 ^ $2)) }
 | MISC                   { Single (Prop $1) }
                 
valu: 
 | MISC valu { $1 ^ $2 } 
 | ATTR valu { $1 ^ $2 } 
 | NOT valu  { "!" ^ $2 }
 /* allow in attr   | AND valu  { "&" ^ $2 } */
 | DOUBLE valu { ":" ^ $2 }
 /* otherwise conflict, force to have good paren */
 | LPAREN valux RPAREN valu  { "(" ^ $2 ^ ")" ^ $4 } 
 | /* empty */ { "" }

valux: 
 | MISC valux { $1 ^ $2 } 
 | ATTR valux { $1 ^ $2 } 
 | NOT valux  { "!" ^ $2 }
 | DOUBLE valux { ":" ^ $2 }
 | AND valux { "&" ^ $2 } /* can now allowed Or & */
 | OR valux { "|" ^ $2 }
 /* otherwise conflict, force to have good paren */
 | LPAREN valux RPAREN valux  { "(" ^ $2 ^ ")" ^ $4 } 
 | /* empty */ { "" }


