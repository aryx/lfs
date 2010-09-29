%{
open Typetype
open Common
%}
%token <string> POLY FREEPOLY ATOM
%token LPAR RPAR STAR FUNC LCRO RCRO COMMA PTVIRG ETC IN OUT INOUT EOF     
%start main             /* the entry point */                                               
%type <Typetype.typep> main                                                                            

%right FUNC /* a -> b -> c  = a -> (b -> c) */
%left STAR
%nonassoc IN
%nonassoc OUT
%nonassoc INOUT
%nonassoc ATOM
%%                                                                                          
/* cf parser.mly from ocaml, from simple_core_type2 */
/* ambiguity on:   iproperty set -> int,  in fact app forcement simple name */
/* cant just put expr STAR expr  and inline  cos how then make difference between a * b * c, and the more explicit a * (b * c) */
/* () contain information, not just to indicate priority */
/* BUG: cos (int * int) * int is parsed the same way that int * int * int */



main: expr1 EOF       { $1 }                                                          

expr1: expr           { $1 }
     | LPAR expr RPAR { $2 }

expr: ATOM           { Name $1 }
    | POLY           { Poly $1 }
    | FREEPOLY       { FreePoly $1 }
    | expr FUNC expr { 
          match $3 with 
	  | (Function (es, e)) -> Function ($1::es, e) 
	  | e                  -> Function ([$1], e) 
		     } /* simplifying */
    | expr ATOM      { Application ($2, $1) }
    | expr STAR expr { Tuple ($1::[$3]) }
    | LPAR expr COMMA expr_comma RPAR { Tuple ($2::$4) }
    | LCRO expr_list RCRO            { IsoParam (fst $2, snd $2) }
    | IN expr        { Function ([IsoParam ([$2], true)], (FreePoly ("`" ^ "_a" ^ (counter() +> i_to_s)))) }
    | OUT expr       { Function ([(FreePoly ("`" ^ "_a" ^ (counter() +> i_to_s)))], $2) } /* TODO? isoparam */
    | INOUT expr     { InOut $2 }
    | LPAR expr RPAR { $2 } 

expr_list: expr                 { ([$1], false) }
         | expr COMMA expr_list { ($1::(fst $3), snd $3) }
         | expr COMMA ETC       { ([$1], true) }

expr_comma: expr                 { [$1] }
          | expr COMMA expr_comma { $1::$3 }
