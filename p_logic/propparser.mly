%{
open Proptype
%}
%token <string> ATOM
%token AND OR NOT IMP
%token TOP BOTTOM
%token LPAR RPAR
%token EOF
%start main
%type <Proptype.formula> main

%left AND OR IMP
%nonassoc NOT
%%
main:
    expr EOF		                { $1 }
expr:
  | LPAR expr RPAR                      {$2}
  | expr AND  expr                      {And($1,$3)}
  | expr OR   expr                      {Or($1,$3)}
  | expr IMP   expr                     {Imp($1,$3)}
  | NOT  expr                           {Not($2)}
  | TOP                                 {Top}
  | BOTTOM                              {Bottom}
  | ATOM				{Atom($1)}
