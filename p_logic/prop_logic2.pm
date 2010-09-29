% -*- prolog -*-
%pmc prop_logic2.pm -o prop_logic2
%##############################################################################
% Commons
%##############################################################################
use "stdio".
use "arith".
use "lists".
use "strings".
use "dcg".
use "std".

#define if
#define then ?
#define else ;

type mem A -> (list A) -> o.            
mem X [X|_].
mem X [_|L] :- mem X L.

type memdel A -> (list A) -> (list A) -> o.            
memdel X [X|L] L :- !.
memdel X [Y|A] [Y|C] :- memdel X A C.

type letter int -> o.
letter C :- C =< 0'z, C >= 0'a.
letter C :- C =< 0'Z, C >= 0'A.
letter C :- C = 0'_.
%'
type flush int -> o.
flush I :-
        call_c (out_c I {{ 
            MK_INTEGER(1, &out[1]) ;
                          fflush(stdout);
                           
        }}) .

%##############################################################################
% Types
%##############################################################################
kind formula type.

type and    formula -> formula -> formula.
type or     formula -> formula -> formula.
type not    formula -> formula.
type supset formula -> formula -> formula.
type vrai   formula.
type faux   formula.
type idf    string -> formula.

%------------------------------------------------------------------------------
op 820 xfy and.
op 850 xfy or.
op 800 fy not.
op 870 xfy supset.

%------------------------------------------------------------------------------
kind sequentT type.
type sequent (list formula) -> (list formula) -> sequentT.

%##############################################################################
% Lexical analysis
%##############################################################################
kind token type.

%TODO generic lexer (ident, symbol, op)
type (lp, rp) token.
type keyword string -> token.
type ident string -> token.

%------------------------------------------------------------------------------
type lex_ident (list int) -> % acc 
               (list int) -> % flow in
               token ->      % res ident 
               (list int) -> % flow out
                o.
lex_ident Acc [C|Flow] Ident Resflow :-
  letter C, !, 
  append Acc [C] Acc2,
  lex_ident Acc2 Flow Ident Resflow.

lex_ident Ident Flot Tok Flot :-
  string_list S Ident,
  (if member S ["OR", "AND", "NOT", "IMP", "BOTTOM", "TOP"] 
   then  Tok = (keyword S) % could do just one categ, matter of taste
   else  Tok = (ident S)
  ).

type lexer (list int) -> (list token) -> o.
lexer []      [].
lexer [0'(|S] [lp|L] :- lexer S L.
lexer [0')|S] [rp|L] :- lexer S L.
lexer [32|S]   L     :- lexer S L.
lexer [0'\b|S] L     :- lexer S L.
lexer [0'\t|S] L     :- lexer S L.
lexer [0'\n|S] L     :- lexer S L.
%lexer [0'\m|S] L             :- lexer S L.
lexer [C|Flow] [Tok|L] :- 
	     letter C,
	     lex_ident [C] Flow Tok ResFlow,
	     lexer ResFlow L.

%##############################################################################
% Syntactic analysis
%##############################################################################
#define parser (list token) -> (list token) -> o

type formulad     formula -> parser.
type atomic       formula -> parser.
type formulabis   formula -> formula -> parser.

formulad G       --> atomic F  & formulabis F G.
atomic (idf S) --> $[ident S].
atomic F       --> $[lp] & formulad F & $[rp].
atomic (not F) --> $[keyword "NOT"] & atomic F.
atomic vrai    --> $[keyword "TOP"].
atomic faux    --> $[keyword "BOTTOM"].

%TODO clean grammar via returning function (lambda)
% si priorite a droite : %.resteformula Acc (Acc ou Gros) --> $[keyword "OR"]  & atomique F & reste_formula F Gros.
formulabis Acc Fcompl --> $[keyword "OR"]  & atomic F & formulabis (Acc or F)     Fcompl.
formulabis Acc Fcompl --> $[keyword "AND"] & atomic F & formulabis (Acc and F)    Fcompl.
formulabis Acc Fcompl --> $[keyword "IMP"] & atomic F & formulabis (Acc supset F) Fcompl.
formulabis Acc Acc    --> $[].

%##############################################################################
% Proof engine
%##############################################################################
type (prouve,
      axiome_1, axiome_2, axiome_3, 
      regle_etg, regle_etd, regle_oug, regle_oud, regle_img, regle_imd, regle_nog, regle_nod
     ) sequentT -> o.
%------------------------------------------------------------------------------
prouve S :- axiome_1 S; axiome_2 S; axiome_3 S;
            regle_etg S; regle_etd S;
            regle_oug S; regle_oud S;
            regle_img S; regle_imd S;
            regle_nog S; regle_nod S.

%------------------------------------------------------------------------------
% axiomes 
%------------------------------------------------------------------------------
axiome_1 (sequent LG LD) :- mem A LG, mem A LD, !.
axiome_2 (sequent LG _)  :- mem faux LG, !.
axiome_3 (sequent _ LD)  :- mem vrai LD, !.

%------------------------------------------------------------------------------
% rules 
%------------------------------------------------------------------------------
regle_etg (sequent LG LD) :-
    memdel (A and B) LG LRES,
    prouve (sequent [A, B |LRES] LD), !.

regle_etd (sequent LG LD) :-
    memdel (A and B) LD LRES,
    prouve (sequent LG [A|LRES]),
    prouve (sequent LG [B|LRES]), !.

regle_oug (sequent LG LD) :-
    memdel (A or B) LG LRES,
    prouve (sequent [A|LRES] LD),
    prouve (sequent [B|LRES] LD), !.
    
regle_oud (sequent LG LD) :-
    memdel (A or B) LD LRES,
    prouve (sequent LG [A, B| LRES]), !.

regle_img (sequent LG LD) :-
    memdel (A supset B) LG LRES,
    prouve (sequent LRES [A|LD]),
    prouve (sequent [B|LRES] LD), !.
    
regle_imd (sequent LG LD) :-
    memdel (A supset B) LD LRES,
    prouve (sequent [A|LG] [B|LRES]), !.

regle_nog (sequent LG LD) :-
    memdel (not A) LG LRES,
    prouve (sequent LRES [A|LD]), !.
    
regle_nod (sequent LG LD) :-
    memdel (not A) LD LRES,
    prouve (sequent [A|LG] LRES), !.

%##############################################################################
% Main
%##############################################################################

type parse string -> formula -> o.
parse S F :- string_list S XS, lexer XS XS1, formulad F XS1 [], !.
parse S _F :- format "parsing error:~p\n" [~S], fail.



%when have not fast logic:
%main _ [_,X,Y]:-


%getS dont work ;(
type read_line (list int) -> o.
read_line X :-
        get0 I,
        (if (I = -1)
         then exit 0
         else
          (if (I = 10)
           then  X = []
           else (read_line Y,  X = [I|Y])
          )
        ).

type loop o -> o.
loop true :-
        read_line P1,
        read_line P2,
        string_list X P1, string_list Y P2,
        (if Y = "IS_FORMULA?"
         then format "yes\n" []
         else
	  (parse X F1, parse Y F2,
                                %  format "f1: ~p\nf2: ~p\n" [~X,~Y],
                                %  format "~p\n" [~F1],
	  (if prouve (sequent [F1] [F2])
          then format "yes\n" []
          else format "no\n" []
          ))
        ),
        flush I, !, 
        loop true.

type main int -> (list string) -> o.
main _  _ :-
        loop true.
        %get0 I, 
        %format "res=~p\n" [~I].


        
        

