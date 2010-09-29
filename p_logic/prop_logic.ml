open Common

open Common_logic

module PC = Parser_combinators
open PC.Infix

(*****************************************************************************)
(* PROP logic (syntax = a AND b, OR, NOT, IMP, (, ), TOP, BOTTOM *)

(* TODO 
 * test, how ?? ex pb du b&c&a |= b&c qui buggait 
 *  - use assign bool tech, and compare
 *  - use quickcheck
 *  - faster if assign bool and see 
 *    (if number of obj < 4 => 4^4 combinaison, can be faster ?? 
 *  use priority as in chaza ? 
 *  could embed in atom more complex stuff (interval, regexp) 
 * => the intersection test for atoms will be in fact a call to embed solver
 *)


(*****************************************************************************)

(* old: open Proptype *)
type atom = string
type formula = 
  | Top    (* true *)
  | Bottom (* false  redundant with Not Top *)

  | Or  of formula * formula
  | And of formula * formula
  | Imp of formula * formula
  | Not of formula

  | Atom of atom




(*****************************************************************************)
(* old: trop dur comme ca 
 * let (relation: formule * formule -> relation) = function
 * | (Top, Top) -> Equal
 * | (Top, x)   -> Imply
 * | (x,   Top) -> Implied
 * | (Atom x, Atom y) -> if (x=y) then Equal else Uncomparable
 * | (Or x1 x2, Or x3 x4)   -> 
 *  match(relation (x1,x3) 
 * 
 * easier to use systeme a la sequent, imply, simplify from left and right, 
 * as sequent calculus sequent must be formule list * formule list, 
 * sinon balaise car faut reduire les & en list
 * 
 * type sequent = (formule list) * (formule list)
 * 
 * le pb c'est si on veut appliquer une regle, ca peut etre sur n'importe
 * quel elt de la liste, en prolog ca va on fait un member (A et B) LG et hop
 * la pas pareil, on serait oblige d'avoir un
 * List.exists (fun (And x y) -> true | _ -> false) puis de le deleter
 * de le recuperer, ... better to do as chaza, diviser en 2 le sequent
 * d'un cote les atomes et de l'autre les trucs un peu complique
 * 
 * Algo: 
 * chazarain, programmer avec scheme, P507
 * 
 * critique: 
 *  - dont like his lsequent, just use or and && of caml/scheme when 
 *    multiple path
 *  - dont like his sequent, with atomg atomd signedformule, 
 *    dont like signedformule
 *  - dont like his side effect, ses sequent sont imperative struct
 *     => he use copy-sequent 
 * 
 * sometimes je vois pas ou il traite les true et false, il a pas les
 * axiomes pour ca, 
 * 
 * tp2 of programming in lambda prolog at irisa in  DEA with bekkers
 *)

type sequent = ((atom list) * (formula list)) * ((atom list) * (formula list))

let (make_sequent: formula * formula -> sequent) = function
  | (Atom x, Atom y) -> ([x], []) , ([y], [])
  | (x,      Atom y) -> ([],  [x]), ([y], [])
  | (Atom x,  y)     -> ([x], []) , ([],  [y])
  | (x,       y)     -> ([],  [x]), ([],  [y])

let (add: formula * (atom list * formula list) -> (atom list * formula list)) = function
  | (Atom x, (y, z)) -> (x::y, z)
  | (x,      (y, z)) -> (y, x::z)

let rec (proof: sequent -> bool) = function
  | ((atoml, fl), (atomr, fr)) when (inter_set atoml atomr != []) -> true (* axiom *)
  | ((atoml, []), (atomr, [])) -> false  (* irreducible *)
  | ((atoml, x::xs), r) -> 
      let l = (atoml, xs) in
      (match x with
      |	(Not f)       -> proof (l,                   add (f,r))
      |	(Or (f1,f2))  -> proof (add (f1,l),          r) && 
	                 proof (add (f2,l),          r)
      |	(And (f1,f2)) -> proof (add (f2,add (f1,l)), r)
      |	(Imp (f1,f2)) -> proof (l,                   add(f1,r)) &&
	                 proof (add(f2,l),           r)
      |	Top           -> proof (l,r)
      |	Bottom        -> true
      |	_ -> failwith "internal error: cant have atom in right part of sequent"
      )
  | (l,(atomr,x::xs)) ->
      let r = (atomr, xs) in
      (match x with
      |	(Not f)       -> proof (add(f,l),            r)
      |	(Or (f1,f2))  -> proof (l,                   add(f2,add (f1,r)))
      |	(And (f1,f2)) -> proof (l,                   add (f1,r)) &&
	                 proof (l,                   add (f2,r))
      |	(Imp (f1,f2)) -> proof (add(f1,l),           add(f2,r))
      |	Top           -> true
      |	Bottom        -> proof (l,r)
      |	_ -> failwith "internal error: cant have atom in right part of sequent"
      )

(*****************************************************************************)
(* Now use parser combinators so can put all the code in the same file. No
 * need to split in a proptype.ml, proplexer.mll, propparser.mly and 
 * prop_logic.ml.
 *
 * old:
 *   let parse = fun s -> 
 *      Lexing.from_string s +> Propparser.main Proplexer.token
 *)

(* ---------------------------------------------------------------------- *)
(* lexer *)
(* ---------------------------------------------------------------------- *)
let kwds = ["AND";"OR";"NOT";"IMP";  "TOP";"BOTTOM"]
let symbols = ["(";")"]

let mykeyword = 
  PC.pred PC.alpha +++ PC.several PC.alpha >| (fun x -> 
    let s = (PC.collect x) in 
    if List.mem s kwds
    then PC.KWD s
    else raise Not_found
  )

let mysymbols = 
  PC.pred PC.symbol +++ PC.several PC.symbol >| (fun x -> 
    let s = (PC.collect x) in 
    if List.mem s symbols
    then PC.SYM s
    else raise Not_found
  )
  

(* the order is important if some "rules" overlap, as in ocamllex *)
let token =
  (mykeyword ||| mysymbols ||| PC.rawident) +++ PC.several PC.space >| fst

let lexer s = PC.lex_gen token s

(* ---------------------------------------------------------------------- *)
(* grammar *)
(* ---------------------------------------------------------------------- *)
let rec atom s =
 (
  (PC.ident           >| (fun x -> Atom x))
  |||
  (PC.a (PC.KWD "NOT") +++ term >| (fun (_, x) -> Not x))
  |||
  (PC.a (PC.KWD "TOP" ) >| (fun _ -> Top))
  |||
  (PC.a (PC.KWD "BOTTOM" ) >| (fun _ -> Bottom))
  |||
  (PC.a (PC.SYM "(") +++ term +++ PC.a (PC.SYM ")")  >| fun ((_, e), _) -> e)
  ) s
and factor s =
 (
  (atom +++ PC.a (PC.KWD "AND") +++ factor   >| fun ((f, _), g) -> And (f,g)) 
  |||
  (atom +++ PC.a (PC.KWD "IMP") +++ factor   >| fun ((f, _), g) -> And (f,g)) 
  |||
  atom
 ) s
and term s =
  (
   (factor +++ PC.a (PC.KWD "OR") +++ term     >| fun ((f, _), g) -> Or (f,g)) 
   |||
   factor
  ) s


let expr =
    term +++ PC.fin >| fst

(* ---------------------------------------------------------------------- *)
(* main *)
(* ---------------------------------------------------------------------- *)
let parse string =
    PC.val_of_parser (expr (lexer string))



(* ---------------------------------------------------------------------- *)
(* test *)
(* ---------------------------------------------------------------------- *)

let _ = Common.example 
  (lexer "(a AND b)" 
  = 
  [PC.SYM "("; PC.IDENT "a"; PC.KWD "AND";PC.IDENT "b";PC.SYM ")"]
  )

(*let _ =   lexer "(a AND b)" +> List.map PC.string_of_token +> List.iter pr2*)

  
let ex1 =  parse "NOT (a AND b)"
let _ = Common.example (ex1 = Not (And (Atom "a", Atom "b")))



(*****************************************************************************)
let prop_logic = fun (Prop s1) (Prop s2) -> 
  let (f1, f2) = (parse s1, parse s2) in
  proof (make_sequent (f1, f2))

(* As we want have a bottom, even prop:aa is a formula and must 
 * be inserted carefully. 
 *)
let is_formula_prop (Prop s) = true 

(*****************************************************************************)
let (main: unit) = interact_logic prop_logic     is_formula_prop
