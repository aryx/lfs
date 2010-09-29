(*
   This file contains the examples given in a paper submitted to IJCAR.
   It develops the reconstruction of the description logic ALC, as well
   as variants of it.
   It also gives examples of formulas and subsumption tests.
*)

open Token
open Logic

(* customization of the concrete syntax in each functor *)

module Atom =
  Atom.Make
    (struct
      let names x = not (List.mem x ["some"; "any"; "none"; "and"; "or"; "not"; "except"; "is"; "contains"])
    end)

module Int = Int.Make

module String =
  Substring.Verb
    (struct
      let normalize s = String.lowercase s
      let normalize_is s = s
      let words s = []
    end)

module Prod =
  Prod.Make
    (struct
      let toks_pre = [Ident "some"; PP_tilda; LeftPar]
      let toks_sep = [PP_tilda]
      let toks_suf = [RightPar]
      let toks_bot = [Nat 0]
    end)

module Sum =
  Sum.Make
    (struct
      let toks_a1 = []
      let toks_a2 = []
      let toks_bot = [Nat 0]
    end)

module Set =
  Iset.Make

module Prop =
  Prop.Make
    (struct
      let toks_all = [Ident "any"]
      let toks_none = [Ident "none"]
      let toks_and = [Ident "and"]
      let toks_or = [Ident "or"]
      let toks_not = [Ident "not"]
      let toks_except = [Ident "except"]
      let toks_implies = [Ident "implies"]
      let desc_atom = false
    end)

module Single =
  Single.Make
    (struct
      let toks_K = [Exclam]
      let toks_O = [LeftBra]
      let toks_O_end = [RightBra]
    end)

module Bottom =
  Bottom.Make
    (struct
      let toks_bot = [Nat 0]
    end)


(* utilitary functions for testing logics *)

let parse_log p s =
  p (Syntax.from_string s)

let print_bool b =
  print_endline (string_of_bool b)

let print_prop p b =
  print_string p;
  if LSet.is_empty b
  then print_endline " is ok"
  else begin
    print_string " requires";
    List.iter (fun s -> print_string " "; print_string s) b;
    print_newline () end

let test name props parse entails d q =
  let t = props () in
  print_endline name;
  print_prop "cs_subs" t.cs_entails;
  print_prop "cp_subs" t.cp_entails;
  print_prop "cp'_subs" t.cp'_entails;
  print_string "d = "; print_endline d;
  print_string "q = "; print_endline q;
  print_string "d is subsumed by q ? ";
  print_bool (entails (parse_log parse d) (parse_log parse q));
  print_newline ()


(* definition of successive improvements of ALC-like logics *)

module rec ALC : T = Prop (Set (Sum Atom (Prod Atom ALC)))
    (* the classic logic ALC... *)

let d = "tall and some (child male) and not some (child not tall)"
let q = "some (child male and tall) and not some (child not (male implies tall))"
let _ = test "ALC" ALC.props ALC.parse ALC.entails d q


module GenALC (A : T) (R : T) (Rec : T) : T = Prop (Set (Sum A (Prod R Rec)))
    (* generalized over atoms A, and roles R... *)


module rec ALC1 : T = GenALC (Sum Atom (Sum String Int)) Atom ALC1
    (* plus concrete domains... *)

let d1 = "tall and some (name is \"Peter\") and some (age 39) and some (child some (name is \"Arthur\") and some (age 16))"
let q1 = "some (name contains \"Peter\" or contains \"Paul\") and some (child some (age 20) or some (name any))"
let _ = test "ALC1" ALC1.props ALC1.parse ALC1.entails d1 q1


module rec ALC2 : T = GenALC (Sum Atom (Sum String Int)) (Prop (Set Atom)) ALC2
    (* plus boolean connectives in roles... *)

let d2 = "some ((cousin and friend) doctor and some (name is \"Jack\")) and not some ((not likes) cat) and some (has cat)"
let q2 = "some ((likes or friend) doctor) and some (likes cat)"
let _ = test "ALC2" ALC2.props ALC2.parse ALC2.entails d2 q2


module ALC3 : T = Prop (Bottom (Single ALC2))
    (* plus Closed World Assumption *)

let d3 = "[some (name is \"Peter\") and not female]"
let q3 = "!(not female)"
let q3' = "not !some(name is \"Arthur\")"
let _ = test "ALC3" ALC3.props ALC3.parse ALC3.entails d3 q3
let _ = test "ACL3" ALC3.props ALC3.parse ALC3.entails d3 q3'


(* interactive testing of a logic *)

(*
module LTester = Tester(ALC3)
let _ = LTester.main ()
*)
