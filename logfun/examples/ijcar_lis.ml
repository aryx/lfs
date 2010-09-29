(*
   This file contains the examples given in a paper submitted to IJCAR.
   It develops the progressive construction of a logic for Logical Information
   Systems (multisets of valued attributes, closed world assumption, etc.).
   It also gives examples of formulas and subsumption tests.
*)

open Token
open Logic

(* customization of the concrete syntax in each functor *)

let toks_bot = [Sharp]

module Int = Int.Make

module Interval = Openinterval.DotDot

module String =
  Substring.Verb
    (struct
      let normalize s = String.lowercase s
      let normalize_is s = s
      let words s = []
    end)

module Sum =
  Sum.Make
    (struct
      let toks_a1 = []
      let toks_a2 = []
      let toks_bot = toks_bot
    end)

module Top =
  Top.Make
    (struct
      let toks_top = [PP_tilda; Interro]
      let feat_top = false
    end)

module Option =
  Option.Make
    (struct
      let toks_some = [PP_tilda]
      let toks_none = []
      let toks_bot = toks_bot
    end)

module Atom =
  Atom.Make
    (struct
      let names s = not (List.mem s [])
    end)

module Prod =
  Prod.Make
    (struct
      let toks_pre = []
      let toks_sep = []
      let toks_suf = []
      let toks_bot = toks_bot
    end)

module PropInt =
  Prop.Make
    (struct
      let toks_all = [Nat 1]
      let toks_none = [Nat 0]
      let toks_and = [Et]
      let toks_or = [Pipe]
      let toks_not = [Exclam]
      let toks_except = [Minus]
      let toks_implies = [Minus; GT]
      let desc_atom = true
    end)

module Multiset =
  Multiset.Make
    (struct
      let toks_sep = [Comma]
      let toks_begin = [LeftAcc]
      let toks_end = [RightAcc]
      let toks_empty = [LeftAcc; RightAcc]
      let no_begin_end_when_singleton = true
    end)

module PropExt =
  Prop.Make
    (struct
      let toks_all = [Ident "all"]
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
      let toks_K = []
      let toks_O = [LeftBra]
      let toks_O_end = [RightBra]
    end)

module Bottom =
  Bottom.Make
    (struct
      let toks_bot = toks_bot
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

let test name props parse entails d qs =
  let t = props () in
  print_endline name;
  print_prop "cs_subs" t.cs_entails;
  print_prop "cp'_subs" t.cp'_entails;
  print_string "d = "; print_endline d;
  List.iter
    (fun q ->
      print_string "q = "; print_endline q;
      print_string "d is subsumed by q ? ";
    print_bool (entails (parse_log parse d) (parse_log parse q)))
    qs;
  print_newline ()


(* progressive construction of a logic for LIS *)

module L1 : T = Multiset Atom
    (* multisets of atoms... *)

let d1 = "{TheJungleBook, paper_back, recent}"
let q1 = "{recent, TheJungleBook}"
let _ = test "L1" L1.props L1.parse L1.entails d1 [q1]


module Val2 : T = Option (Sum String Int)
module L2 : T = Multiset (Prod Atom Val2)
    (* plus attributes optionaly valued by strings and integers... *)

let d2 = "{title is \"The Jungle Book\", paper_back, year 1985}"
let q2 = "title contains \"Jungle\""
let q2' = "{paper_back, year 1985}"
let _ = test "L2" L2.props L2.parse L2.entails d2 [q2; q2']


module Val3 : T = Top (Option (Sum String (Interval Int)))
module L3 : T = Multiset (Prod Atom Val3)
    (* plus undetermined value, and intervals over integers... *)

let d3 = "{title is \"The Jungle Book\", paper_back, year = 1985}"
let q3 = "title ?"
let q3' = "{paper_back, year in 1980 .. 1990}"
let _ = test "L3" L3.props L3.parse L3.entails d3 [q3; q3']


module Val4 : T = Top (Option (Sum String (Interval Int)))
module L4 : T = PropExt (Multiset (Prod Atom Val4))
    (* plus boolean connectives inside and outside multisets... *)

let d4 = "{title is \"The Jungle Book\", paper_back, year = 1985}"
let q4 = "(title contains \"Jungle\" and not title contains \"Wood\") and paper_back and not (year in 1970 .. 1980 or year in 1990 .. 2000)"
let _ = test "L4" L4.props L4.parse L4.entails d4 [q4]


module Val5 : T = Top (Option (Sum String (Interval Int)))
module L5 : T = PropExt (Bottom (Multiset (Prod Atom Val5)))
    (* plus functor Bottom as a try to get the right properties... *)

let d5 = "{title is \"The Jungle Book\", paper_back, year = 1985}"
let _ = test "L5" L5.props L5.parse L5.entails d5 []


module Val6 : T = Top (Option (Sum String (Interval Int)))
module L6 : T = PropExt (Bottom (Single (Multiset (Prod Atom Val6))))
    (* plus functors Single and Bottom again as a try to get the right properties... *)

let d6 = "[{title is \"The Jungle Book\", paper_back, year = 1985}]"
let q6 = "(title contains \"Jungle\" and not title contains \"Wood\") and paper_back and not (year in 1970 .. 1980 or year in 1990 .. 2000))"
let _ = test "L6" L6.props L6.parse L6.entails d6 [q6]


(* interactive testing of a logic *)

(*
module LTester = Tester(L6)
let _ = LTester.main ()
*)
