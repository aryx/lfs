(***********************************************************************)
(*                                                                     *)
(*            The "agrep" library for Objective Caml                   *)
(*                                                                     *)
(*         Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id: agrep.ml,v 1.2 2002/02/02 09:29:01 xleroy Exp $ *)

type bitmatrix
external new_bitmatrix : int -> int -> bitmatrix
                       = "caml_agrep_new_bitmatrix"
external set_bit : bitmatrix -> int -> int -> int -> unit
                 = "caml_agrep_set_bit"

(* Deep syntax for patterns *)

type pattern =
    CBase of int * bitmatrix
  | CAnd of pattern * pattern
  | COr of pattern * pattern

(* String matching *)

external match_: string -> int -> int -> int -> bitmatrix -> int -> bool -> int
               = "caml_agrep_match_bytecode" "caml_agrep_match"

let string_match pat ?(numerrs = 0) ?(wholeword = false) text =
  if numerrs < 0 then invalid_arg "Agrep.string_match";
  let rec do_match = function
    CBase(len, bm) ->
      match_ text 0 (String.length text) len bm numerrs wholeword < max_int
  | CAnd(p1, p2) ->
      do_match p1 && do_match p2
  | COr(p1, p2) ->
      do_match p1 || do_match p2
  in do_match pat

let substring_match pat ?(numerrs = 0) ?(wholeword = false) text ~pos ~len =
  if pos < 0 || pos + len > String.length text
  then invalid_arg "Agrep.substring_match";
  if numerrs < 0 then invalid_arg "Agrep.substring_match";
  let rec do_match = function
    CBase(plen, bm) ->
      match_ text pos len plen bm numerrs wholeword < max_int
  | CAnd(p1, p2) ->
      do_match p1 && do_match p2
  | COr(p1, p2) ->
      do_match p1 || do_match p2
  in do_match pat

let errors_substring_match pat ?(numerrs = 0) ?(wholeword = false)
                           text ~pos ~len =
  if pos < 0 || pos + len > String.length text
  then invalid_arg "Agrep.errors_substring_match";
  if numerrs < 0 then invalid_arg "Agrep.errors_substring_match";
  let rec do_match = function
    CBase(plen, bm) ->
      match_ text pos len plen bm numerrs wholeword
  | CAnd(p1, p2) ->
      max (do_match p1) (do_match p2)
  | COr(p1, p2) ->
      min (do_match p1) (do_match p2)
  in do_match pat

(* Representation of character sets *)

module Charset =
  struct
    type t = string (* of length 32 *)

    let new_empty () = String.make 32 '\000'

    let all = String.make 32 '\255'

    let add s c =
      let i = Char.code c in
      s.[i lsr 3] <- Char.chr(Char.code s.[i lsr 3] lor (1 lsl (i land 7)))

    let add_range s c1 c2 =
      for i = Char.code c1 to Char.code c2 do add s (Char.chr i) done

    let complement s =
      let r = String.create 32 in
      for i = 0 to 31 do
        r.[i] <- Char.chr(Char.code s.[i] lxor 0xFF)
      done;
      r

    let iter fn s =
      for i = 0 to 31 do
        let c = Char.code s.[i] in
        for j = 0 to 7 do
          if c land (1 lsl j) <> 0 then fn (Char.chr ((i lsl 3) lor j))
        done
      done
end

(* Shallow syntax for patterns *)

type simple_pattern =
    Char of char
  | String of string
  | Char_class of Charset.t
  | Wildcard

type complex_pattern =
    Simple of simple_pattern list
  | And of complex_pattern * complex_pattern
  | Or of complex_pattern * complex_pattern

(* Compilation of shallow syntax into deep syntax *)

let add_char transl bm len c r =
  match transl with
    None ->
      set_bit bm len (Char.code c) r
  | Some tr ->
      let t = tr.[Char.code c] in
      for i = 0 to 255 do
        if tr.[i] = t then set_bit bm len i r
      done

let simple_pattern_len sp =
  List.fold_left
    (fun len p ->
      match p with
        Char c -> 1 + len
      | String s -> String.length s + len
      | Char_class s -> 1 + len
      | Wildcard -> len)
    0 sp

let compile_simple_pattern transl sp =
  let len = simple_pattern_len sp in
  let bm = new_bitmatrix len 257 in
  let rec fill pos = function
    [] -> ()
  | Char c :: rem ->
      add_char transl bm len c pos;
      fill (pos + 1) rem
  | String s :: rem ->
      for i = 0 to String.length s - 1 do
        add_char transl bm len s.[i] (pos + i)
      done;
      fill (pos + String.length s) rem
  | Char_class cls :: rem ->
      Charset.iter (fun c -> add_char transl bm len c pos) cls;
      fill (pos + 1) rem
  | Wildcard :: rem ->
      set_bit bm len 256 pos;
      fill pos rem in
  fill 0 sp;
  CBase(len, bm)

let rec compile_pattern ?transl = function
    Simple sp -> compile_simple_pattern transl sp
  | And(p1, p2) -> CAnd(compile_pattern ?transl p1, compile_pattern ?transl p2)
  | Or(p1, p2) -> COr(compile_pattern ?transl p1, compile_pattern ?transl p2)

(* From concrete syntax to shallow abstract syntax *)

exception Syntax_error of int

let parse_pattern s =

  let rec parse_or i =
    let (p1, i1) = parse_and i in
    parse_ors p1 i1

  and parse_ors p1 i =
    if i >= String.length s then (p1, i) else
      match s.[i] with
        ')' -> (p1, i)
      | '|' -> let (p2, i2) = parse_and (i + 1) in parse_ors (Or(p1, p2)) i2
      |  _  -> raise (Syntax_error i)

  and parse_and i =
    let (p1, i1) = parse_base i in
    parse_ands p1 i1

  and parse_ands p1 i =
    if i >= String.length s then (p1, i) else
      match s.[i] with
        ')' | '|' -> (p1, i)
      | '&' -> let (p2, i2) = parse_base (i + 1) in parse_ands (And(p1, p2)) i2
      |  _  -> raise (Syntax_error i)

  and parse_base i =
    if i >= String.length s then (Simple [], i) else
      match s.[i] with
        ')' | '|' | '&' ->
          (Simple [], i)
      | '(' ->
          let (p, j) = parse_or (i + 1) in
          if j >= String.length s || s.[j] <> ')' then raise (Syntax_error j);
          (p, j + 1)
      |  _ ->
          let (sl, j) = parse_simple_list [] i in
          (Simple (List.rev sl), j)

  and parse_simple_list sl i =
    if i >= String.length s then (sl, i) else
      match s.[i] with
        ')' | '&' | '|' ->
          (sl, i)
      | '(' ->
          raise (Syntax_error i)
      | '?' ->
          parse_simple_list (Char_class Charset.all :: sl) (i + 1)
      | '*' ->
          parse_simple_list (Wildcard :: sl) (i + 1)
      | '\\' when i + 1 < String.length s ->
          parse_simple_list (Char s.[i+1] :: sl) (i + 2)
      | '[' ->
          let (cls, i1) = parse_char_class (i + 1) in
          parse_simple_list (Char_class cls :: sl) i1
      | c ->
          parse_simple_list (Char c :: sl) (i + 1)

  and parse_char_class i =
    let cls = Charset.new_empty() in
    if i < String.length s && s.[i] = '^' then begin
      let j = parse_class cls (i+1) in
      (Charset.complement cls, j)
    end else begin
      let j = parse_class cls i in
      (cls, j)
    end

  and parse_class cls i =
    if i >= String.length s then raise (Syntax_error i)
    else if s.[i] = ']' then i + 1
    else if s.[i] = '\\' && i + 1 < String.length s then
      (Charset.add cls s.[i+1];
       parse_class cls (i+2))
    else if i + 2 < String.length s && s.[i+1] = '-' && s.[i+2] <> ']' then
      (Charset.add_range cls s.[i] s.[i+2];
       parse_class cls (i+3))
    else
      (Charset.add cls s.[i];
      parse_class cls (i+1))

  in
    let (p, i) = parse_or 0 in
    assert (i = String.length s);
    p

(* All together *)

let pattern ?transl s = compile_pattern ?transl (parse_pattern s)

let pattern_string ?transl s = compile_pattern ?transl (Simple[String s])

(* Translation tables for ISO 8859-15 (Latin 1 with Euro) *)

module Iso8859_15 =
  struct
    let case_insensitive =
"\000\001\002\003\004\005\006\007\008\t\n\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159 ¡¢£¤¥¨§¨©ª«¬­®¯°±²³¸µ¶·¸¹º»½½ÿ¿àáâãäåæçèéêëìíîïðñòóôõö×øùúûüýþßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"

    let accent_insensitive =
"\000\001\002\003\004\005\006\007\008\t\n\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159 ¡¢£¤¥S§s©ª«¬­®¯°±²³Zµ¶·z¹º»OoY¿AAAAAAACEEEEIIIIÐNOOOOO×OUUUUYÞsaaaaaaaceeeeiiiiðnooooo÷ouuuuyþy"

    let case_and_accent_insensitive =
"\000\001\002\003\004\005\006\007\008\t\n\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159 ¡¢£¤¥s§s©ª«¬­®¯°±²³zµ¶·z¹º»ooy¿aaaaaaaceeeeiiiiðnooooo×ouuuuyþsaaaaaaaceeeeiiiiðnooooo÷ouuuuyþy"
  end
