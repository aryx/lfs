
open Token
open Logic

let toks_bot = [Nat 0]

module MyTop =
  Top.Make
    (struct
      let toks_top = []
      let feat_top = true
    end)

module MyProd =
  Prod.Make
    (struct
      let toks_suf = []
      let toks_sep = []
      let toks_pre = []
      let toks_bot = toks_bot
    end)

module MySum =
  Sum.Make
    (struct
      let toks_a1 = []
      let toks_a2 = []
      let toks_bot = toks_bot
    end)

module MyBottom =
  Bottom.Make
    (struct
      let toks_bot = toks_bot
    end)

module SingleObject =
  Single.Make
    (struct
      let toks_K = [Interro]
      let toks_O = []
      let toks_O_end = []
    end)

module UnitArray =
  Unit.Make
    (struct
      let toks = [LeftBra; RightBra]
    end)

module PrelistArray =
  Prelist.Make
    (struct
      let toks_top = [Star]
      let toks_nil = []
      let toks_sep = []
      let toks_bot = toks_bot
      let feat_top = true
    end)

module TopAtom =
  Top.Make
    (struct
      let toks_top = []
      let feat_top = true
    end)

module PrefixName = Prefix.Make (struct let toks = [Ident "name"; PP_tilda] end)
module PrefixIn = Prefix.Make (struct let toks = [Ident "param"; PP_tilda] end)
module PrefixOut = Prefix.Make (struct let toks = [Ident "return"; PP_tilda] end)
module PrefixClass = Prefix.Make (struct let toks = [Ident "class"; PP_tilda] end)
module PrefixThrows = Prefix.Make (struct let toks = [Ident "throws"; PP_tilda] end)

module MultisetParam =
  Multiset.Make
    (struct
      let toks_begin = [LeftPar]
      let toks_end = [RightPar]
      let toks_sep = [Comma]
      let toks_empty = [LeftPar; RightPar]
      let no_begin_end_when_singleton = true
    end)


(* -------------------------------- *)

module Class =
  struct
    module M =
      Attr.Make
	(struct
	  let names x = not (List.mem x ["void"; "char"; "byte"; "short"; "int"; "long"; "float"; "double"; "boolean"; "integer"; "real"; "primitive"])
	  let toks_top = Some [Term "Object"]
	end)

    include M

    let rec parse_path = parser
      | [<'Term p; path = Syntax.parse_opt (parser [<'Dot; path = parse_path>] -> "." ^ path) "">] -> p ^ path
      | [<'Ident p; 'Dot; path = parse_path>] -> p ^ "." ^ path

    let parse str =
      parse (Syntax.of_list [Term (parse_path str)])

    let print f =
      match print f with
      | [Term path] -> Syntax.list_of_stream (Syntax.from_string path)
      | _ -> assert false

  end

module Primitive =
  Taxo.Make
    (struct
      let formulas = [
	(1,([Ident "void"],[12]));
	(2,([Ident "char"],[12]));
	(3,([Ident "byte"],[10]));
	(4,([Ident "short"],[10]));
	(5,([Ident "int"],[10]));
	(6,([Ident "long"],[10]));
	(7,([Ident "float"],[11]));
	(8,([Ident "double"],[11]));
	(9,([Ident "boolean"],[12]));
	(10,([Ident "integer"],[12]));
	(11,([Ident "real"],[12]));
	(12,([Ident "primitive"],[]))
      ]
    end)

module Object =
  MyBottom (SingleObject(Class))

module Atom =
  TopAtom (MySum(Primitive)(Object))

module Array =
  MyProd
    (Atom)
    (PrelistArray(UnitArray))

module Kind =
  Taxo.Make
    (struct
      let formulas = [
	(1,([Ident "method"],[]));
	(2,([Ident "constructor"],[]))
      ]
    end)

module Name =
  Substring.Verb
    (struct
      let normalize_is n = n
      let normalize n = n
      let words n =
	let len = String.length n in
	let l = ref [] in
	let start = ref 0 in
	for i = 0 to len - 1 do
	  match n.[i] with
	  | 'A' .. 'Z' ->
	      l := (true, String.sub n !start (i - !start)) :: !l;
	      start := i
	  | _ -> ()
	done;
	l := (true, String.sub n !start (len - !start)) :: !l;
	!l
    end)

module Access =
  Taxo.Make
    (struct
      let formulas = [
	(1,([Ident "public"],[5]));
	(2,([Ident "protected"],[5]));
	(3,([Ident "private"],[5]));
	(4,([Ident "package_private"],[5]));
	(5,([Ident "access"],[]))
      ]
    end)

module Modifier =
  Taxo.Make
    (struct
      let formulas = [
	(1,([Ident "abstract"],[6]));
	(2,([Ident "static"],[6]));
	(3,([Ident "final"],[6]));
	(4,([Ident "native"],[6]));
	(5,([Ident "synchronized"],[6]));
	(6,([Ident "modifier"],[]))
      ]	
    end)

module Make =
  MyTop (MySum
    (MySum
       (MySum
	  (Kind)
	  (PrefixName(Name)))
       (MySum
	  (Access)
	  (Modifier)))
    (MySum
       (MySum
	  (PrefixIn (MultisetParam(Array)))
	  (PrefixOut(Array)))
       (MySum
	  (PrefixClass(Object))
	  (PrefixThrows(Object)))))

