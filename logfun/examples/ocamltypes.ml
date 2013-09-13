
open Token

module MyOpt =
  Opt.Make
    (struct
      let toks_none = []
      let toks_some = []
    end)

module MyInterv =
  Interv.Make
    (struct
      let verbose = true
    end)

module MyProd =
  Prod.Make
    (struct
      let toks_suf = []
      let toks_pre = []
      let toks_sep = [PP_tilda]
    end)

module MySum =
  Sum.Make
    (struct
      let toks_top = None
      let toks_a1 = []
      let toks_a2 = []
    end)

module MySumNil =
  Sum.Make
    (struct
      let toks_top = Some []
      let toks_a1 = []
      let toks_a2 = []
    end)

module TypeConstr =
  Attr.Make
    (struct
      let toks_top = None
      let names x = true
    end)

module SumComplexType =
  Sum.Make
    (struct
      let toks_top = Some [Interro]
      let toks_a1 = [BackQuote]
      let toks_a2 = []
    end)
module TypeVar =
  Attr.Make
    (struct
      let toks_top = Some [Term "_"]
      let names x = true
    end)
module ProdBuilt =
  Prod.Make
    (struct
      let toks_suf = []
      let toks_pre = []
      let toks_sep = []
    end)
module VectorArgs =
  Vector.Make
    (struct
      let toks_begin = [PP_tilda; LeftPar]
      let toks_end = [RightPar]
      let toks_sep = [Comma; PP_tilda]
      let toks_empty = []
    end)
module ValType (Type : Logic.T) =
  SumComplexType
    (TypeVar)
    (ProdBuilt
       (TypeConstr)
       (VectorArgs(Type)))


module MultisetFun =
  Multiset.Make
    (struct
      let toks_begin = [LeftAcc]
      let toks_end = [RightAcc]
      let toks_sep = [Comma]
      let toks_empty = [LeftAcc; RightAcc]
    end)
module SumFun =
  Sum.Make
    (struct
      let toks_top = None
      let toks_a1 = [LT] (* arguments of functions *)
      let toks_a2 = [GT] (* results of functions *)
    end)
module FunType (Type : Logic.T) =
  struct
    module S = SumFun(Type)(Type)
    module F = MultisetFun(S)

    let rec parse_fun = parser
      | [<'LeftPar; t = parse_fun0; 'RightPar>] ->
	  match t with
	  | Comma::t' -> LeftAcc :: t'
	  | [RightAcc] -> [LeftAcc; RightAcc]
	  | _ -> raise (Stream.Error "Syntax error: invalid function description")
    and parse_fun0 = parser
      | [<'Minus; 'GT; str>] ->
	  (match str with parser
	  | [<t = parse_type; r = parse_rets>] -> Comma :: GT :: t @ r
	  | [<>] -> [RightAcc])
      | [<t = parse_fun1>] -> t
    and parse_fun1 = parser
(*
      | [<c = parse_c; t = parse_fun2 c>] -> Comma :: t
      | [<'Dot; 'Minus; 'GT; t = parse_fun1>] -> Comma :: LT :: Dot :: t
      | [<f = parse_fun; 'Minus; 'GT; t = parse_fun1>] -> Comma :: LT :: f @ t
*)
      | [<t0 = parse_type; t = parse_fun2 t0>] -> Comma :: t
      | [<>] -> [RightAcc]
    and parse_fun2 t0 = parser
      | [<'Star; t = parse_type; r = parse_rets>] -> GT :: t0 @ Comma :: GT :: t @ r
      | [<'Minus; 'GT; t = parse_fun1>] -> LT :: t0 @ t
      | [<>] -> GT :: t0 @ [RightAcc]
    and parse_rets = parser
      | [<'Star; t = parse_type; r = parse_rets>] -> Comma :: GT :: t @ r
      | [<>] -> [RightAcc]
    and parse_type = parser
      | [<t = Type.parse>] -> Type.print t

    let print_fun t =
      let args, rets =
	List.fold_left
	  (fun (args,rets) -> function
	    | (n, S.Top) -> (args, rets)
	    | (n, S.A1 a) -> (Common.fold_for (fun _ res -> a::res) 1 n args, rets)
	    | (n, S.A2 r) -> (args, Common.fold_for (fun _ res -> r::res) 1 n rets))
	  ([],[])
	  t in
      let toks_rets =
	match rets with
	| [] -> [RightPar]
	| r::rs -> List.fold_left (fun res r' -> Type.print r' @ PP_tilda :: Star :: PP_tilda :: res) (Type.print r @ [RightPar]) rs in
      LeftPar ::
      if args = []
      then
	PP_tilda :: Minus :: GT :: PP_tilda :: toks_rets
      else
	List.fold_left
	  (fun res a -> Type.print a @ PP_tilda :: Minus :: GT :: PP_tilda :: res)
	  toks_rets
	  args

    include F
    let parse = parser
      | [<f = F.parse>] -> f
      | [<toks = parse_fun>] -> F.parse (Syntax.of_list toks)
    let print = print_fun
  end

module SumType =
  Sum.Make
    (struct
      let toks_top = None
      let toks_a1 = []
      let toks_a2 = []
    end)
module rec Type : Logic.T =
  SumType
    (ValType(Type))
    (FunType(Type))


module Plugins =
  struct
    let parse : string -> Type.t =
      fun s ->
	Type.parse (Syntax.from_string s)

    let entails : string -> string -> bool =
      fun s1 s2 ->
	Type.entails (parse s1) (parse s2)

    let isvalue : string -> bool =
      fun s ->
	Type.isvalue (parse s)
  end


(** Small program prompting the user to enter formulas, and answering whether this is syntactically correct. *)

let _ =
  try
    while true do
      print_endline "\nEnter a formula (or 'exit' to quit):";
      let line = read_line () in
      if line = "exit" then raise End_of_file;
      try
        match Syntax.from_string line with parser
        | [<_ = Type.parse; _ = Stream.empty>] -> print_endline "OK"
      with Stream.Error msg -> print_endline (if msg="" then "Syntax error" else msg)
    done
  with
  | End_of_file -> print_endline "Good bye"
  | e -> print_endline (Printexc.to_string e)
