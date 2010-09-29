(** Open intervals over values from the sub-logic. *)

open Token
open Logic

module type PARAM =
  sig
    val toks_bound : Syntax.t_list
    val toks_equal : Syntax.t_list
    val toks_pre : Syntax.t_list
    val toks_sep : Syntax.t_list
    val toks_suf : Syntax.t_list
  end

module Make (Param : PARAM) (Val : T) =
  struct
    module MyInterval =
      Interval.Make
	(struct
	  let toks_equal = Param.toks_equal
	  let toks_pre = Param.toks_pre
	  let toks_sep = Param.toks_sep
	  let toks_suf = Param.toks_suf
	end)

    module MyBounds =
      Top.Make
	(struct
	  let toks_top = Param.toks_bound
	  let feat_top = true
	end)

    module B = MyBounds(Val)
    module I = MyInterval(B)

    include I

    let top () = (B.Top, B.Top) (* I.parse (Syntax.of_list (Param.toks_pre @ Ident "min" :: Param.toks_sep @ Ident "max" :: Param.toks_suf)) *)

    let features f =
      let x = top () in
      (true,x) :: I.features f

    let min_bound = B.Top (* B.parse (Syntax.of_list [Ident "min"]) *)
    let max_bound = B.Top (* B.parse (Syntax.of_list [Ident "max"]) *)
    let val_bound v = B.Other v (* B.parse (Syntax.of_list (Ident "other" :: Val.print v)) *)

    let parse_interval = parser
      | [<_ = Syntax.parse_tokens (Param.toks_bound @ Param.toks_sep); str>] ->
	  begin match str with parser
	  | [<v = Val.parse>] -> (min_bound, val_bound v)
	  | [<_ = Syntax.parse_tokens Param.toks_bound>] -> (min_bound, max_bound) end
      | [<v1 = Val.parse; str1>] ->
	  let a = val_bound v1 in
	  begin match str1 with parser
	  | [<_ = Syntax.parse_tokens Param.toks_sep; str2>] ->
	      begin match str2 with parser
	      | [<v2 = Val.parse>] ->
		  let b = val_bound v2 in
		  if B.le_min a b && B.le_max a b
		  then (a,b)
		  else raise (Stream.Error "Syntax error: inconsistent interval")
	      | [<_ = Syntax.parse_tokens Param.toks_bound>] -> (a, max_bound) end
	  | [<>] -> (a, a) end

    let parse = parser
      | [<i = Syntax.parse_tokens_and Param.toks_pre parse_interval;
	  _ = Syntax.parse_tokens Param.toks_suf ?? "Syntax error: missing end of interval">] -> i
      | [<v = Syntax.parse_tokens_and Param.toks_equal Val.parse>] ->
	  let a = val_bound v in
	  (a, a)


    let print_bound = function
      | B.Top -> Param.toks_bound
      | B.Other v -> Val.print v
(*
      match B.print a with
      | [Ident "min"] -> Param.toks_min
      | [Ident "max"] -> Param.toks_max
      | Ident "other" :: toks -> toks
      | toks -> toks
*)

    let print (a,b) =
      if a <> B.Top && a = b
      then Param.toks_equal @ print_bound a
      else Param.toks_pre @ print_bound a @ Param.toks_sep @ print_bound b @ Param.toks_suf

  end

module DotDot =
  Make
    (struct
      let toks_bound = []
      let toks_equal = [Equal; PP_tilda]
      let toks_pre = [Ident "in"; PP_tilda]
      let toks_sep = [Dot; Dot]
      let toks_suf = []
    end)

module Brackets =
  Make
    (struct
      let toks_bound = []
      let toks_equal = [Equal; PP_tilda]
      let toks_pre = [Ident "in"; PP_tilda; LeftBra]
      let toks_sep = [Comma]
      let toks_suf = [RightBra]
    end)

