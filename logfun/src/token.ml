(** Tokens and utility functions. *)

exception Eof

type t =
    BackQuote | Tilda | Exclam | At | Sharp | Dollar | Percent | Hat | Et | Star
  | LeftPar | RightPar | Minus | Plus | Equal | LeftAcc | RightAcc | LeftBra | RightBra
  | Pipe | BackSlash | Slash | Interro | LT | GT | Comma | Dot | Colon | SemiColon
  | DoubleQuote | Quote
  | Ident of string
  | Nat of int
  | Char of char | String of string | Term of string
  | PP_tilda | PP_space | PP_cut | PP_break of int * int | PP_newline

(* predicate filtering PP_* tokens *)
let is_PP = function PP_tilda | PP_space | PP_cut | PP_break _ | PP_newline -> true | _ -> false

(* get the precision of a float number from its writing *)
let prec_of_sfloat : string -> int =
  fun s ->
      let l = String.length s in
      let i_dot =
        try String.index s '.'
        with Not_found -> try String.index s 'e' - 1
        with Not_found -> l - 1 in
      let i_e,p =
        try
          let i_e = String.index s 'e' in
          i_e,
          match s.[i_e+1] with
          | '+' -> int_of_string (String.sub s (i_e+2) (l-(i_e+2)))
          | _ -> int_of_string (String.sub s (i_e+1) (l-(i_e+1)))
        with Not_found -> l, 0 in
      p - (i_e - (i_dot+1))
