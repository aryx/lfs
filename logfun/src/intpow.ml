(** Integer numbers with rounding.

   E.g., 134 entails 13* entails 1** entails 0***.

*)

(*
module type T =
  sig
    include Logic.T
    module Diff : Logic.T
    val diff : t -> t -> Diff.t
  end
*)

module Make =
  struct
    include Logic.Default

    let st' () = true
    let cs_entails () = true
    let cp'_entails () = true
    let cp_top () = true

    type t = int * int  (* (n,p) = [n*10^p, (n+1)*10^p[, interval *)

    let rec power10 : int -> int =
      function
      | 0 -> 1
      | p -> 10 * power10 (p-1)

    let get_min : t -> int = fun (n,p) -> n * power10 p

    let get_max : t -> int = fun (n,p) -> (n+1) * power10 p

    let compare x1 x2 =
      if get_max x1 <= get_min x2 then -1
      else if get_min x1 >= get_max x2 then 1
      else 0

    let entails x1 x2 = get_min x1 >= get_min x2 & get_max x1 <= get_max x2

    let conj x1 x2 =
      if entails x1 x2 then x1
      else if entails x2 x1 then x2
      else raise Not_found

    let add x1 x2 = conj x1 x2

    let sub x1 x2 = if entails x1 x2 then raise Not_found else x1

    let rec features = function
      | (0,p) -> [(true,(0,p))]
      |	(n,p) -> (true,(n,p)) :: features (n/10,p+1)

    let gen x1 x2 xs = []

    open Token

    let rec parse_zeros = parser
      | [<'Star; p = parse_zeros>] -> p+1
      | [<>] -> 0

    let parse = parser
      |	[<n = Syntax.parse_int; p = parse_zeros>] -> (n,p)

    let rec print_zeros = function
      | 0 -> []
      | p -> Star :: print_zeros (p-1)

    let rec print = function
      |	(n,p) -> Syntax.toks_of_int n @ print_zeros p

    let simpl (n,p) =
      let n' = n / 10 in
      if n' = 0
      then [<>]
      else [<'(n',p+1)>]

    module Diff =
      struct
        type t = int * int
        let compare = compare
        let top = top
        let bot = bot
        let entails = entails
        let conj = conj
        let add = add
        let sub = sub
        let features = features
        let gen = gen
        let parse = parse
        let print = print
        let simpl = simpl
      end

    let diff x1 x2 = (get_max x1 - get_min x2, 0)

  end
