(** Float numbers with precision. *)

module Make =
  struct
    include Logic.Default

    type t = float * int  (* (f,p) = [f, f+1x10^p[, interval *)

    let rec power10 : int -> float =
      function
      | 0 -> 1.
      | p -> if p > 0
             then power10 (p-1) *. 10.
             else power10 (p+1) /. 10.

    let delta = 5. /. 9.

    let get_min : t -> float = fun (f,p) -> f               (*-. (delta *. (power10 p))*)

    let get_max : t -> float = fun (f,p) -> f +. power10 p  (* delta *. (power10 p) *)

    let compare x1 x2 =
      if get_max x1 <= get_min x2 then -1
      else if get_min x1 >= get_max x2 then 1
      else 0

    let entails (f1,p1) (f2,p2) = p1 <= p2 & f1 >= get_min (f2,p2) & f1 <= get_max (f2,p2)
    (* let entails x1 x2 = get_min x1 >= get_min x2 & get_max x1 <= get_max x2 *)

    let conj x1 x2 =
      if entails x1 x2 then x1
      else if entails x2 x1 then x2
      else raise Not_found

    let add x1 x2 = conj x1 x2

    let sub x1 x2 = if entails x1 x2 then raise Not_found else x1

    let rec features = function
      |	(0.,p) -> [(true,(0.,p))]
      | (f,p) ->
           let f' =
             if f >= 0.
             then floor (f /. power10 (p+1)) *. power10 (p+1)
             else ceil (f /. power10 (p+1)) *. power10 (p+1) in
           (true,(f,p)) :: features (f', p+1)

    let gen x1 x2 xs = []

    open Token

    let parse = parser
      |	[<(f,p) = Syntax.parse_float>] -> (f,p)

    let rec print = function
      |	(f,p) -> Syntax.toks_of_float (f,p)

    let simpl (f,p) =
      let f' =
        if f >= 0.
        then floor (f /. power10 (p+1)) *. power10 (p+1)
        else ceil (f /. power10 (p+1)) *. power10 (p+1) in
      if f' = 0.
      then [<>]
      else [<'(f',p+1)>]

  end
