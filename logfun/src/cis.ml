(**
   Cis : compact integer sets

   This module implements compact integer sets, represented as a (custom) list
   of integer intervals. Usual set operations are provided.
   The advantage compared to ordered lists is that the actual size may be smaller
   than the cardinal of a set when many elements are contiguous. Most set operations
   are linear w.r.t. the size, not the cardinal.

   Author: Sébastien Ferré <ferre@irisa.fr>
   License: LGPL
*)

(* for test
#load "nums.cma"
#load "str.cma"
#load "unix.cma"
#load "common.cmo"
#load "lSet.cmo"
*)

(* copied from module Common *)

let fold_for : (int -> 'a -> 'a) -> int -> int -> 'a -> 'a =
  fun f a b e ->
    let res = ref e in
    for x = a to b do
      res := f x !res
    done;
    !res

let fold_for_down : (int -> 'a -> 'a) -> int -> int -> 'a -> 'a =
  fun f a b e ->
    let res = ref e in
    for x = a downto b do
      res := f x !res
    done;
    !res

(* end of copy *)

type elt = int

type t = Nil | Single of int * t | Interv of int * int * t
      (** integers in decreasing order *)

let compare x y = Pervasives.compare y x

let max_elt : t -> int =
  function
    | Nil -> raise (Invalid_argument "Cis.max_elt: set is empty")
    | Single (x,_) -> x
    | Interv (xmax,_,_) -> xmax

let rec min_elt : t -> int =
  function
    | Nil -> raise (Invalid_argument "Cis.max: set is empty")
    | Single (x,Nil) -> x
    | Interv (_,xmin,Nil) -> xmin
    | Single (_,l) -> min_elt l
    | Interv (_,_,l) -> min_elt l

let step : t -> nil:(unit -> 'a) -> single:(int -> t -> 'a) -> interv:(int * int -> t -> 'a) -> 'a =
  fun l ~nil ~single ~interv ->
    match l with
    | Nil -> nil ()
    | Single (x,l') -> single x l'
    | Interv (x,y,l') -> interv (x,y) l';;

let cons_single : int -> t -> t =
  fun x l ->
    step l
      ~nil:(fun () -> Single (x,Nil))
      ~single:(fun x' l' -> (* assert (x > x');*) if x=x'+1 then Interv (x,x',l') else Single (x,l))
      ~interv:(fun (xmax',xmin') l' -> (* assert (x > xmax');*) if x=xmax'+1 then Interv (x,xmin',l') else Single (x,l));;

let cons_interv : int * int -> t -> t =
  fun (xmax,xmin) l ->
    if xmax > xmin then
      step l
	~nil:(fun () -> Interv (xmax, xmin, Nil))
	~single:(fun x' l' -> (* assert (xmin > x');*) if xmin=x'+1 then Interv (xmax,x',l') else Interv (xmax,xmin,l))
	~interv:(fun (xmax',xmin') l' -> (* assert (xmin > xmax');*) if xmin=xmax'+1 then Interv (xmax, xmin', l') else Interv (xmax,xmin,l))
    else if xmin=xmax then (* inlining of 'cons_single xmin l' *)
      step l
	~nil:(fun () -> Single (xmin,Nil))
	~single:(fun x' l' -> (* assert (xmin > x');*) if xmin=x'+1 then Interv (xmin,x',l') else Single (xmin,l))
	~interv:(fun (xmax',xmin') l' -> (* assert (xmin > xmax');*) if xmin=xmax'+1 then Interv (xmin,xmin',l') else Single (xmin,l))
    else (* xmin > xmax *) l;;

let rec append : t -> t -> t = (* assumes (min_elt l1) > (max_elt l2) *)
  fun l1 l2 ->
    if l2 = Nil
    then l1
    else
      let m = max_elt l2 in
      append_aux l1 (m,l2)
and append_aux l1 (m,l2) =
  match l1 with
  | Nil -> l2
  | Single (x,Nil) -> if x=m+1 then cons_single x l2 else Single (x,l2)
  | Interv (xmax,xmin,Nil) -> if xmin=m+1 then cons_interv (xmax,xmin) l2 else Interv (xmax,xmin,l2)
  | Single (x,l') -> Single (x, append_aux l' (m,l2))
  | Interv (xmax,xmin,l') -> Interv (xmax, xmin, append_aux l' (m,l2))

(* -------------------------- *)

let empty : t = Nil;;

let is_empty : t -> bool =
  fun l -> l = Nil

let rec cardinal : t -> int =
  fun l -> cardinal_aux 0 l
and cardinal_aux accu l =
  step l
    ~nil:(fun () -> accu)
    ~single:(fun x l' -> cardinal_aux (accu+1) l')
    ~interv:(fun (xmax,xmin) l' -> cardinal_aux (accu+xmax-xmin+1) l');;

let rec mem : elt -> t -> bool =
  fun e l ->
    step l
      ~nil:(fun () -> false)
      ~single:(fun x l_tail ->
	e=x or (pred x > e & mem e l_tail))
      ~interv:(fun (xmax,xmin) l_tail ->
	(xmax >= e & e >= xmin) or (pred xmin > e & mem e l_tail));;

let singleton : elt -> t =
  fun x -> Single (x,Nil)

let rec add : int -> t -> t =
  fun x l ->
    step l
      ~nil:(fun () -> cons_single x l)
      ~single:(fun x' l' ->
	if x > x' then cons_single x l
	else if x = x' then l
	else (* x' > x *) cons_single x' (add x l'))
      ~interv:(fun (xmax',xmin') l' ->
	if x > xmax' then cons_single x l
	else if xmax' >= x & x >= xmin' then l
	else (* xmin' > x *) cons_interv (xmax',xmin') (add x l'))

let rec remove : int -> t -> t =
  fun x l ->
    step l
      ~nil:(fun () -> empty)
      ~single:(fun x' l' ->
	if x > x' then l
	else if x = x' then l'
	else (* x' > x *) cons_single x' (remove x l'))
      ~interv:(fun (xmax',xmin') l' ->
	if x > xmax' then l
	else if xmax' >= x & x >= xmin' then cons_interv (xmax',x+1) (cons_interv (x-1,xmin') l')
	else cons_interv (xmax',xmin') (remove x l'));;

let of_list : int list -> t =
  fun l ->
    List.fold_left (fun res x -> add x res) empty l;;

let rec union : t -> t -> t =
  fun l1 l2 ->
    step l1
      ~nil:(fun () -> l2)
      ~single:(fun x1 l1_tail ->
	step l2
	  ~nil:(fun () -> l1)
	  ~single:(fun x2 l2_tail ->
	    if x1 > 1+x2 then cons_single x1 (union l1_tail l2)
	    else if x2 > 1+x1 then cons_single x2 (union l1 l2_tail)
	    else if x1 = 1+x2 then cons_interv (x1,x2) (union l1_tail l2_tail)
	    else if x2 = 1+x1 then cons_interv (x2,x1) (union l1_tail l2_tail)
	    else (* x1=x2 *) cons_single x1 (union l1_tail l2_tail))
	  ~interv:(fun (xmax2,xmin2) l2_tail ->
	    if x1 > xmax2 then cons_single x1 (union l1_tail l2)
	    else if xmin2 > 1+x1 then cons_interv (xmax2,xmin2) (union l1 l2_tail)
	    else if xmin2 = 1+x1 then cons_interv (xmax2,x1) (union l1_tail l2_tail)
	    else (* xmax2 >= x1 & x1 >= xmin2 *) cons_interv (xmax2,x1) (union l1_tail (cons_interv (x1-1,xmin2) l2_tail))))
      ~interv:(fun (xmax1,xmin1) l1_tail ->
	step l2
	  ~nil:(fun () -> l1)
	  ~single:(fun x2 l2_tail ->
	    if x2 > xmax1 then cons_single x2 (union l1 l2_tail)
	    else if xmin1 > 1+x2 then cons_interv (xmax1,xmin1) (union l1_tail l2)
	    else if xmin1 = 1+x2 then cons_interv (xmax1,x2) (union l1_tail l2_tail)
	    else (* xmax1 >= x2 & x2 >= xmin1 *) cons_interv (xmax1,x2) (union (cons_interv (x2-1,xmin1) l1_tail) l2_tail))
	  ~interv:(fun (xmax2,xmin2) l2_tail ->
	    if xmin2 > xmax1 then cons_interv (xmax2,xmin2) (union l1 l2_tail)
	    else if xmin1 > xmax2 then cons_interv (xmax1,xmin1) (union l1_tail l2)
	    else
	      cons_interv
		(max xmax1 xmax2, max xmin1 xmin2)
		(if xmin1 = xmin2 then union l1_tail l2_tail
		else if xmin1 > xmin2 then union l1_tail (cons_interv (xmin1-1,xmin2) l2_tail)
		else (* xmin2 > xmin1 *) union (cons_interv (xmin2-1,xmin1) l1_tail) l2_tail)));;

let rec inter : t -> t -> t =
  fun l1 l2 ->
    step l1
      ~nil:(fun () -> empty)
      ~single:(fun x1 l1_tail ->
	step l2
	  ~nil:(fun () -> empty)
	  ~single:(fun x2 l2_tail ->
	    if x1 > 1+x2 then inter l1_tail l2
	    else if x2 > 1+x1 then inter l1 l2_tail
	    else if x1 = 1+x2 then inter l1_tail l2_tail
	    else if x2 = 1+x1 then inter l1_tail l2_tail
	    else (* x1=x2 *) cons_single x1 (inter l1_tail l2_tail))
	  ~interv:(fun (xmax2,xmin2) l2_tail ->
	    if x1 > xmax2 then inter l1_tail l2
	    else if xmin2 > x1 then inter l1 l2_tail
	    else (* xmax2 >= x1 & x1 >= xmin2 *) cons_single x1 (inter l1_tail l2)))
      ~interv:(fun (xmax1,xmin1) l1_tail ->
	step l2
	  ~nil:(fun () -> empty)
	  ~single:(fun x2 l2_tail ->
	    if x2 > xmax1 then inter l1 l2_tail
	    else if xmin1 > x2 then inter l1_tail l2
	    else (* xmax1 >= x2 & x2 >= xmin1 *) cons_single x2 (inter l1 l2_tail))
	  ~interv:(fun (xmax2,xmin2) l2_tail ->
	    if xmin2 > xmax1 then inter l1 l2_tail
	    else if xmin1 > xmax2 then inter l1_tail l2
	    else
	      cons_interv
		(min xmax1 xmax2, max xmin1 xmin2)
		(if xmin1 >= xmin2 then inter l1_tail l2 else inter l1 l2_tail)));;

let rec diff : t -> t -> t =
  fun l1 l2 ->
    step l1
      ~nil:(fun () -> empty)
      ~single:(fun x1 l1_tail ->
	step l2
	  ~nil:(fun () -> l1)
	  ~single:(fun x2 l2_tail ->
	    if x1 > x2 then cons_single x1 (diff l1_tail l2)
	    else if x2 > x1 then diff l1 l2_tail
	    else (* x1=x2 *) diff l1_tail l2_tail)
	  ~interv:(fun (xmax2,xmin2) l2_tail ->
	    if x1 > xmax2 then cons_single x1 (diff l1_tail l2)
	    else if xmin2 > x1 then diff l1 l2_tail
	    else (* xmax2 >= x1 & x1 >= xmin2 *) diff l1_tail l2))
      ~interv:(fun (xmax1,xmin1) l1_tail ->
	step l2
	  ~nil:(fun () -> l1)
	  ~single:(fun x2 l2_tail ->
	    if x2 > xmax1 then diff l1 l2_tail
	    else if xmin1 > x2 then cons_interv (xmax1,xmin1) (diff l1_tail l2)
	    else (* xmax1 >= x2 & x2 >= xmin1 *) cons_interv (xmax1,x2+1) (diff (cons_interv (x2-1,xmin1) l1_tail) l2_tail))
	  ~interv:(fun (xmax2,xmin2) l2_tail ->
	    if xmin2 > xmax1 then diff l1 l2_tail
	    else if xmin1 > xmax2 then cons_interv (xmax1,xmin1) (diff l1_tail l2)
	    else
	      cons_interv
		(xmax1,xmax2+1)
		(if xmin1 >= xmin2
		then diff l1_tail l2
		else diff (cons_interv (xmin2-1,xmin1) l1_tail) l2_tail)));;


let rec subset : t -> t -> bool =
  fun l1 l2 ->
    step l1
      ~nil:(fun () -> true)
      ~single:(fun x1 l1_tail ->
	step l2
	  ~nil:(fun () -> false)
	  ~single:(fun x2 l2_tail ->
	    if x1 > x2 then false
	    else if x2 > x1 then subset l1 l2_tail
	    else (* x1=x2 *) subset l1_tail l2_tail)
	  ~interv:(fun (xmax2,xmin2) l2_tail ->
	    if x1 > xmax2 then false
	    else if xmin2 > x1 then subset l1 l2_tail
	    else (* xmax2 >= x1 & x1 >= xmin2 *) subset l1_tail l2))
      ~interv:(fun (xmax1,xmin1) l1_tail ->
	step l2
	  ~nil:(fun () -> false)
	  ~single:(fun x2 l2_tail ->
	    if x2 > xmax1 then subset l1 l2_tail
	    else if xmin1 > x2 then false
	    else (* xmax1 >= x2 & x2 >= xmin1 *) false)
	  ~interv:(fun (xmax2,xmin2) l2_tail ->
	    if xmin2 > xmax1 then subset l1 l2_tail
	    else if xmin1 > xmax2 then false
	    else xmax2 >= xmax1 & xmin1 >= xmin2 & subset l1_tail l2));;


let equal = (=)

let rec iter : (elt -> unit) -> t -> unit =
  fun proc l ->
    step l
      ~nil:(fun () -> ())
      ~single:(fun x l_tail -> proc x; iter proc l_tail)
      ~interv:(fun (xmax,xmin) l_tail ->
	for x = xmax downto xmin do proc x done;
	iter proc l_tail);;

let rec fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a =
  fun f e l ->
    step l
      ~nil:(fun () -> e)
      ~single:(fun x l_tail ->
	fold_left f (f e x) l_tail)
      ~interv:(fun (xmax,xmin) l_tail ->
	fold_left f (fold_for_down (fun x res -> f res x) xmax xmin e) l_tail);;

let rec fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a =
  fun f l e ->
    step l
      ~nil:(fun () -> e)
      ~single:(fun x l_tail ->
	f x (fold_right f l_tail e))
      ~interv:(fun (xmax,xmin) l_tail ->
	fold_for (fun x res -> f x res) xmin xmax (fold_right f l_tail e));;

let rec elements : t -> elt list =
  fun l ->
    List.rev (fold_left (fun res x -> x::res) [] l);;


(* test section *)
(*

module OrdInt =
  struct
    type t = int
    let compare = compare
    let succ = succ
    let pred = pred
    let size x y = y - x + 1
  end

module OSet = Set.Make(OrdInt)

let set_of_list l =
  List.fold_left (fun res x -> OSet.add x res) OSet.empty l;;

let rec to_lset : t -> int LSet.t =
  fun l ->
    fold (fun res x -> LSet.add x res) (LSet.empty ()) l

let print_lset l =
  List.iter (fun x -> print_int x; print_string " ") l;
  print_newline ()

let print_prof s =
  try
    let n, t, m = Hashtbl.find Common.tbl_prof s in
    print_int n; print_string "\t";
    print_float t; print_string "\t";
    print_float m; print_string "\n"
  with _ -> print_endline (s ^ " cannot be found in profiling")

let rec random_list range =
  function
    | 0 -> []
    | len ->
	let x = 1 + Random.int range in
	x::random_list range (len-1)

let rec test range len1 len2 n =
  Hashtbl.clear Common.tbl_prof;
  for i = 1 to n do
    let l1 = random_list range len1 in
    let ls1 = LSet.of_list l1 in
(*    let os1 = set_of_list l1 in*)
    let ds1 = of_list l1 in
    let l2 = random_list range len2 in
    let ls2 = LSet.of_list l2 in
(*    let os2 = set_of_list l2 in*)
    let ds2 = of_list l2 in
    let ls = Common.prof "lset" (fun () -> LSet.contains ls2 ls1) in
(*    let os = Common.prof "oset" (fun () -> OSet.union os1 os2) in*)
    let ds = Common.prof "cis" (fun () -> subset ds1 ds2) in
    if (*to_lset*) ds <> ls
    then begin
      print_lset ls1;
      print_lset ls2;
(*
      print_lset ls;
      print_lset (to_lset ds);
*)
      raise Not_found end
  done;
  print_prof "lset";
(*  print_prof "oset";*)
  print_prof "cis"

*)
