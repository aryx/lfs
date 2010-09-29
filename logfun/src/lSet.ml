(** Sets as ordered polymorphic lists. *)

include List

let compare x y = Pervasives.compare y x

type 'a t = 'a list

(** The empty set. *)
let empty : unit -> 'a list = fun () -> []

(** Test if a set is empty or not. *)
let is_empty : 'a list -> bool = function [] -> true | _ -> false

(** Return the cardinal of a set. *)
let cardinal : 'a list -> int = List.length

(** Return a list of the elements. *)
let elements : 'a t -> 'a list = fun l -> l

(** Comparison of 2 sets for any relation among: Contains, Contained, Equals, Other. *)

type comp = Contains | Contained | Equals | Other

let rec comp : 'a list -> 'a list -> comp =
  fun l1 l2 -> match l1, l2 with
  | [], [] -> Equals
  | _, [] -> Contains
  | [], _ -> Contained
  | x1::l1, x2::l2 ->
      let c = compare x1 x2 in
      if c < 0 then
        match comp l1 (x2::l2) with
        | Equals
        | Contains -> Contains
        | Contained
        | Other -> Other
      else if c > 0 then
        match comp (x1::l1) l2 with
        | Equals
        | Contained -> Contained
        | Contains
        | Other -> Other
      else comp l1 l2

(** Return true if the first set contains the second. *)
let rec contains : 'a list -> 'a list -> bool =
  fun l1 l2 -> match l1, l2 with
    _, [] -> true
  | [], _ -> false
  | x1::l1, x2::l2 ->
      let comp = compare x1 x2 in
      if comp < 0 then contains l1 (x2::l2)
      else if comp > 0 then false
      else contains l1 l2

let subset l1 l2 = contains l2 l1

let equal = (=)

let mem : 'a -> 'a list -> bool =
  fun x l -> contains l [x]

(** Return the union of 2 and several sets. *)
let rec union : 'a list -> 'a list -> 'a list =
  fun l1 l2 -> match l1, l2 with
    [], l2 -> l2
  | l1, [] -> l1
  | x1::l1, x2::l2 ->
      let comp = compare x1 x2 in
      if comp < 0 then x1::union l1 (x2::l2)
      else if comp > 0 then x2::union (x1::l1) l2
      else x1::union l1 l2

let union_r : 'a list list -> 'a list =
  fun sets -> List.fold_left (fun res set -> union res set) [] sets

let add : 'a -> 'a list -> 'a list =
  fun x l -> union [x] l

let singleton : 'a -> 'a list =
  fun x -> [x]

(** Get a set from a list. *)
let of_list : 'a list -> 'a t =
  fun l -> List.fold_left (fun res x -> add x res) [] l

(** Return the intersection of 2 sets. *)
let rec inter : 'a list -> 'a list -> 'a list =
  fun l1 l2 -> match l1, l2 with
    [], _ -> []
  | _, [] -> []
  | x1::l1, x2:: l2 ->
      let comp = compare x1 x2 in
      if comp < 0 then inter l1 (x2::l2)
      else if comp > 0 then inter (x1::l1) l2
      else x1::inter l1 l2

let rec inter_r : 'a list list -> 'a list =
  function
      [] -> raise (Invalid_argument "inter_r : empty list of sets")
    | set::sets -> List.fold_right (fun set res -> inter set res) sets set

(** Return the subtraction of 2 sets. *)
let rec subtract : 'a list -> 'a list -> 'a list =
  fun l1 l2 ->  match l1, l2 with
    [], _ -> []
  | l1, [] -> l1
  | x1::l1, x2::l2 ->
      let comp = compare x1 x2 in
      if comp < 0 then x1::subtract l1 (x2::l2)
      else if comp > 0 then subtract (x1::l1) l2
      else subtract l1 l2

let diff = subtract

let subtract_r : 'a list -> 'a list list -> 'a list =
  fun set sets -> List.fold_left (fun res set -> subtract res set) set sets

let remove : 'a -> 'a list -> 'a list =
  fun x l -> subtract l [x]

(* partition par the belonging to a set *)
(* partitioner -> partitionee -> inter * diff *)
      let rec partition_set : 'a t -> 'a t -> 'a t * 'a t =
	fun l1 l2 -> match l1, l2 with
	| [], l2 -> [], l2
	| _, [] -> [], []
	| x1::l1, x2::l2 ->
	    let comp = compare x1 x2 in
	    if comp < 0 then
	      partition_set l1 (x2::l2)
	    else if comp > 0 then
	      let inter, diff = partition_set (x1::l1) l2 in
	      inter, x2::diff
	    else
	      let inter, diff = partition_set l1 l2 in
	      x2::inter, diff

(** Remove an element if present, add it otherwise. *)
let rec flip : 'a -> 'a list -> 'a list =
  fun x -> function
    [] -> [x]
  | y::l ->
      let comp = compare x y in
      if comp < 0 then x::y::l
      else if comp > 0 then y::flip x l
      else l

(** Generic folding over the synchronized traversal of 2 LSets *)

type inwhich = Infst | Insnd | Inboth

let rec fold : ('b -> inwhich * 'a -> 'b) -> 'b -> 'a list -> 'a list -> 'b =
  fun f e l1 l2 ->  match l1, l2 with
  | [], [] -> e
  | x1::l1, [] -> fold f (f e (Infst,x1)) l1 []
  | [], x2::l2 -> fold f (f e (Insnd,x2)) [] l2
  | x1::l1, x2::l2 ->
      let comp = compare x1 x2 in
      if comp < 0 then fold f (f e (Infst,x1)) l1 (x2::l2)
      else if comp > 0 then fold f (f e (Insnd,x2)) (x1::l1) l2
      else fold f (f e (Inboth,x1)) l1 l2


(*
   iterative functions on lists can also be applied, at least in some cases
*)
