
(* useful code for representing idents and terms as integers *)
(* --------------------------------------------------------- *)

let cpt : int ref = ref 0

type id = int (* >0 for terms, <0 for idents *)

let h_term2id : (string,id) Hashtbl.t = Hashtbl.create 100
let h_ident2id : (string,id) Hashtbl.t = Hashtbl.create 100
    
let h_id2s : (id,string) Hashtbl.t = Hashtbl.create 100

let term2id s =
  try Hashtbl.find h_term2id s
  with Not_found ->
    incr cpt;
    let id = !cpt in
    Hashtbl.add h_term2id s id;
    Hashtbl.add h_id2s id s;
    id

let ident2id s =
  try Hashtbl.find h_ident2id s
  with Not_found ->
    incr cpt;
    let id = - !cpt in
    Hashtbl.add h_ident2id s id;
    Hashtbl.add h_id2s id s;
    id

let id2s id = (* may raise Not_found *)
  Hashtbl.find h_id2s id


(** Utility code for the representation and managing of taxonomic relations over terms. *)

(* type of data associated to a term *)
type t = {
    mutable sufs : int LSet.t;  (* list of sufficient conditions *)
    mutable necs : int LSet.t  (* list of necessary conditions *)
  }

(* dictionary of terms *)
let tbl : (int,t) Hashtbl.t = Hashtbl.create 1000

(* Invariants
   - a term absent from tbl has all fields equal to []
*)

(* get data about a term *)
let get : int -> t =
  fun id ->
    try Hashtbl.find tbl id
    with Not_found ->
      let data = {sufs = LSet.empty (); necs = LSet.empty ()} in
      Hashtbl.add tbl id data;
      data

let get_sufs : int -> int list =
  fun id ->
    (get id).sufs

let get_necs : int -> int list =
  fun id ->
    (get id).necs

(* add a sufficient condition *)
let add_suf : int -> int -> unit =
  fun id suf ->
    let data = get id in
    data.sufs <- LSet.add suf data.sufs;
    let data2 = get suf in
    data2.necs <- LSet.add id data2.necs

(* add a necessary condition *)
let add_nec : int -> int -> unit =
  fun id nec ->
    let data = get id in
    data.necs <- LSet.add nec data.necs;
    let data2 = get nec in
    data2.sufs <- LSet.add id data2.sufs

(* delete a sufficient condition *)
let del_suf : int -> int -> unit =
  fun id suf ->
    let data = get id in
    data.sufs <- LSet.remove suf data.sufs;
    let data2 = get suf in
    data2.necs <- LSet.remove id data2.necs

(* delete a necessary condition *)
let del_nec : int -> int -> unit =
  fun id nec ->
    let data = get id in
    data.necs <- LSet.remove nec data.necs;
    let data2 = get nec in
    data2.sufs <- LSet.remove id data2.sufs


(* load and save *)

type data = {
    cpt : int;
    id2s : (id * string) list;
    tbl : (int * t) list;
  }

let init () =
  cpt := 0;
  Hashtbl.clear h_id2s;
  Hashtbl.clear h_term2id;
  Hashtbl.clear h_ident2id;
  Hashtbl.clear tbl

let load (t : data) =
  init ();
  cpt := t.cpt;
  List.iter
    (fun (id,s) ->
      Hashtbl.add h_id2s id s;
      if id > 0
      then Hashtbl.add h_term2id s id
      else Hashtbl.add h_ident2id s id)
    t.id2s;
  List.iter
    (fun (id,x) ->
      Hashtbl.add tbl id x)
    t.tbl

let save () =
  let t = {
    cpt = !cpt;
    id2s = Hashtbl.fold (fun id s l -> (id,s)::l) h_id2s [];
    tbl = Hashtbl.fold (fun id x l -> (id,x)::l) tbl [];
  } in
  (t : data)
