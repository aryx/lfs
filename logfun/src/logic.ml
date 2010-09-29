(** Logic signature and common definitions on logics.

   The signature of logic is made of a type [t] for the abstract syntax, or internal
   representation, of formulas, operations, and properties. The main operation [entails]
   is a theorem prover for the subsumption test (alike description logics). The properties
   of a composed logic are automatically computed by calling the function [props].
   The semantics of a logic is implicit in the statement of these properties in each logic
   functor.

*)

(** {1 Logic Properties} *)

type requirements = string LSet.t
      (** Type for representing a set of requirements for a property to be satisfied. *)

let isok = LSet.empty ()
    (** The empty set represents a satisfied property. *)
let requires m p = LSet.singleton (String.concat "." [m; p])
    (** The singleton [requires m p] says the required property [p] is not satisfied in logic [m]. *)
let reqand l = LSet.union_r l
    (** [reqand] defines the conjunction over requirements, and is defined as the set union over sets of requirements. *)

type props = {
    df : requirements;
    st : requirements;
    st' : requirements;
    sg' : requirements;
    po_entails : requirements;
    cs_entails : requirements;
    cp_entails : requirements;
    cp'_entails : requirements;
    cp_top : requirements;
    cs_bot : requirements;
    defst_conj : requirements;
    cs_conj : requirements;
    cp_conj : requirements;
    cs_disj : requirements;
    cp_disj : requirements;
    total_le : requirements;
    po_le_min : requirements;
    cs_le_min : requirements;
    cp_le_min : requirements;
    po_le_max : requirements;
    cs_le_max : requirements;
    cp_le_max : requirements;
    cs_le_overlap : requirements;
    cp_le_overlap : requirements;
    reduced : requirements;
    reduced' : requirements;
    reduced_top : requirements;
    reduced_bot : requirements;
    reduced_bot' : requirements;
    reduced_right : requirements;
  }

let no_props m = {
  df = requires m "df";
  st = requires m "st";
  st' = requires m "st'";
  sg' = requires m "sg'";
  po_entails = requires m "po_entails";
  cs_entails = requires m "cs_entails";
  cp_entails = requires m "cp_entails";
  cp'_entails = requires m "cp'_entails";
  cp_top = requires m "cp_top";
  cs_bot = requires m "cs_bot";
  defst_conj = requires m "defst_conj";
  cs_conj = requires m "cs_conj";
  cp_conj = requires m "cp_conj";
  cs_disj = requires m "cs_disj";
  cp_disj = requires m "cp_disj";
  total_le = requires m "total_le";
  po_le_min = requires m "po_le_min";
  cs_le_min = requires m "cs_le_min";
  cp_le_min = requires m "cp_le_min";
  po_le_max = requires m "po_le_max";
  cs_le_max = requires m "cs_le_max";
  cp_le_max = requires m "cp_le_max";
  cs_le_overlap = requires m "cs_le_overlap";
  cp_le_overlap = requires m "cp_le_overlap";
  reduced = requires m "reduced";
  reduced' = requires m "reduced'";
  reduced_top = requires m "reduced_top";
  reduced_bot = requires m "reduced_bot";
  reduced_bot' = requires m "reduced_bot'";
  reduced_right = requires m "reduced_right";
}

let all_props = {
  df = isok;
  st = isok;
  st' = isok;
  sg' = isok;
  po_entails = isok;
  cs_entails = isok;
  cp_entails = isok;
  cp'_entails = isok;
  cp_top = isok;
  cs_bot = isok;
  defst_conj = isok;
  cs_conj = isok;
  cp_conj = isok;
  cs_disj = isok;
  cp_disj = isok;
  total_le = isok;
  po_le_min = isok;
  cs_le_min = isok;
  cp_le_min = isok;
  po_le_max = isok;
  cs_le_max = isok;
  cp_le_max = isok;
  cs_le_overlap = isok;
  cp_le_overlap = isok;
  reduced = isok;
  reduced' = isok;
  reduced_top = isok;
  reduced_bot = isok;
  reduced_bot' = isok;
  reduced_right = isok;
}


let fixpoint : (unit -> props) -> (unit -> props) =
  fun p ->
    let rec f : unit -> props =
      let l = ref all_props in
      let is_ancestor = ref false in
      let was_visited = ref false in
      fun () ->
	if !is_ancestor
	then begin
	  was_visited := true;
	  !l end
	else begin
	  was_visited := false;
	  is_ancestor := true;
	  let l' = p () in
	  is_ancestor := false;
	  if not !was_visited or l' = !l
	  then begin
	    l := l';
	    !l end
	  else begin
	    l := l';
	    f () end
	end in
    f


(** {1 Logic abstract syntax with tag} *)

open Token

type tag = Token.t

let parse_left : tag -> Syntax.t_stream -> unit = fun tag -> parser [<'t when t = tag; 'LeftPar>] -> ()
let print_left : tag -> Syntax.t_list = fun tag -> [tag; LeftPar]

let parse_right : tag -> Syntax.t_stream -> unit = fun tag -> parser [<'RightPar>] -> ()
let print_right : tag -> Syntax.t_list = fun tag -> [RightPar]

let parse_unary : tag -> (Syntax.t_stream -> 'a) -> Syntax.t_stream -> 'a =
  fun tag p -> parser
    | [<_ = parse_left tag; x = p; _ = parse_right tag>] -> x
let print_unary : tag -> Syntax.t_list -> Syntax.t_list =
  fun tag l ->
    print_left tag @ l @ print_right tag



(** {1 Logic signatures} *)

(** Basic logical operations *)
module type T_BASE =
  sig
    type t (** Type of formulas. *)
    val parse : Syntax.t_stream -> t  (** Parser of formulas. *)
    val print : t -> Syntax.t_list (** Printer of formulas. *)
    val entails : t -> t -> bool (** Logical subsumption. *)
  end

(*
(** Additional logical operations for CAMELIS *)
module type T_CAMELIS =
  sig
    include T_BASE
    val isvalue : t -> bool (** [isvalue f] returns whether [f] has neither more specific formula, nor different equivalents. *)
    val top : unit -> t (** Top formula. @raise Not_found when undefined *)
    val bot : unit -> t (** Bottom formula. @raise Not_found when undefined *)
    val conj : t -> t -> t (** Conjunction (glb). @raise Not_found when undefined *)
    val add : t -> t -> t (** Update the first formula by making it entail the second formula. @raise Not_found when undefined *)
    val sub : t -> t -> t (** Update the first formula by making it not entail the second formula. @raise Not_found when undefined *)
    val features : t -> (bool * t) list (** Extract a set of features entailed by the given formula. *)
	(** If the boolean is true, the feature is made visible. *)
    val axiom : t -> t option -> string LSet.t (** Get modified terms from axioms: snd arg. is None => Top. *)
    val gen : t -> t -> t list -> t LSet.t (** Dichotomic operator (for machine learning). *)
  end
*)

(** Additional logical operations for DELFIN *)
module type T_DELFIN =
  sig
    include T_BASE
    val top : unit -> t (** Top formula. @raise Not_found when undefined *)
    val gen : t -> t -> t list -> t LSet.t (** Dichotomic operator (for machine learning). *)
    val simpl : t -> t Stream.t (** Produces a stream of syntactical simplifications of the given formula. *)
  end

(** The largest useful signature of logics. *)
module type T =
  sig
    val props : unit -> props
    type t
    val desc : t -> bool
    val feat : t -> bool
    val isvalue : t -> bool
    val top : unit -> t (* may raise Not_found *)
    val bot : unit -> t (* may raise Not_found *)
    val entails : t -> t -> bool
    val share : t -> t -> bool (* may raise Not_found *)
    val conj : t -> t -> t (* may raise Not_found *)
    val disj : t -> t -> t list
    val le_min : t -> t -> bool (* may raise Not_found *)
    val le_max : t -> t -> bool (* may raise Not_found *)
    val le_overlap : t -> t -> bool (* may raise Not_found *)
    val add : t -> t -> t (* may raise Not_found *)
    val sub : t -> t -> t (* may raise Not_found *)
    val features : t -> (bool * t) list
    val terms : t -> string list
    val axiom : t -> t option -> string LSet.t
    val gen : t -> t -> t list -> t LSet.t
    val parse : Syntax.t_stream -> t
    val print : t -> Syntax.t_list
    val simpl : t -> t Stream.t
    val compare : t -> t -> int
    val parse_compact : Syntax.t_stream -> Syntax.t_list
  end

(** A default definition of a logic to be included in logic functors for extensibility. *)
module Default =
  struct
    let props () = no_props "Default"

    let desc f = true
    let feat f = true
    let isvalue f = false
    let top () = raise Not_found
    let bot () = raise Not_found
    let entails f g = false
    let share f g = raise Not_found
    let conj f g = raise Not_found
    let disj f g = [f; g]
    let le_min f g = raise Not_found
    let le_max f g = raise Not_found
    let le_overlap f g = raise Not_found
    let add f g = raise Not_found
    let sub f g = raise Not_found
    let features f = []
    let terms f = []
    let axiom f g_opt = LSet.empty ()
    let gen f g hs = LSet.empty ()
    let parse _ = raise (Invalid_argument "Logic.Default.parse: define 'parse' in logics")
    let print f = raise (Invalid_argument "Logic.Default.print: define 'print' in logics")
    let simpl f = [<>]
    let compare = Pervasives.compare
    let parse_compact _ = raise (Invalid_argument "Logic.Default.parse_compact: define 'parse_compact' in logics")
  end

(** The functor identity. *)
module Id (X : T) =
  struct
    let props =
      fixpoint
	(fun () ->
	  let l = X.props () in
	  l)

    type t = X.t

    let desc f = X.desc f
    let feat f = X.feat f
    let isvalue f = X.isvalue f
    let top () = X.top ()
    let bot () = X.bot ()
    let entails f g = X.entails f g
    let share f g = X.share f g
    let conj f g = X.conj f g
    let disj f g = X.disj f g
    let le_min f g = X.le_min f g
    let le_max f g = X.le_max f g
    let le_overlap f g = X.le_overlap f g
    let add f g = X.add f g
    let sub f g = X.sub f g
    let features f = X.features f
    let terms f = X.terms f
    let axiom f g_opt = X.axiom f g_opt
    let gen f g hs = X.gen f g hs
    let parse str = X.parse str
    let print f = X.print f
    let simpl f = X.simpl f
    let compare f g = X.compare f g
    let parse_compact str = X.parse_compact str
  end


(** {1 Logic tester} *)

module Tester (L : T) =
  struct
    let print_bool b =
      print_endline (string_of_bool b)

    let print_undef () =
      print_endline "<undefined>"

    let print_f f =
      print_endline (Syntax.stringizer (L.print f))

    let print_prop p b =
      print_string p;
      if LSet.is_empty b
      then print_endline " is ok"
      else begin
	print_string " requires";
	List.iter (fun s -> print_string " "; print_string s) b;
	print_newline () end

    let print_bool_undef func =
      try print_bool (func ())
      with Not_found -> print_undef ()

    let print_f_undef func =
      try print_endline (Syntax.stringizer (L.print (func ())))
      with Not_found -> print_undef ()

    let apply op l =
      match op, l with
      | "parse", [f] -> ()
      | "print", [f] -> print_f f
      | "desc", [f] -> print_bool (L.desc f)
      | "feat", [f] -> print_bool (L.feat f)
      | "isvalue", [f] -> print_bool (L.isvalue f)
      | "entails", [f; g] -> print_bool (L.entails f g)
      | "share", [f; g] -> print_bool (L.share f g)
      | "top", [] -> print_f_undef (fun () -> L.top ())
      | "bot", [] -> print_f_undef (fun () -> L.bot ())
      | "conj", [f; g] -> print_f_undef (fun () -> L.conj f g)
      | "disj", [f; g] -> List.iter print_f (L.disj f g)
      | "le_min", [f; g] -> print_bool_undef (fun () -> L.le_min f g)
      | "le_max", [f; g] -> print_bool_undef (fun () -> L.le_max f g)
      | "le_overlap", [f; g] -> print_bool_undef (fun () -> L.le_overlap f g)
      | "add", [f; g] -> print_f_undef (fun () -> L.add f g)
      | "sub", [f; g] -> print_f_undef (fun () -> L.sub f g)
      | "features", [f] ->
	  List.iter
	    (fun (vis,x) -> 
	      print_string (if vis then "show " else "hide ");
	      print_f x)
	    (L.features f)
      | "terms", [f] -> List.iter print_endline (L.terms f)
      | "axiom", [f] -> List.iter print_endline (L.axiom f None)
      | "axiom", [f; g] -> List.iter print_endline (L.axiom f (Some g))
      | "gen", f::g::hs -> List.iter print_f (L.gen f g hs)
      | "simpl", [f] -> Stream.iter print_f (L.simpl f)
      | "compare", [f; g] -> print_int (L.compare f g); print_newline ()
      | op, l ->
	  print_endline ("Invalid number of arguments for operation " ^ op);
	  print_int (List.length l); print_endline " arguments read"

    let main () =
      let props = L.props () in
      print_prop "df" props.df;
      print_prop "st'" props.st';
      print_prop "sg'" props.sg';
      print_prop "po_entails" props.po_entails;
      print_prop "cs_entails" props.cs_entails;
      print_prop "cp_entails" props.cp_entails;
      print_prop "cp'_entails" props.cp'_entails;
      print_prop "cp_top" props.cp_top;
      print_prop "cs_bot" props.cs_bot;
      print_prop "defst_conj" props.defst_conj;
      print_prop "cs_conj" props.cs_conj;
      print_prop "cp_conj" props.cp_conj;
      print_prop "cs_disj" props.cs_disj;
      print_prop "cp_disj" props.cp_disj;
      print_prop "po_le_min" props.po_le_min;
      print_prop "cs_le_min" props.cs_le_min;
      print_prop "cp_le_min" props.cp_le_min;
      print_prop "total_le" props.total_le;
      print_prop "po_le_max" props.po_le_max;
      print_prop "cs_le_max" props.cs_le_max;
      print_prop "cp_le_max" props.cp_le_max;
      print_prop "cs_le_overlap" props.cs_le_overlap;
      print_prop "cp_le_overlap" props.cp_le_overlap;
      print_prop "reduced" props.reduced;
      print_prop "reduced'" props.reduced';
      print_prop "reduced_top" props.reduced_top;
      print_prop "reduced_bot" props.reduced_bot;
      print_prop "reduced_bot'" props.reduced_bot';
      print_prop "reduced_right" props.reduced_right;
      while true do
	print_string "[op f*] ";
	let cmd = read_line () in
	try match Syntax.from_string cmd with parser
	| [<'Token.Ident "exit">] -> exit 0
	| [<'Token.Ident op; l = Syntax.parse_star (parser [<x = L.parse; 'Token.Slash>] -> x); str>] ->
	    ( match Stream.peek str with
	    | None -> apply op l
	    | Some tok -> print_endline ("Syntax error: " ^ Syntax.stringizer (Syntax.list_of_stream str)))
	| [<>] -> print_endline "Syntax error"
	with
	| Stream.Error msg -> print_endline msg
	| Not_found -> print_undef ()
(*	| exn -> print_endline (Printexc.to_string exn)*)
      done

  end


(** {1 Common functions for logics} *)


let rec satgen =
  fun entails gen f g lazy_hs lubs ->
    let xs = gen f g lubs in
    if xs = []
    then []
    else
      let hs = Lazy.force lazy_hs in
      let xs' = List.filter (fun x -> not (List.exists (fun h -> entails h x) hs)) xs in
      if xs' = []
      then satgen entails gen f g lazy_hs (satgen_insert entails xs lubs)
      else xs'
and satgen_insert entails xs lubs =
  List.fold_left
    (fun res x -> x::List.filter (fun l -> not (entails x l)) lubs)
    lubs
    xs

(** {1 Adapters for LISFS plugins} *)

module Plugin (L : T) :
    sig
      val entails : string -> string -> bool
      val isvalue : string -> bool
    end =
  struct
    let parse : string -> L.t =
      fun s ->
	L.parse (Syntax.from_string s)
	  
    let entails : string -> string -> bool =
      fun s1 s2 ->
	L.entails (parse s1) (parse s2)
	  
    let isvalue : string -> bool =
      fun s ->
	L.isvalue (parse s)
  end
