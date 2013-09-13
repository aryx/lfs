open Common

open Common_logic

open Typetype

(*
  logic of type

      int -> string -> (x * y)    (exact)
      int * string -> bool        (can have diff between curry and tupling)
      ?x, !y,
      %x (=~ ?|!), ls  %bool, %'a list, %string, %char, %graph, (verra bien les types de base :)
      [int,string] -> x  (handle tuple/curry iso, swap of arg)
        could have 2 mechanism for each isomorphism, but not so useful i think
      ['a list,'a -> 'b,...] -> ['b list,...]
        ... is not `d, cos can be empty
        cant do just via ? and !, cos we need to relate the variable
	the 'a, 'b must be different (dont want have a a result a func with do 'a list -> ('a -> a) -> 'a list
	 => take care in the unification ('a <=> 'd) alpha conversion

      polymorphisme, ?('a -> 'b)
        pb cos ho func that take 'a -> 'a, will not satisfy => need make a diff
          between polymorphic variable that must be different between them, and those who
          can be identical `a -> `b  vs 'a -> 'b or use _ -> _
        how say the variable that must not instantiate farther ?
         ex dont want that 'a list -> 'b list  match the 'a -> 'b
         by default make 'a -> 'b match only 'a -> 'b (or 'c -> 'd)  and use `a -> `b for
         other stuff (and still require that they be different)
       => 'a (exact), `a(not exact), (still-needed?: '_a(exact no constrait), `_a(not exact, no constrait))
        the ` handle the generalise/instantiate isomorphisme

TODO subtyping polymorphisme, same pb: exact class (:class), specialise class (:<class)
	   or parent class (have both + and - polarity handled via solver)

TODO record (enable ?{x:`a}), class,
     can use too for tuple (int * float * ...)

TODO   in fact, string can be a regexp => use string solver
TODO   in fact can have or, and, not   everywhere:  (string|int) -> int
         but (again) this one can be emulated via builtin
        => could be able to use %notation

TODO   have arity ? use int solver ? (put in another taxo)

TODO   number, container,  class ?

TODO?  can force that `a and `b different,  (syntax: with `a != `b)
        could use this syntax for oo too ?

TODO handle qualified, cos when Unix.process, not sure than when cd process we found it

RESEARCH:
 xpath ?
 ho unif,   F bool <=> %bool
 we have a quite generic pb: we have type, and want find one, there is many
  subprop of type that can be a good lead (arity, nesteness, ...) too many in fact
  a generic soluce ?
----
choices:
  solver or just taxo ?
    taxo allow oo inheritance, but sux to have flexible query
     how say: i want exactly int -> string -> int, cos with taxo we would
       have "featurise" the type, and decompose it in ?int, ?string, !int, then
       ok the & of corelfs allow the swap argument isomorphism, but forbid precise query
     the transducer will get more complicated (have to create many hierarchy, for ?,!,%
     can handle alpha conversion ? need computation
     but yes can handle isomorphism, just cool for swap of arg, and can be handled via ?x & ?y, use the & of corelfs

  camlsearch good enough ?
      cant do the exact (how disable isomorphism ? always in [] mode)
      cant do the %
      can do the ? and ! with the _,  ex for !char, just transform in _a -> char
      up to isomorphism: search for int list -> (int * int) -> int list, and get fst, snd, map as answers
        but do we want this ? the programmer will make a better query, cos know that he search
        for something that is independant of list or int => will do cd ['a,'b] -> 'a

   algovista ?

impl choices:
   | IsoFunc of typep list * bool * typep list * bool
   in fact want also allow [int,string] -> bool

-----
pbs:
    how mix oo (taxo) and normal typing(polymorphisme) (solver) ?
     - the solver could have access to lfs axioms ?
     - encode in the type its inheritance (have oassob<binarytree|assoc<sequence)
       => more complex transducer, and  transducer must have acces to all the def :(
    how handle type synonyme ? with alias via links ?

  ho type ? if want container func, then want a ho type ('a 'container)
          have the kind of the type ?
          in that case we dont search really about the func, but about a type,
	  type could be obj too (and related to func via meta) and have the kind/x prop
	  the made_of (if a record, can have its constituent), is_newtype, ...

----
note: could be call to have the -and for solver, cos when do cd ?'a/!'b in fact we dont
       get what we want

*)

let parse = fun s ->
  Lexing.from_string s +> Typeparser.main Typelexer.token

let type_logic = fun (Prop s1) (Prop s2) ->
    let (x1, x2) = (parse s1, parse s2) in
    let _ = (assert (invariant x1), assert (invariant x2)) in
    let _ = pr2 (string_of_typep x1) in let _ = pr2 (string_of_typep x2) in

    (* we assume that x1 is simple, not a formula, TODO try compare formula *)
    let rec  solver_rec = function
      | (x, InOut y) -> for_inout y x
      | (x, FreePoly s) -> true (* TODO subst/unify *)

      | (Name s1, Name s2) -> s1 = s2
      | (Poly s1, Poly s2) -> s1 = s2    (* TODO alpha renaming pb *)
      | (Tuple xs, Tuple ys) when length xs = length ys ->
	  zip xs ys +> map solver_rec +> and_list (* TODO subst/unify (and in other place too) *)
      | (Application (a1,b1), Application (a2, b2)) ->
	  a1 = a2 && solver_rec (b1, b2)

      | (Function (ins1, out1), Function ([IsoParam (args, etc)], out2)) ->
	  (for_iso (ins1, args, etc) && solver_rec (out1, out2)) ||
	  (match ins1 with
	  | [Tuple ins1] -> (* do it only for func with one tuple, otherwise this is certainly not a case of curry/tuple iso *)
           (for_iso (ins1, args, etc) && solver_rec (out1, out2))
	  | _ -> false
	  )
      | (Function (ins1, out1), Function ([FreePoly s], out2)) -> (* TODO subst/unify *)
	  solver_rec (out1, out2)
      | (Function (ins1, out1), Function (ins2, out2)) when length ins1 = length ins2 ->
	  (* TODO inversion? with contravariance *)
	  zip ins1 ins2 +> map solver_rec +> and_list  && solver_rec (out1, out2)
      |	(x, (IsoParam (xs, etc))) -> for_iso2 (xs, etc) x

      | (x,y) -> false
(*	  let _ = pr2 (string_of_typep x) in let _ = pr2 (string_of_typep y) in *)
(* not handled, possibly cos contain formula *)
(*	  raise Impossible  *)


    (* can assume here too that have not formula, TODO try, x could be formula too, so do better than = *)
    and for_inout x = function
      |	y when y = x -> true
      |	(Name _ | Poly _) -> false
      |	Tuple xs -> xs +> List.exists (for_inout x)
      |	Application (a, b) -> for_inout x (Name a) || for_inout x b
      |	Function (xs, b) -> xs +> List.exists (for_inout x) || for_inout x b
      |	_ -> raise Impossible

    and for_iso (ins, isoargs, allowmore) =
      match (ins, isoargs, allowmore) with
      |	([],    [], false) -> true
      |	(_,     [], true) -> true
      |	([], x::xs, _) -> false
      |	(vs, x::xs, _) when List.mem x vs -> (* TODO, could be formula => replace mem by call to solver *)
	  for_iso (vs +> filter (fun v -> not (v = x)),   xs, allowmore)
      |	_ -> false


    and for_iso2 (xs, etc) x = false
    in

    match Typetype.is_formula x1, Typetype.is_formula x2 with
    | (true, true)   -> x1 = x2 (* TODO, try compare formula between them *)
    | (false, false) -> x1 = x2
    | (false, true)  -> solver_rec (x1, x2)
    | (true, false)  -> false


let is_formula (Prop s) = Typetype.is_formula (parse s)

let (main: unit) = interact_logic type_logic  is_formula





(*
when [],  try get all combinaison of func if a -> b -> c
  then try just a, try a -> b,
   match also tupling =>  can be used for fuzzy tuple

unifier/subst/renaming
   when in [], when have found a x, dont forget to suppr it from right,
   so be able to do [string,string,...] ->   linear logic spirit
  when `a, can match different time => matching return list of possibilities, then
   do any
kind of unify except than unify we allow x1 unif x2 or x2 unif x1, with |=
 there is an order, int |= `a   but not the inverse

renaming pb: 'a -> 'b -> 'a |=  'b -> 'a -> 'b
 - first alpha rename a in the right,  'b -> 'a1 -> 'b
    and subst 'b by 'a,  and say that 'a have been managed (otherwise allow futur
    new assoc)
 -  or, simpler?, normalise terms so that no need alpha renaming handling
    pb cos normalise via isomorphisme can be made invalid ['a,'b] vs 'a -> 'b
    the 'a can be associated to the 'b
 - CURRENT or, ensure that no variable occur at left and right =>
     rename 'a -> 'b -> 'a en 'a1 -> 'b1 -> 'a1,  then no pb of masking
     'a1 -> 'b1 -> 'a1 |= 'b2 -> 'a2 -> 'b2
       'a1 => 'b2 TODOCURRENT


    let (x1, x2) = (normalise "%1" x1, normalise "%2" x2)


    | (Name s1, Name s2) ->     (s1 = s2, [])
    | (Name s1, FreePoly s2) -> (true,    [(s2, Name s1)])
    | (Name _, _) ->            (false,   [])

    | (Poly s1, Poly s2) -> alpha renaming pb
    | (Poly s1, FreePoly s2) -> same
    | (Poly s1, _) -> (false, [])

    | (Function (e1, e2)), Function (IsoParam, IsoParam)) ->
	  compute list,
	  parse e1, if nuplet then incorporate each
            and that's all
          otherwise incorporate e1 as is,
	  parse e2, if again func, then incorporate
	      (dont look if tuple, a tuple in arg 2 is not a sign of curry/tuple pb)
	      fst e2, parse e3, ...
          compute all combination and try find one soluce among this
        kind of normalise func to have same form, with list of args
	    (but as curry sometimes(very rare) we want return func => many possibilites
              could make it explicit by require and handle explicit () when say want return func
	      as in f: int -> (int -> int)  to disambiguate
            )
	  take care handle subst
    | (Function (e1, e2)), Function (IsoParam, x)) ->
	  same, except that match for out will be easier
    | (Function (e1, e2)), Function (x, IsoParam)) ->
	  same, except that match for in will be easier
    | (Function (e1, e2), Function (x, y)) ->

    | (Function (e1, e2), (Function (FreePoly s, y))) ->
	the s can match many form, have to try multiple possibilities
	    when 'a -> 'b -> 'c |= `a -> `b

    | (Function (e1, e2), FreePoly s) -> as usual (take care occur check ?)
    | _ -> false

*)
