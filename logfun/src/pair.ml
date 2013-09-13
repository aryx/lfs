(** The product of the domains and languages of 2 sub-logics. *)

module Make (L1 : Logic.T) (L2 : Logic.T) =
  struct
    include Logic.Default

    type t = L1.t * L2.t

    let isvalue (x1,x2) = L1.isvalue x1 & L2.isvalue x2

    let top () = L1.top (), L2.top ()

    let entails (x1,x2) (y1,y2) = L1.entails x1 y1 & L2.entails x2 y2

    let conj(x1,x2) (y1,y2) = (L1.conj x1 y1, L2.conj x2 y2)

    let add (x1,x2) (y1,y2) = (L1.add x1 y1, L2.add x2 y2)

    let sub (x1,x2) (y1,y2) =
      if entails (x1,x2) (y1,y2) then
	try (x1, L2.sub x2 y2)
	with Not_found -> (L1.sub x1 y1, x2)
      else (x1,x2)

    let features (x1,x2) =
      let lf1 = match L1.features x1 with [] -> [(true,x1)] | l -> l in
      let lf2 = match L2.features x2 with [] -> [(true,x2)] | l -> l in
      List.fold_left (fun res1 (vis1,f1) ->
        List.fold_left (fun res2 (vis2,f2) ->
          (vis1&vis2,(f1,f2))::res2
        ) res1 lf2
      ) [] lf1

    let axiom (x1,x2) y_opt =
      let y1_opt, y2_opt =
	match y_opt with
	| None -> None, None
	| Some (y1,y2) -> Some y1, Some y2 in
      let t1 = L1.axiom x1 y1_opt in
      let t2 = L2.axiom x2 y2_opt in
      LSet.union t1 t2

    let gen (f1,f2) (g1,g2) hs =
      if hs=[]
      then
        let xs1 = L1.gen f1 g1 [] in
        let xs2 = if xs1 = [] then [] else L2.gen f2 g2 [] in
        List.fold_right (fun x1 res ->
          List.fold_right (fun x2 res' ->
            LSet.add (x1,x2) res'
          ) xs2 res
        ) xs1 []
      else
        let hs1, hs2 = List.split hs in
        let hs1, hs2 = LSet.of_list hs1, LSet.of_list hs2 in
        let xs1 =
          List.fold_left (fun res x2 ->
            List.fold_left (fun res' h1 ->
              if List.exists (fun h -> entails h (h1,x2)) hs then res else LSet.add (h1,x2) res
            ) res hs1
          ) (LSet.empty ()) (L2.gen f2 g2 hs2) in
        let xs2 =
          List.fold_left (fun res x1 ->
            List.fold_left (fun res' h2 ->
              if List.exists (fun h -> entails h (x1,h2)) hs then res else LSet.add (x1,h2) res
            ) res hs2
          ) (LSet.empty ()) (L1.gen f1 g1 hs1) in
        LSet.union xs1 xs2

    let parse = parser
      [<x1 = L1.parse;
        x2 = L2.parse ?? "Syntax error in the second part of a pair, after: " ^ Syntax.stringizer (L1.print x1)
       >] -> (x1, x2)

    let print (x1,x2) =
      let p2 = L2.print x2 in
      if p2 = []
      then L1.print x1
      else L1.print x1 @ (Token.PP_tilda::p2)

    let simpl (x1,x2) =
      [<Common.stream_map (fun x1' -> (x1',x2)) (L1.simpl x1);
        Common.stream_map (fun x2' -> (x1,x2')) (L2.simpl x2)>]

  end
