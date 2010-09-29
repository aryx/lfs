(** Times with 3 levels of granularity: hours, minutes, and seconds. *)

open Logic

module Make =
  struct
    include Default

    let props () =
      {(no_props "Time") with
       st = isok;
       st' = isok;
       po_entails = isok;
       cs_entails = isok;
       cp_entails = isok;
       cp'_entails = isok;
       cp_top = isok;
       cs_bot = isok;
       cs_conj = isok;
       cp_conj = isok;
       cs_disj = isok;
       cp_disj = isok;
       po_le_min = isok;
       cs_le_min = isok;
       cp_le_min = isok;
       po_le_max = isok;
       cs_le_max = isok;
       cp_le_max = isok;
     }

    type t = (int option * int) option * int  (* seconds, minutes, hours *)

    open Unix

    let now : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (Some (Some tm.tm_sec, tm.tm_min), tm.tm_hour)

    let this_min : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (Some (None, tm.tm_min), tm.tm_hour)

    let this_hour : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (None, tm.tm_hour)
    let next_hour : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (None, (tm.tm_hour + 1) mod 24)
    let last_hour : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (None, (tm.tm_hour - 1) mod 24)

    open Token

    let parse = parser
      |	[<'Nat h; ms>] ->
	  if h>=24 or h<0 then raise (Stream.Error ("Syntax error: out of range hour (0..23)"));
	  ( match ms with parser
	  | [<'Colon; 'Nat m ?? "Syntax error: minutes expected after hour"; s>] ->
	      if m>=60 or m<0 then raise (Stream.Error ("Syntax error: out of range minutes (0..59)"));
	      ( match s with parser
	      |	[<'Colon; 'Nat s ?? "Syntax error: seconds expected after minutes">] ->
		  if s>=60 or s<0 then raise (Stream.Error ("Syntax error: out of range seconds (0..59)"));
		  (Some (Some s, m), h)
	      |	[<>] ->
		  (Some (None, m), h))
	  | [<'Ident "h">] ->
	      (None, h)
	  | [<>] ->
	      (None, h))
      |	[<'Ident "noon">] -> (Some (Some 0, 0), 12)
      |	[<'Ident "midnight">] -> (Some (Some 0, 0), 0)
      |	[<'Ident "now">] -> now ()
      |	[<'Ident "this"; str>] -> begin match str with parser
	| [<'Ident "sec">] -> now ()
	| [<'Ident "min">] -> this_min ()
	| [<'Ident "hour">] -> this_hour ()
	| [<>] -> raise (Stream.Error "Syntax error: 'sec', 'min' or 'hour' expected after 'this'") end
      |	[<'Ident "next"; 'Ident "hour" ?? "Syntax error: 'hour' expected after 'next'">] -> next_hour ()
      |	[<'Ident "last"; 'Ident "hour" ?? "Syntax error: 'hour' expected after 'last'">] -> last_hour ()

    let rec print = function
      |	None, h -> [Nat h]
      |	Some (None, m), h -> [Nat h; Colon; Nat m]
      |	Some (Some s, m), h -> [Nat h; Colon; Nat m; Colon; Nat s]


    let isvalue = function
      |	Some (Some s, m), h -> true
      | _ -> false

    let compare (sm1,h1) (sm2,h2) =
      if h1 < h2 then -1
      else if h1 > h2 then 1
      else match sm1, sm2 with
      | None, None -> 0
      |	None, _ -> -1
      |	_, None -> 1
      |	Some (s1,m1), Some (s2,m2) ->
	  if m1 < m2 then -1
	  else if m1 > m2 then 1
	  else match s1, s2 with
	  | None, None -> 0
	  | None, _ -> -1
	  | _, None -> 1
	  | Some n1, Some n2 ->
	      if n1 < n2 then -1
	      else if n1 > n2 then 1
	      else 0

    let entails (sm1,h1) (sm2,h2) =
      h1 = h2 &
      match sm1, sm2 with
      |	_, None -> true
      |	Some (s1,m1), Some (s2,m2) ->
	  m1 = m2 &
	  ( match s1, s2 with
	  | _, None -> true
	  | Some n1, Some n2 -> n1 = n2
	  | _ -> false)
      |	_ -> false

    let share t1 t2 = entails t1 t2 or entails t2 t1

    let conj t1 t2 =
      if entails t1 t2 then t1
      else if entails t2 t1 then t2
      else raise Not_found

    let le_min (sm1,h1) (sm2,h2) =
      if h1 < h2 then true
      else if h1 > h2 then false
      else match sm1, sm2 with
      |	None, _ -> true
      |	Some (Some 0, 0), _ -> true
      | Some  _, None -> false
      |	Some (s1,m1), Some (s2,m2) ->
	  if m1 < m2 then true
	  else if m1 > m2 then false
	  else match s1, s2 with
	  | None, _ -> true
	  | Some 0, _ -> true
	  | Some _, None -> false
	  | Some n1, Some n2 -> n1 <= n2

    let le_max (sm1,h1) (sm2,h2) =
      if h1 < h2 then true
      else if h1 > h2 then false
      else match sm1, sm2 with
      |	_, None -> true
      |	_, Some (Some 59, 59) -> true
      | None, Some _ -> false
      |	Some (s1,m1), Some (s2,m2) ->
	  if m1 < m2 then true
	  else if m1 > m2 then false
	  else match s1, s2 with
	  | _, None -> true
	  | _, Some 59 -> true
	  | None, Some _ -> false
	  | Some n1, Some n2 -> n1 <= n2

    let features = function
      |	None, h -> [true,(None, h)]
      |	Some (None, m), h -> [true,(Some (None, m), h); true,(None,h)]
      |	Some (Some s, m), h -> [true,(Some (Some s, m), h); true,(Some (None, m), h); true,(None,h)]

    let simpl = function
      | None, h -> [<>]
      | Some (None, m), h -> [<'None,h>]
      | Some (Some s, m), h -> [<'Some (None, m), h>]

  end 
