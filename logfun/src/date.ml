(** Dates at 3 levels of granularity: years, months, and days. *)

open Logic

module Make =
  struct
    include Default

    let props () =
      {(no_props "Date") with
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

    type t = (int option * int) option * int  (* day, month, year *)

    open Unix

    let from_time : float -> t =
      fun sec ->
	let tm = localtime sec in
	(Some (Some tm.tm_mday, tm.tm_mon+1), tm.tm_year+1900)

    let day : float = 86400.
    let today : unit -> t = fun () -> from_time (time ())
    let yesterday : unit -> t = fun () -> from_time (time () -. day)
    let tomorrow : unit -> t = fun () -> from_time (time () +. day)

    let this_month : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (Some (None, tm.tm_mon+1), tm.tm_year+1900)
    let next_month : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (Some (None, ((tm.tm_mon + 1) mod 12) + 1), tm.tm_year + 1900 + if tm.tm_mon=11 then 1 else 0)
    let last_month : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (Some (None, ((tm.tm_mon - 1) mod 12) + 1), tm.tm_year + 1900 - if tm.tm_mon=0 then 1 else 0)

    let this_year : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (None, tm.tm_year+1900)
    let next_year : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (None, tm.tm_year+1901)
    let last_year : unit -> t = fun () ->
      let tm = localtime (time ()) in
      (None, tm.tm_year+1899)

    open Token

    let length_month y = function
      | 1 -> 31
      | 2 -> if y mod 4 = 0 & (y mod 100 <> 0 or y mod 400 = 0) then 29 else 28
      | 3 -> 31
      | 4 -> 30
      | 5 -> 31
      | 6 -> 30
      | 7 -> 31
      | 8 -> 31
      | 9 -> 30
      | 10 -> 31
      | 11 -> 30
      | 12 -> 31
      | _ -> raise (Invalid_argument "Date.length_month: invalid month number")

    let parse_month = parser
      |	[<'Ident "jan">] -> 1
      |	[<'Ident "feb">] -> 2
      |	[<'Ident "mar">] -> 3
      |	[<'Ident "apr">] -> 4
      |	[<'Ident "may">] -> 5
      |	[<'Ident "jun">] -> 6
      |	[<'Ident "jul">] -> 7
      |	[<'Ident "aug">] -> 8
      |	[<'Ident "sep">] -> 9
      |	[<'Ident "oct">] -> 10
      |	[<'Ident "nov">] -> 11
      |	[<'Ident "dec">] -> 12

    let print_month = function
      |	1 -> Ident "jan"
      |	2 -> Ident "feb"
      |	3 -> Ident "mar"
      |	4 -> Ident "apr"
      |	5 -> Ident "may"
      |	6 -> Ident "jun"
      |	7 -> Ident "jul"
      |	8 -> Ident "aug"
      |	9 -> Ident "sep"
      |	10 -> Ident "oct"
      |	11 -> Ident "nov"
      |	_ -> Ident "dec"

    let parse = parser
      |	[<dy = Syntax.parse_int; s>] ->
	  ( match s with parser
	  | [<m = parse_month; y = Syntax.parse_int ?? "Syntax error: year expected after month">] ->
	      (Some (Some dy, m), y)
	  | [<>] ->
	      (None, dy)
	  )
      |	[<m = parse_month; y = Syntax.parse_int ?? "Syntax error: year expected after month">] ->
	  (Some (None, m), y)
      |	[<'Ident "today">] -> today ()
      |	[<'Ident "tomorrow">] -> tomorrow ()
      |	[<'Ident "yesterday">] -> yesterday ()
      |	[<'Ident "this"; str>] -> begin match str with parser
	| [<'Ident "day">] -> today ()
	| [<'Ident "month">] -> this_month ()
	| [<'Ident "year">] -> this_year ()
	| [<>] -> raise (Stream.Error "Syntax error: 'day', 'month' or 'year' expected after 'this'") end
      |	[<'Ident "next"; str>] -> begin match str with parser
	| [<'Ident "day">] -> tomorrow ()
	| [<'Ident "month">] -> next_month ()
	| [<'Ident "year">] -> next_year ()
	| [<>] -> raise (Stream.Error "Syntax error: 'day', 'month' or 'year' expected after 'next'") end
      |	[<'Ident "last"; str>] -> begin match str with parser
	| [<'Ident "day">] -> yesterday ()
	| [<'Ident "month">] -> last_month ()
	| [<'Ident "year">] -> last_year ()
	| [<>] -> raise (Stream.Error "Syntax error: 'day', 'month' or 'year' expected after 'last'") end

    let rec print = function
      |	None, y -> Syntax.toks_of_int y
      |	Some (None, m), y -> [print_month m; PP_tilda] @ Syntax.toks_of_int y
      |	Some (Some d, m), y -> Syntax.toks_of_int d @ [PP_tilda; print_month m; PP_tilda] @ Syntax.toks_of_int y

    let isvalue = function
      |	Some (Some d, m), y -> true
      | _ -> false

    let compare (dm1,y1) (dm2,y2) =
      if y1 < y2 then -1
      else if y1 > y2 then 1
      else match dm1, dm2 with
      | None, None -> 0
      |	None, _ -> -1
      |	_, None -> 1
      |	Some (d1,m1), Some (d2,m2) ->
	  if m1 < m2 then -1
	  else if m1 > m2 then 1
	  else match d1, d2 with
	  | None, None -> 0
	  | None, _ -> -1
	  | _, None -> 1
	  | Some n1, Some n2 ->
	      if n1 < n2 then -1
	      else if n1 > n2 then 1
	      else 0

    let entails (dm1,y1) (dm2,y2) =
      y1 = y2 &
      match dm1, dm2 with
      |	_, None -> true
      |	Some (d1,m1), Some (d2,m2) ->
	  m1 = m2 &
	  ( match d1, d2 with
	  | _, None -> true
	  | Some n1, Some n2 -> n1 = n2
	  | _ -> false)
      |	_ -> false

    let share d1 d2 = entails d1 d2 or entails d2 d1

    let conj d1 d2 =
      if entails d1 d2 then d1
      else if entails d2 d1 then d2
      else raise Not_found

    let le_min (dm1,y1) (dm2,y2) =
      if y1 < y2 then true
      else if y1 > y2 then false
      else match dm1, dm2 with
      |	None, _ -> true
      |	Some (Some 1, 1), _ -> true
      | Some  _, None -> false
      |	Some (d1,m1), Some (d2,m2) ->
	  if m1 < m2 then true
	  else if m1 > m2 then false
	  else match d1, d2 with
	  | None, _ -> true
	  | Some 1, _ -> true
	  | Some _, None -> false
	  | Some n1, Some n2 -> n1 <= n2

    let le_max (dm1,y1) (dm2,y2) =
      if y1 < y2 then true
      else if y1 > y2 then false
      else match dm1, dm2 with
      |	_, None -> true
      |	_, Some (Some 31, 12) -> true
      | None, Some _ -> false
      |	Some (d1,m1), Some (d2,m2) ->
	  if m1 < m2 then true
	  else if m1 > m2 then false
	  else match d1, d2 with
	  | _, None -> true
	  | _, Some d when d = length_month y2 m2 -> true
	  | None, Some _ -> false
	  | Some n1, Some n2 -> n1 <= n2

    let features = function
      |	None, y -> [true,(None, y)]
      |	Some (None, m), y -> [true,(Some (None, m), y); true,(None,y)]
      |	Some (Some d, m), y -> [true,(Some (Some d, m), y); true,(Some (None, m), y); true,(None,y)]

    let rec simpl = function
      |	None, y -> [<>]
      |	Some (None, m), y -> [<'None, y>]
      |	Some (Some d, m), y -> [<'Some (None, m), y>]

  end
