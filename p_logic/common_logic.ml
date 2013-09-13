open Common

type property = Prop of string

type logic    = (property -> property -> bool)  (* mean: a |= b ? *)

(* make ml solver stdin(argv)/stdout compliant *)
let (interact_logic_old: logic -> unit) = fun (|=)  ->
  let xs = Array.to_list Sys.argv in
  let xs = xs +> Common.exclude (fun s -> s = "-test") in

  match xs  with
  | [_;x;y] ->
      if (Prop x) |= (Prop y)
      then print_endline "yes"
      else print_endline "no"
  | _::xs ->
      failwith ("give me 2 formulas, not " ^
                   (i_to_s (length xs)) ^
                   "\nI was given:\n" ^
                   (xs +> join "\n") ^ "\n")
  | _ -> raise Impossible
  (* can also return exception *)


(* fast_logic: now take also a 'is_formula' in param *)
let (interact_logic: logic -> (property -> bool) -> unit) =
  fun (|=) is_formula ->
    try
      while true do
        let x = read_line () in
        let y = read_line () in
        if
          (if y = "IS_FORMULA?"
          then is_formula (Prop x)
          else  (Prop x) |= (Prop y)
          )
        then print_endline "yes"
        else print_endline "no"
      done
    with
    | End_of_file -> ()
    | x -> raise x


let interact_logic (|=) is_formula =
  if not !Sys.interactive then
    if List.mem "-test" (Array.to_list Sys.argv)
    then interact_logic_old (|=)
    else interact_logic (|=) is_formula
  else ()
