(* ocamlopt -I src str.cmxa nums.cmxa unix.cmxa lib/logfun.cmxa  date_solver.ml  *)

(*
 * >    Day = Top(Enum(Interval(Int)))
 * > 	ex: *; 12; 12..14; {12, 14..17}
 * >    Month = Top(Enum(Atom))
 * > 	ex: *; juin; {juin, aout}
 * >    Year = Top(Enum(Interval(Int)))
 * > 	ex: idem Day
 * >    Date_pattern = Prod(Day, Prod(Month, Year))
 * > 	ex: 20--juin--2006; {12, 20..24}--*--2006
 *)


module MyDate = Date_pattern.ForYoann

module MyPlugin = Logic.Plugin (MyDate)

let main () =
 try 
  while true do
    let x = read_line () in
    let y = read_line () in
    if 
      (if y = "IS_FORMULA?"
      then not (MyPlugin.isvalue x)
      else MyPlugin.entails x y
      )
    then print_endline "yes" 
    else print_endline "no"
  done
 with 
  | End_of_file -> () 


let _ = main()
