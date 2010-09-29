open Common
open Common_transducer

(* fake mp3 transducer, just to test *)
let c_transducer = fun s -> 
  let xs = lines_with_nl s in
  xs +> fold (fun acc s -> 
    if s =~ "^\\([A-Za-z_]*\\)[ ]*(" then
      acc#add (matched1 s)
    else 

      if s =~ "^[^ \t].*[^A-Za-z]\\([A-Za-z_]+\\)[ ]*(" then
      acc#add (matched1 s)
      else acc
             ) (new osetb Setb.empty)
   +> (fun set -> set#tolist)
   +> map (fun s -> (Prop ("function:" ^ s)))

let _ = interact_transducer c_transducer
