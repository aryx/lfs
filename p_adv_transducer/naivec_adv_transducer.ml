open Common

open Common_adv_transducer

(* very rudimentary, just to test *)
let c_adv_transducer = fun xs -> 
  let state = ref None in
  xs +> map (fun s -> 
    let _ = 
      if s =~ "int [a-zA-Z0-9]+(int [a-zA-Z0-9]+) {"
      then state := Some (Prop ("function:" ^ (regexp_match s "int \\([a-zA-Z0-9]+\\)("))) 
    in
    (match !state with None -> empty_list | Some x -> set [x]) $+$
    (if s =~ "int [a-zA-Z0-9]+(int [a-zA-Z0-9]+) {" then set [Prop "synchro"] else empty_list) $+$
    (if s =~ ".*fprintf(stderr" then set [Prop "debugging"] else empty_list) $+$
    (if s =~ ".*assert"           then set [Prop "error"] else empty_list) $+$
    (words s +> set +> filter (fun s -> member s ["x";"y";"z";"w";"i";"j"])
       +> map (fun s -> Prop ("var:" ^ s)))
  )

let _ = interact_adv_transducer c_adv_transducer
