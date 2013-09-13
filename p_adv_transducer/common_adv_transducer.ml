open Common

type property = Prop of string
type adv_transducer = (partcontent list -> (property set) list)
      and partcontent = string

(* make ml adv_transducer stdin(argv)/stdout compliant *)
let (interact_adv_transducer: adv_transducer -> unit) = fun advtrans ->
  let rec aux () =
    try let x = read_line () in x::aux()
    with End_of_file -> [] in
  aux () +> advtrans +> iter (fun ps -> ps +> map (fun (Prop s) -> s) +> join "/" +> print_endline)

