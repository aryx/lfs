open Common

type property = Prop of string
type transducer = (filecontent -> property set)
      and filecontent = string

let (interact_transducer: transducer -> unit) = fun trans -> 
  match (Array.to_list Sys.argv ) with
  | [_;x] -> read_file x +> trans +> map (fun (Prop s) -> s) +> join "/" +> print_endline
  | _ -> failwith "PB:"

