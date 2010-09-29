open Common

open Common_adv_transducer

(* TODO better use of the keyword=  field cos can be on more than one lines, 
   in fact same pb for other entries => perhaps better pack pre processing (gather some lines)

 bug: if author is 'michand Brandt' => je vois le and :) faut des \b dans le split
*)
(* better domain: 
    use classification=  (cf 100_000.bib for example) 
    try do as in bibtex (suppr a/of/in/for/with/on, try make complex domain), split [ \t:], or have a dictionnary? (take acm classification) 
    inspire bibtex2html ? have tricks to infer from title ?
*)


let bibtex_adv_transducer = fun xs -> 
  let state = ref empty_list in     (* THE INTERNAL STATE *)
  let rec aux = function
    | [] -> []
    | s::xs -> 
	let props = 
	  if s =~ "[ \t]*@\\(.*\\){\\(.*\\)," then  (* @ symbol => we've got a new entry, such as @book{... *)
	    let (typ, ref) = matched2 s in
	    let xs' = take_until (fun s -> s =~ "[ \t]*}") xs in 
	    let props = [Prop ("typeref:" ^ typ); Prop ("ref:" ^ ref)] in
	    let _ = state :=
	      xs' +> fold (fun a s -> 
		(
		 match () with
		 | _ when s =~ "[ \t]*author[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     matched 1 s +> split "[ \t]*and[ \t]*" +> map (fun s -> Prop ("author:" ^ s))
		 | _ when s =~ "[ \t]*year[ \t]*=[ \t]*\"?\\([0-9]+\\)" -> 
		     [Prop ("year:" ^ matched 1 s)]
		 | _ when s =~ "[ \t]*institution[ \t]*=[ \t]*\"\\(.*\\)\"," -> 
		     [Prop ("institution:" ^ matched 1 s)]
		 | _ when s =~ "[ \t]*title[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("title:" ^ matched 1 s)]
                 | _ when s =~ "[ \t]*keywords[ \t]*=[ \t]*[\"{]\\([^\"]*\\)" -> 
		     matched 1 s +> split "[ \t]*[,;][ \t]*" +> map (fun s -> Prop ("domain:" ^ s))

                  (* for irisa.bib *)
		 | _ when s =~ "[ \t]*typededocument[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("typeref:" ^ matched 1 s)]
		 | _ when s =~ "[ \t]*titre[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("title:" ^ matched 1 s)]
		 | _ when s =~ "[ \t]*auteur[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("author:" ^ matched 1 s)]
		 | _ when s =~ "[ \t]*motclef[ \t]*=[ \t]*[\"{]\\(.*\\)[\"}]," -> 
		     [Prop ("domain:" ^ matched 1 s)]
		 | _ when s =~ "[ \t]*datededition[ \t]*=[ \t]*[\"{][0-9][0-9]-\\([0-9]+\\)[\"}]," -> 
		     [Prop ("year:" ^ matched 1 s)]
		 | _ when s =~ "[ \t]*datededition[ \t]*=[ \t]*[\"{]\\([0-9]+\\)[\"}]," -> 
		     [Prop ("year:" ^ matched 1 s)]


		 | _ -> []
		) ++ a
			  ) props in
	    (Prop "synchro")::!state (* SYNCHRONISATION POINT,  a new bibtex entry start a new unit *)
	  else !state in
	props::aux xs
  in
  aux xs

let _ = interact_adv_transducer bibtex_adv_transducer

