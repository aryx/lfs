module OptParam = struct let none = [] end
module IntervParam = struct let verbose = true end

module NamesInt = struct let names = ["year";"length";"volume";"number";"size";"w";"h";"x";"y";"z"] end
module AttrIntervInt = Pair.Make (Attr.Make(NamesInt)) (Opt.Make OptParam (Interv.Make(IntervParam)(Int.Make)))

module NamesDate = struct let names = ["date"] end
module AttrIntervDate = Pair.Make (Attr.Make(NamesDate)) (Opt.Make OptParam (Interv.Make(IntervParam)(Date.Make)))

module NamesTime = struct let names = ["time"] end
module AttrIntervTime = Pair.Make (Attr.Make(NamesTime)) (Opt.Make OptParam (Interv.Make(IntervParam)(Time.Make)))

module NamesAuthor = struct let names = ["author"] end
module ToolsAuthor : Sentence.PARAM =
  struct
    let normalize s = String.lowercase s
    let normalize_is = normalize
    let separator c = List.mem c [' ';'\t';'\n';',';';']
    let words s = List.fold_left (fun res s' -> (false,(true,String.sub s' 0 1,false))::(true,(true,s',true))::res) [] (Str.split (Str.regexp " +and +") s)
  end
module AttrAuthor = Pair.Make (Attr.Make(NamesAuthor)) (Opt.Make OptParam (Sentence.Make(ToolsAuthor)))

module NamesKeywords = struct let names = ["keywords"] end
module ToolsKeywords : Sentence.PARAM =
  struct
    let normalize s = String.lowercase s
    let normalize_is = normalize
    let separator c = List.mem c [' ';'\t';'\n';',';';']
    let words s = List.fold_right (fun s' l -> (false,(true,String.sub s' 0 1,false))::(true,(true,s',false))::(false,(false,String.sub s' (String.length s' -1) 1,true))::(true,(false,s',true))::(true,(true,s',true))::l) (Str.split (Str.regexp " *[,;] *") s) []
  end
module AttrKeywords = Pair.Make (Attr.Make(NamesKeywords)) (Opt.Make OptParam (Sentence.Make(ToolsKeywords)))

module NamesOptSentence = struct let names = [] end
module Tools : Sentence.PARAM =
  struct
    let normalize s = String.lowercase s
    let normalize_is = normalize
    let separator c = List.mem c [' ';'\t';'\n';',';'.';';';'?';'!';':';'\'';'(';')';'@';'{';'}';'[';']']
    let stopword s = String.length s <= 3 or List.mem s ["from";"with";"avec";"pour"]
    let words s = List.fold_right (fun s' l -> (false,(true,String.sub s' 0 1,false))::(true,(true,s',false))::(true,(true,s',true))::l) (Syntax.split (normalize,separator,stopword) s) []
  end
module AttrOptSentence = Pair.Make (Attr.Make(NamesOptSentence)) (Opt.Make OptParam (Sentence.Make(Tools)))

module Attrval =
  Opt.Make
    (struct let none=[Token.Interro] end)
    (Sum.Make
       (AttrIntervInt)
       (Sum.Make
	  (AttrIntervDate)
	  (Sum.Make
	     (AttrIntervTime)
	     (Sum.Make
		(AttrAuthor)
		(Sum.Make
		   (AttrKeywords)
		   (AttrOptSentence))))))

module HasParam = struct let toks = [] end

module Log = Conj.Make(struct let top = true let sep = Some Token.Comma let lgg = false end)(Has.Make HasParam Attrval)


(** Small program prompting the user to enter formulas, and answering whether this is syntactically correct. *)

let _ =
  try
    while true do
      print_endline "\nEnter a formula (or 'exit' to quit):";
      let line = read_line () in
      if line = "exit" then raise End_of_file;
      try
        match Syntax.from_string line with parser
        | [<_ = Log.parse; _ = Stream.empty>] -> print_endline "OK"
      with Stream.Error msg -> print_endline (if msg="" then "Syntax error" else msg)
    done
  with
  | End_of_file -> print_endline "Good bye"
  | e -> print_endline (Printexc.to_string e)
