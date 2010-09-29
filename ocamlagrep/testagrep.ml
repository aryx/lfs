(* Test harness *)

open Printf

let numerrs = ref 0
and wholeword = ref false
and verbatim = ref false
and fromfile = ref false

let do_search p s =
  if !fromfile then begin
    let ic = open_in s in
    begin try
      while true do
        let l = input_line ic in
        let n = Agrep.errors_substring_match p
                  ~numerrs:!numerrs ~wholeword:!wholeword
                  l ~pos:0 ~len:(String.length l) in
        if n < max_int then printf "%d %s\n" n l
      done
    with End_of_file -> ()
    end;
    close_in ic
  end else begin
    let n = Agrep.errors_substring_match p
              ~numerrs:!numerrs ~wholeword:!wholeword
              s ~pos:0 ~len:(String.length s) in
    if n = max_int
    then printf "No match"
    else printf "Match, with %d error(s)" n;
    print_newline()
  end

let _ =
  let pattern = ref None in
  Arg.parse
    ["-1", Arg.Unit(fun () -> numerrs := 1), "  one error";
     "-2", Arg.Unit(fun () -> numerrs := 2), "  two error";
     "-3", Arg.Unit(fun () -> numerrs := 3), "  three error";
     "-e", Arg.Int(fun n -> numerrs := n), "<n>  n errors";
     "-f", Arg.Set fromfile, "  search in given file rather than in string";
     "-w", Arg.Set wholeword, "  match entire words";
     "-v", Arg.Set verbatim,  "  match string verbatim (no special chars)"]
    (fun s ->
      match !pattern with
        None ->
          pattern := Some(if !verbatim then Agrep.pattern_string s
                                       else Agrep.pattern s)
      | Some p -> do_search p s)
    "Usage: testagrep [options] <pattern> <string>\nOptions are:"
