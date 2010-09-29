(*
ocamlc str.cma unix.cma -I ~/lfs_code/commons/ ~/lfs_code/commons/dumper.cmo  ~/lfs_code/commons/commonop.cmo ~/lfs_code/commons/common.cmo index_LFS_with_glimpse.ml
*)

(*
*.PDF.gz        glimpse-filter-pdf-pdfgz-use-pdftotext.sh -z	<
*.PDF   glimpse-filter-pdf-pdfgz-use-pdftotext.sh < 
*.xls   glimpse-filter-xls-use-catdoc.sh <
*.doc   glimpse-filter-doc-use-catdoc.sh <
*)

open Common


let path_meta = "."

let _Timeoutval = 20

let timeout_function = Common.timeout_function _Timeoutval

(* true = correct execution, false = pb *)
let timeout_command_and_exit = fun s -> 
  let pid = Unix.fork () in
  if pid = 0
  then
    begin
      try
       timeout_function (fun () -> 
        (* some tool does not honor the return value convetion, so look to stderr *)
        let x = Sys.command (s ^ " 2>/tmp/timeout_err") in
        (* CONFIG *)
        (*if filesize "/tmp/timeout_err" > 0
        then exit (-1)
        else *) exit x;
      ) with
        Timeout -> 
          begin
            pr2 "TIMEOUT"; 
(*
            Unix.kill (Unix.getpid()) Sys.sigint; 
            pr2 "here"; 
 *)
            exit (-1);
          end
      | e -> raise e
    end
  else 
    let (pid2,process_status ) = (Unix.waitpid [] pid) in
    assert(pid = pid2);
    match process_status with
    | Unix.WEXITED 0 -> true
    | _ -> false



let process s = 

  let extract_dir s = 
    if s =~ ".*files/\\([0-9]+/[0-9]+\\)/" 
    then matched1 s
    else raise Impossible
  in

  let do_command s commandname = 
    pr2 ("processing:" ^ s);
    let path = "glimpse/cache_files/" ^ (extract_dir s) in
    command2 ("mkdir -p " ^ path);
    timeout_command_and_exit (commandname  s path)
      +> (fun b -> if not b then begin pr2 "erasing and killing"; command2 ("rm -f " ^ path ^ "/data.txt") end;)
  in    
  begin    

  match s with
  | _ when s =~ ".*\\.pdf$" -> 
      do_command s (fun s path -> sprintf "pdftotext \"%s\" %s/data.txt" s path)
  | _ when s =~ ".*\\.pdf.gz$" -> 
      command2 (sprintf "zcat -f \"%s\" > /tmp/toto.pdf"  s);
      do_command s (fun _s path -> sprintf "pdftotext /tmp/toto.pdf %s/data.txt" path) ;


  | _ when s =~ ".*\\.ps$" -> 
      pr2 ("processing:" ^ s);
      let path = "glimpse/cache_files/" ^ (extract_dir s) in
      command2 ("mkdir -p " ^ path);
      timeout_command_and_exit (sprintf "pstotext -output %s/data.txt \"%s\" " path s)
        +> (fun b -> 
          if not b then 
            begin 
              pr2 "erasing and killing"; 
              command2 ("rm -f " ^ path ^ "/data.txt");

              pr2 "second tentative";
              timeout_command_and_exit (sprintf 
                                         "ps2ascii \"%s\" %s/data.txt  "
                                          (* "gs -q -dNODISPLAY -dSAFER -dDELAYBIND -dWRITESYSTEMDICT -dSIMPLE -c save -f ps2ascii.ps \"%s\"  -c quit >  %s/data.txt  "   *)
                                          s path)
                +> (fun b -> 
                  if not b then 
                    begin 
                      pr2 "erasing and killing"; 
                      command2 "pkill gs";
                      command2 ("rm -f " ^ path ^ "/data.txt");
                    end);
            end
           )
  | _ when s =~ ".*\\.ps.gz$" -> 
      command2 (sprintf "zcat -f \"%s\" > /tmp/toto.ps"  s);

      pr2 ("processing:" ^ s);
      let path = "glimpse/cache_files/" ^ (extract_dir s) in
      command2 ("mkdir -p " ^ path);
      let s = "/tmp/toto.ps" in
      timeout_command_and_exit (sprintf "pstotext -output %s/data.txt \"%s\" " path s)
        +> (fun b -> 
          if not b then 
            begin 
              pr2 "erasing and killing"; 
              command2 ("rm -f " ^ path ^ "/data.txt");

              pr2 "second tentative";
              timeout_command_and_exit (sprintf "ps2ascii \"%s\" %s/data.txt  " s path)
                +> (fun b -> 
                  if not b then 
                    begin 
                      pr2 "erasing and killing"; 
                      command2 "pkill gs";
                      command2 ("rm -f " ^ path ^ "/data.txt");
                    end);
            end
           )

  | _ -> ()
  end
      

let main () =
  if (not (Sys.file_exists "lfs_secu"))
  then 
    begin
      pr2 "There is no 'lfs_secu' file in directory. You must launch this command from the LFS meta-data directory.";
      exit 0
    end
  else 
    begin
      command2 "mkdir -p glimpse";
      command2 "rm -rf glimpse/cache_files";
      pr2 "rm finished";
      command2 "mkdir  glimpse/cache_files";
      command2 "echo '*_backup'   > glimpse/.glimpse_exclude";
      command2 "echo '*_backup,v' >> glimpse/.glimpse_exclude";

      command2 "rm -rf glimpse/recent_files";
      pr2 "rm finished";
      command2 "mkdir -p glimpse/recent_files";
      command2 "mkdir -p glimpse/recent_files/fake_start";
      command2 "echo 'padioleau yoann' > glimpse/recent_files/fake_start/start.txt";
      command2 "echo 'padioleau yoann' > glimpse/recent_files/fake_start/start2.txt";

      command2 "echo '*_backup'   > glimpse/recent_files/.glimpse_exclude";
      command2 "echo '*_backup,v' >> glimpse/recent_files/.glimpse_exclude";

      let files = process_output_to_list "find files/ -type f" in
      pr2 "find finished";
      let files = files +> List.filter (fun s -> not ((s =~ ".*/_backup$") || (s =~ ".*/RCS/_backup,v$"))) in
      (* files +> List.iter pr2;*)
      files +> List.iter process;
(*      
      command2 "/usr/local/bin/glimpseindex -o -H glimpse/ files/ glimpse/cache_files/";
      command2 "/usr/local/bin/glimpseindex -o -H glimpse/recent_files/ glimpse/recent_files/fake_start";
*)
    end


let _ = main ()
