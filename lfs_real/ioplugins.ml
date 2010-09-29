open Common

open Lfs

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* file to plugins, kind of 'eval' as in scheme *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type idfile = objet


(*****************************************************************************)

(* coupling: if change interface of plugins, then have to change 
 * common_logic.ml to put the good interact 
 *)
let (uninteract_logic_old: filename -> logic) = fun file -> 
  Common.command2 ("chmod a+x " ^ file); (* obsolete? *)
  fun (Prop s1) (Prop s2) -> 
    log2 ("logic called:" ^ file ^ ":" ^ s1 ^ ":" ^ s2);
    let chan = Unix.open_process_in 
      (file ^ " \"" ^ s1 ^ "\" " ^ "\"" ^ s2 ^ "\"") (* robust:? escaped? *)
    in
    let x = 
      match (try input_line chan with End_of_file -> "no output") with 
      | "yes" -> true
      | "no"  -> false 
      | s -> failwith ("pb uninteract:" ^ s)
        (* Will be catch by the try in cd. But if comes from a cd parts
         * will also stop that => handle that by intercepting it in  
         * check_and_add and by relaunching the solver when the solver
         * process dies.
         *)
    in 
    ignore (Unix.close_process_in chan); 
    x (* robust: timeout, use select ?  *)


(* fast_comm logic *)
let (uninteract_logic: filename -> logic) = fun file -> 
  Common.command2 ("chmod a+x " ^ file); (* obsolete? *)
  log2 ("new pipe with logic");
  let (inc, outc) = Unix.open_process file in (* IMPL *)
  (* obsolete: 
   *  Lfs._pending_logic_process := true; 
   *  Lfs._in_logic := inc; 
   *  Lfs._out_logic := outc in ##IMPL 
   *)
  fun (Prop s1) (Prop s2) -> 
    (* let (inc, outc) = Unix.open_process file in ##SPEC *)
    log2 ("logic called:" ^ file ^ ":" ^ s1 ^ ":" ^ s2);
    Common.unwind_protect (fun () -> 
      output_string outc (s1 ^ "\n");
      output_string outc (s2 ^ "\n"); 
      flush outc;
      log3 ("here1");
      let x = 
        match (try input_line inc with End_of_file -> "no output") with 
        | "yes" -> true
        | "no"  -> false 
        | s -> failwith ("pb uninteract:" ^ s)
        (* will be catch by the try in cd, but if comes from a cd parts
         * will also stop that *)
      in  
      log3 ("here2");
      (* let _ = Unix.close_process (inc, outc) in ##SPEC *)
      x) 
      (* robust: timeout, use select ?  *)
      (fun e -> 
        log2 "closing logic";
        ignore(Unix.close_process (inc, outc));
        log2 "closed logic";
      )
           

(*****************************************************************************)
(* Work only when realfs, cos suppose that content is a path to a file.
 * Seems ugly restriction but simplify:
 *  - cos dont want change core mode (contents is string list)
 *  - cos dont want change real mode by changing interface of transducer
 *    by reading the contents on their stdin
 *    cos sometimes binary files, ...
 * 
 * Could make it work too for core by first creating in /tmp a fake file
 * and then launch the trans on it, mais bon ... 
 *)
let (uninteract_transducer: filename -> (idfile -> filename) -> transducer) = 
 fun file obj_to_filename -> 
  Common.command2 ("chmod a+x " ^ file); (* obsolete? *)
  function 
  | (Real o) -> 
      log2 ("trans called:" ^ file);

      (try (
        (* robust:? escaped? *)
        (* robust: do a timeout ?  *)
        let chan = 
          Unix.open_process_in (spf "%s '%s'" file ((obj_to_filename o))) in
        (* map_set *)
        let x = 
          input_line chan +> Common.split "/" +> map (fun s -> Prop s) in 
        ignore(Unix.close_process_in chan); 
        x
      ) 
        with 
        | Timeout -> raise Timeout
        | e -> Common.empty_list
      )

  | _ -> raise Impossible 


let (uninteract_adv_transducer: filename -> (property -> iproperty option) -> adv_itransducer) = 
 fun file conv -> (* IMPL *)
 (* let (uninteract_adv_transducer: filename -> adv_transducer) = fun file -> ##SPEC *)

  Common.command2 ("chmod a+x " ^ file); (* obsolete? *)
  fun contents -> 
    log2 ("advtrans called:" ^ file);
    try (
      (* note: 
       *  - cant do it without threads
       *  - cant do:  begin send all info;  get all info end  
       *    (produce all; consume all) 
       *    because pipe have limited buffer. So if the transducer start to
       *    deliver some info, at a moment, as I wait to produce all before
       *    consume, the transducer will get block an an output and so will
       *    not do input. So in turn it blocks the output of lfs (deadlock)
       * 
       * note that if transducer first start to read all
       * the file and then deliver, then could do it without thread
       * (all my adv_transducer in ml do that), but bad to impose this. 
       *)
  
      let (inc, outc) = Unix.open_process file in 
      let _producer = Thread.create (fun () -> 
        contents +> List.iter (fun s -> output_string outc s);
        log2 ("advtrans: finished producing");
        close_out outc;
      ) () in
  
      let res = ref [] in
      let consommer = Thread.create(fun () -> 
        let rec aux () = 
          log3 ("advtrans:trying");
          try 
            let x = 
              input_line inc 
              +> split "/"
              +> map (fun s -> Prop s) 
              +> map_filter conv (* map_set *)  (* IMPL *)
              +> (fun x -> if x = [] then [Lfs.iroot] else x)                (* IMPL *)
                (* +> map (fun s -> Prop s)                   ##SPEC *)
                (* +> (fun x -> if x = [] then [root] else x) ##SPEC *)
            in 
            log3 ("advtrans: having");
            x::aux() 
          with End_of_file -> []
        in 
        res := aux (); 
      ) () in
      Thread.join consommer;

      log2 (spf "contents=%d, !res=%d\n" (length contents) (length !res));
      assert (length contents = length !res);
      ignore(Unix.close_process (inc, outc)); 
      !res (* robust: timeout  *)
    )
    with 
    | Timeout -> raise Timeout
    | _ -> contents +> List.map (fun x -> empty_list)

