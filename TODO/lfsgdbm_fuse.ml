open Common
open Fullcommon

open Lfs

open Oassocdbm

open Common_fuse

open Ioplugins

let launch_db_and_lfs path_meta argv_fuse =
  let obj_to_path = (fun obj ->
     (* cos of ext2 limitation, cant have dire with more than 32000 files => have to split *)
     (* coupling: with check_world *)
      path_meta ^ "/files/" ^ (i_to_s (obj / 1000)) ^ "/" ^ (i_to_s obj))
  in
  let obj_to_filename = (fun obj -> obj_to_path obj ^ "/data") in

  begin

    Lfs._realfs := true;
    Lfs.core_get_path_data := (fun () -> path_meta);

    Lfs.core_get_fcontent__id  :=
      (function (Real o) -> read_file (obj_to_filename o) | _ -> raise Impossible);

    Lfs.core_set_fcontent__fst :=
      (function ((Core content), o) -> (Real o) | _ -> raise Impossible);

    Lfs.core_get_size_fcontent__slength :=
      (function (Real o) -> (try filesize (obj_to_filename o) with _ -> 0) +> size_mo_ko | _ -> raise Impossible);
      (*  why try ? cos perl first call ml to check if can mkfile, and then create file on disk (but normally no more pb cos dont get called) *)

    Lfs.core_update_fcontent__fst :=
      (function (content, o) ->
        (*  CONFIG,  can comment if want lazy update (or no update at all) *)
        write_file (obj_to_filename o) (content());
        (*  CONFIG, can comment if trust poffs :) *)
        ignore(Sys.command("mkdir " ^ (obj_to_path o) ^ "/RCS"));
        ignore(Unix.system("cd " ^ (obj_to_path o) ^ "; cp data _backup; echo bidon | ci -l _backup"));
        (Real o)
      );


    Lfs.hook_find_alogic :=  (fun id -> uninteract_logic (obj_to_filename id));
    Lfs.hook_find_transducer := (fun id prop conv -> (* IMPL *)
    (* Lfs.core_hook_transducer := (fun id prop -> ##SPEC *)
      if prop = (Prop "transducer:")
      then Left  (uninteract_transducer     (obj_to_filename id) obj_to_filename)
      else Right (uninteract_adv_transducer (obj_to_filename id) conv));  (* IMPL *)
      (* else AdvTrans (uninteract_adv_transducer (obj_to_filename id))); ##SPEC *)


    (* old: simple persistence
       let (data:(int * Lfs.world)) = get_value (path_meta ^ "/ocaml_world") in
       (Lfs.w := snd data;
       Common._counter := fst data
       )
       old: no persistence
       (Lfs.w := Lfs.default_world;
       Common._counter := 2;
       );
    *)
    (* rescue: let init_not_reload = false in let _ = (Common._counter := 300000; Common._counter2 := 300000 * 20) in *)
    let init_not_reload = not (Sys.file_exists (path_meta ^ "/counter")) in
    if init_not_reload
    then Common._counter := 2 (*  inode must be at least > 1, to avoid conflict with root inode = 1. as file id are inode id => same constraint , TODO freeobj *)
    else (Common._counter  := get_value (path_meta ^ "/counter");
	  Common._counter2 := get_value (path_meta ^ "/counter2");
	 );
    let adapt = fun f o -> if init_not_reload then f o else o in


    (* CONFIG *)
    let size_buffer = 500 in
    let caching db = new oassoc_buffer size_buffer db in
    (* !!!!!!!!!!!!!!!!!!!!!!! *)
    (*  let caching db = db in ## take care cos if not buffered, many stuff slow down (specially context creation) *)

    (*  I now use GDBM (easier to install and less segfault (but no transaction)) *)
    (* lfs *)
    let gchildrendb = Dbm.opendbm (path_meta ^ "/children") [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in
    let gparentsdb  = Dbm.opendbm (path_meta ^ "/parents") [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in

    let iprop_propdb = Dbm.opendbm (path_meta ^ "/iprop_prop") [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in
    let prop_ipropdb = Dbm.opendbm (path_meta ^ "/prop_iprop") [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in
    let cache_is_formuladb = Dbm.opendbm (path_meta ^ "/cache_is_formula") [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in

    let filesdb = Dbm.opendbm (path_meta ^ "/filesdb") [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in
    let extfilesdb = Dbm.opendbm (path_meta ^ "/extfiles") [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in

    let children_assoc = (caching (new oassocdbm [] gchildrendb (fun osetb -> osetb#tosetb) (fun setb -> new osetb setb)     ) ) in
    let parents_assoc =  (caching (new oassocdbm [] gparentsdb  (fun osetb -> osetb#tosetb) (fun setb -> new osetb setb)     ) ) in
    let prop_iprop_assoc = (caching (new oassocdbm [] prop_ipropdb id id)) in
    let iprop_prop_assoc = (caching (new oassocdbm [] iprop_propdb id id)) in
    let cache_is_formula_assoc = (caching (new oassocdbm [] cache_is_formuladb (fun oset -> oset#tosetb) (fun set -> new osetb set))) in
    let files_assoc = (caching (new oassocdbm [] filesdb id id)) in
    let extfiles_assoc = (caching (new oassocdbm [] extfilesdb  (fun oset -> oset#toseti) (fun set -> new oseti set)     ) ) in (*  OPT5 *)
        (* IFNOT OPT5  let extfiles_assoc = (caching (new Bdbo.oassocbtree [] extfilesdb  (fun oset -> oset#tosetb) (fun set -> new osetb set)     ) ) in *)


    (* pof *)
    let _count = ref 0 in                                                                                                                                                                 (* IMPL *)
    let _ = Unix.system ("rm -f " ^ path_meta ^ "/*extparts*") in                                                                                                                          (* IMPL *)
    let _ = Unix.system ("rm -f " ^ path_meta ^ "/*partsinfo*") in                                                                                                                         (* IMPL *)
    let _ = Unix.system ("rm -rf " ^ path_meta ^ "/files/2[0-9][0-9][0-9]/*") in (*  the views *)
(*
    Lfs.empty_extparts := (fun () ->                                                                                                                                                      (* IMPL *)
      let _ = incr _count in                                                                                                                                                              (* IMPL *)
      let newdb  = Dbm.opendbm (path_meta ^ "/extparts" ^ i_to_s !_count)  [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in
      (* coupling: must be same seti than in empty_ext                                                                                                                                     ##IMPL *)
      caching (new oassocdbm [] newdb (fun oset -> oset#toseti) (fun set -> new oseti set)) (*  OPT5                                                                             ##IMPL *)
        (* IFNOT OPT5 caching (new Bdbo.oassocbtree [] newdb (fun oset -> oset#tosetb) (fun set -> new osetb set))                                                                      ##IMPL *)
	);                                                                                                                                                                                (* IMPL *)

  *)                                                                                                                                                                                      (* IMPL *)

    Lfs.empty_partsinfo := (fun () ->                                                                                                                                                     (* IMPL *)
     let _ = incr _count in                                                                                                                                                               (* IMPL *)
     let newdb = Dbm.opendbm (path_meta ^ "/partsinfo" ^ i_to_s !_count) [Dbm.Dbm_create;Dbm.Dbm_rdwr] 0o777 in
     (* note: i bufferise partsinfo just for the grouping of io, not for the multiple update gain, cos never happened for this assoc                                                       ##IMPL *)
     caching (new oassocdbm [] newdb id id)                                                                                                                                        (* IMPL *)
      );                                                                                                                                                                                  (* IMPL *)
                                                                                                                                                                                          (* IMPL *)


    Lfs.w := { default_world with                                                                                                                                                         (* IMPL *)
      graphp = (new ograph2way                                                                                                                                                            (* IMPL *)
		  children_assoc                                                                                                                                                          (* IMPL *)
		  parents_assoc                                                                                                                                                           (* IMPL *)
		  emptysb                                                                                                                                                                 (* IMPL *)
	       )                                                                                                                                                                          (* IMPL *)
        +> adapt (fun o -> o#add_node iroot);                                                                                                                                             (* IMPL *)
      prop_iprop = prop_iprop_assoc                                                                                                                                                       (* IMPL *)
	+> adapt (fun o -> o#add (root, iroot));                                                                                                                                          (* IMPL *)
      iprop_prop = iprop_prop_assoc                                                                                                                                                       (* IMPL *)
	+> adapt (fun o -> o#add (iroot, root));                                                                                                                                          (* IMPL *)
      cache_is_formula = cache_is_formula_assoc;                                                                                                                                                       (* IMPL *)
                                                                                                                                                                                          (* IMPL *)
      files = files_assoc;                                                                                                                                                                (* IMPL *)
      extfiles = extfiles_assoc                                                                                                                                                           (* IMPL *)
	+> adapt (fun o -> o#add (iroot, empty_ext ()));                                                                                                                                  (* IMPL *)
    };                                                                                                                                                                                    (* IMPL   *)




    Common_fuse.main path_meta ((fun () -> ()), (fun () -> ()), (fun () -> ())) argv_fuse;

    (try
      begin
	(*  generic are in fact flush, to use if use with caching (oassoc_buffer, cos table are buffered) *)
	children_assoc#generic;
        parents_assoc#generic;
        prop_iprop_assoc#generic;
        iprop_prop_assoc#generic;
        cache_is_formula_assoc#generic;
        files_assoc#generic;
        extfiles_assoc#generic;

(*       Txn.commit !t None; *)

(*
        Dbm.close gchildrendb;
        Dbm.close gparentsdb;
        Dbm.close prop_ipropdb;
        Dbm.close iprop_propdb;
        Dbm.close cache_is_formuladb;
        Dbm.close filesdb;
        Dbm.close extfilesdb;
*)
      end
     with _ -> ()
     );
     let _ = Timing() in
     write_value !Common._counter (path_meta ^ "/counter");
     write_value !Common._counter2 (path_meta ^ "/counter2");

    ()
  end



(************************************************************************************)
let _ =
  begin
    Lfs.copyright ();
    let arg1 = ref None in
    let arg2 = ref None in
    let options = Arg.align
      [ "-fastfsck", Arg.Set Flag.fast_fsck, "  do not check for coverability";
        "-fsckonly", Arg.Set Flag.fsck_only, "  launch fsck and exit";
        "-timeout",  Arg.Set_int Flag.timeout, "  set number of seconds before interrupting lfs or buggy external plugins";
        "-nostat",   Arg.Set Flag.nostat, "  dont launch stat";
        "-nofsck",   Arg.Set Flag.nofsck, "  dont launch fsck";
        "-command",  Arg.Set Flag.exec_internal_command, "  launch only the internal command specified in lfsbdb_fuse.ml";
        "-notransact",  Arg.Set Flag.notransact, "  dont use transaction";
      ] in
    let usage_msg =
      ("Usage: " ^ basename Sys.argv.(0) ^ " [options] <LFS-meta-data-path> <mountpoint>\nOptions are:") in
    Arg.parse
      options
      (fun s ->
        match !arg1 with
        | None -> arg1 := Some s
        | Some arg1 ->
            (arg2 := Some s;
             launch_db_and_lfs arg1 (Array.of_list ((Sys.argv.(0)::"-s"::"-o"::"max_read=16384,allow_other"::[s])))
            )
      )
      usage_msg;
    (match (!arg1, !arg2) with
    | (Some _, Some _) -> ()
    | _ -> Arg.usage options usage_msg
    );

  end

(*
let _ =
  Lfs.copyright ();
  match (Array.to_list Sys.argv ) with
  | x::y::ys ->
      begin
        Sys.chdir y;
        launch_db_and_lfs (Sys.getcwd()) (Array.of_list (x::"-s"::"-o"::"max_read=16384,allow_other"::ys))
      end
  | _ -> failwith "give me a path, and a mountpoint, where are LFS meta-data ?"



*)
