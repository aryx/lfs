open Common

open Oset (* $??$ *)

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* An LFS mode that is a wrapper over an existing hierarchy of files.
 * Smoother (and in the end better) than switching completely to a
 * "full" real mode LFS.
 * 
 * Quite similar to lfs_real.ml and lfs_bdb.ml. In fact started as a
 * copy-paste. 
 * 
 * Can use as-is mount.ml. Just have to build the good
 * meta-data and symbolic links for data. 
 * update: in fact can also avoid the link with a special mode, 
 * so have real_mode, fake_mode, and now semi_real_mode :)
 * 
 *)

(*****************************************************************************)
(* Database *)
(*****************************************************************************)
(* LFS use a global Lfs.w so we cant really have multiple databases at
 * the same time, so create_db in fact return a fake database
 * handler. 
 *)

type database = {
  metapath : string;
  lfs_world: Lfs.world ref; (* just a shortcut to Lfs.w *)
  transact: Lfs_persistent.database;

  (* to replace the files/x/y/data symlink tech used in the real mode *)
  real_path: (Lfs.objet, filename)   Oassoc.oassoc;
}


(*****************************************************************************)
(* Open/Create db, can be used without launch_semireal to e.g. build a db  *)
(*****************************************************************************)

let open_db ~metapath ~use_transact = 

  (* Lfs_real.launch_real metapath; *)
  Flag_lfs.size_buffer_oassoc_buffer := 100000;
  
  let database = Lfs_bdb.launch_bdb metapath use_transact in
  let o_realpath_db, o_realpath = Oassocdbm.create_dbm metapath "/real_path" in

  let db = { 
    lfs_world = Lfs.w;
    metapath = metapath;
    transact = { database with
      (* maybe not needed *)
      Lfs_persistent.final = (fun () -> 
        database.Lfs_persistent.final ();
        Dbm.close o_realpath_db;
      );
    };
    real_path = o_realpath;
  } in
  begin
    (* Lfs.stat_world !(db.lfs_world); *)
    Lfs.lfs_check := false; (* faster mkfile *)
    db.transact.Lfs_persistent.commit();
    
    db
  end


let close_db db =
  db.transact.Lfs_persistent.final(); (* also close gdbm tables *)
  ()



let clean_metapath metapath = 
  let res = Common.command2_y_or_no (spf "rm -f %s/*" metapath) in
  if not res then failwith "exiting";

  Common.command2 (spf "mkdir -p %s" metapath);
  Common.command2(spf "touch %s/lfs_secu" metapath);
  ()



let create_db ~metapath ~use_transact = 
  clean_metapath metapath;
  let db = open_db ~metapath ~use_transact in
  db






(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let (obj_to_filename: database -> Lfs.idfile -> string) = 
 fun db -> fun obj -> 
   db.real_path#assoc obj(* +> Common.before_leaving (fun x -> log x)*)


(*****************************************************************************)
(* Check world semi real *)
(*****************************************************************************)
let fsck_fast           = ref true
let rm_obj_if_pb        = ref false

let check_world_semireal db metapath = fun w -> 

  let iroot = w.Lfs.prop_iprop#assoc Lfs.root in

  let max_o = ref 0 in

  pr2 "Checking accessibility of files";
  let all_files = w.Lfs.extfiles#assoc iroot in
  Common.execute_and_show_progress (all_files#cardinal) (fun k ->

         k();
  );

  if not !fsck_fast then begin
    pr2 "Checking coverability of files";
    let todel = ref [] in

    Common.execute_and_show_progress (all_files#cardinal) (fun k ->
      all_files#iter (fun o -> 
        k();
        if not (Common.lfile_exists (obj_to_filename db o))
        then begin
          let descr = Lfs.string_of_descr_obj o w in
          pr2(spf "check_world: pb obj not on disk %d with descr= %s" o descr);
          push2 (o, descr) todel;
        end
      );
    );
    if !rm_obj_if_pb then 
      !todel +> List.iter (fun (o, descr) -> 
        Lfs.rm (Right o) +> ignore;
      )
  end;

  pr2 "Checking some equality of global numbers (cardinality, ...)";
  if !max_o > !Common._counter 
  then begin
    pr2 (spf "check_world: pb counter is not good, (max = %d) != (counter = %d)" 
            !max_o !Common._counter);
    Common._counter := !max_o;
    pr2 (spf "check_world: I am adjusting it, now here is the value of counter %d" 
            !Common._counter);
  end;
  ()

(*****************************************************************************)

let launch_semireal ~metapath:path_meta ~use_transact = 
  if not (Sys.file_exists (path_meta ^ "/lfs_secu"))
  then failwith (spf "%s does not appear to be a meta-data LFS directory" 
                    path_meta);

  let db = open_db ~metapath:path_meta ~use_transact in

  (* still ? yes, especially now that I also use regular files as plugins *)
  Lfs._realfs := true;

  Lfs.core_get_fcontent__id  := 
    (function 
    | Lfs.Real o -> Common.read_file (obj_to_filename db o) 
    | _ -> raise Impossible);
  
  Lfs.core_set_fcontent__fst := 
    (function 
    | (Lfs.Core content, o) -> Lfs.Real o
    | _ -> raise Impossible);
  
  Lfs.core_get_size_fcontent__slength := 
    (function 
    | Lfs.Real o -> 
        (* why try ? cos first call ml to check if can mkfile, 
         * and then create file on disk 
         * (but normally no more pb cos dont get called) *)
        (try Common.filesize (obj_to_filename db o)
         with _ -> 0
         ) +> Common.size_ko 
    | _ -> raise Impossible
    );

  Lfs.core_update_fcontent__fst := 
    (function (content, o) -> 
      (*  CONFIG,  can comment if want lazy update (or no update at all) *)

(*
      Common.write_file (obj_to_filename db o) (content());
      (*  CONFIG, can comment if trust poffs :) *)
      Common.command2("mkdir " ^ (obj_to_path path_meta o) ^ "/RCS");
      Common.command2("cd "^(obj_to_path path_meta o) ^ 
                         "; cp data _backup; echo bidon | ci -l _backup");
*)
      Lfs.Real o
    );

(* for change_file ? 
   if (basename path =~ ".*\\.txt$")
   then begin
   Common.command2("mkdir " ^ (obj_to_path o) ^ "/RCS");
   Common.command2("cd " ^ (obj_to_path o) ^ ";" ^
                   "cp data _backup; " ^ 
                    "echo bidon | ci -l _backup");
   end
*)

  Lfs.hook_find_alogic :=  
    (fun id -> Ioplugins.uninteract_logic (obj_to_filename db id));

  (* Lfs.core_hook_transducer := (fun id prop -> ##SPEC *)
(*
  Lfs.hook_find_transducer := 
    (fun id prop conv -> 
      if prop = (Lfs.Prop "transducer:")
      then Left  (Ioplugins.uninteract_transducer
                     (obj_to_filename path_meta id) 
                     (obj_to_filename path_meta))
      else Right (Ioplugins.uninteract_adv_transducer 
                     (obj_to_filename path_meta id) conv);
    (* AdvTrans (uninteract_adv_transducer (obj_to_filename id))); ##SPEC *)
    );
*)


  Lfs.hook_action_check_world +> Common.add_hook_action (fun w -> 
    check_world_semireal db path_meta w
  );


  Lfs.hook_action_mkfile +> Common.add_hook_action (fun (o,filename) -> 
(*    Common.command2("mkdir -p " ^ (obj_to_path path_meta o)); *)
    (* bugfix: 
     *  command2 ("touch " ^ (obj_to_path o) ^ "/\""^(basename path) ^ "\""));
     * bugfix: 
     *  command2 ("cd "^(obj_to_path o) ^ 
     *            "; ln -s \"" ^ (basename path) ^ "\"  data"));
     * 
     * bugfix cos the filename may contain some special symbols such as $1
     * that are interpreted by the shell of Sys.command :(
     * It can also be a filename such as '-nlaib;' and shell tool are
     * confused by that. 
     *)
(*
    Unix.close (Unix.openfile (obj_to_path path_meta o ^ "/" ^ filename) 
                   [Unix.O_CREAT] 0o640);
    (* note: could do just a ln (would be faster), but -s is better/clearer. *)
    Unix.symlink filename (obj_to_path path_meta o ^ "/data");
*)
    ()
  );

  Lfs.hook_action_rm +> Common.add_hook_action (fun o -> 
    ()
(*
    if Flag_lfs.really_delete_not_mv
    then begin
      (* todo?: erase the RCS/ too ? *)
      Common.command2 ("rm -f " ^ (obj_to_path path_meta o) ^ "/*"); 
      Common.command2 ("rm -f " ^ (obj_to_path path_meta o) ^ "/.*");
      Common.command2 ("rmdir " ^ (obj_to_path path_meta o)); 
    end
    else begin
      let extr_str = Lfs.string_extr_of_descr_obj o !Lfs.w in
      Common.write_file ~file:(obj_to_path path_meta o ^ "/__extr_lfs__") 
        (extr_str ^ "\n");
      Common.command2 ("mv " ^ (obj_to_path path_meta o) ^ " " ^ 
                          path_meta ^ "/lost+found/");
    end
*)
  );

  db.transact, (fun o -> obj_to_filename db o)
