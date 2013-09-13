open Common

open Oset (* $??$ *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* because of ext2 limitation, we cant have dirs with more than
 * 32000 files => have to split
 *)
let (obj_to_path: string -> Lfs.idfile -> string) =
 fun metapath -> fun obj ->
   metapath ^ "/files/" ^ (i_to_s (obj / 1000)) ^ "/" ^ (i_to_s obj)

let (obj_to_filename: string -> Lfs.idfile -> string) =
 fun metapath -> fun obj ->
  (* old: obj_to_path metapath obj   ^ "/data", but then
   * have to do some readling in lfs_fuse. So because of the introduction
   * of the semireal mode, simpler to not do this work in lfs_fuse.
   * So obj_to_filename even hide the fact that the real_mode use
   * a /data symlink to the file.
   *)
  let realo = Unix.readlink (obj_to_path metapath obj ^ "/data") in
  obj_to_path metapath obj ^ "/" ^ realo


(*****************************************************************************)
(* Check world real *)
(*****************************************************************************)
let fsck_fast           = ref true
let mv_lost_found_if_pb = ref false
let rm_obj_if_pb        = ref false


let check_world_real metapath = fun w ->

  let iroot = w.Lfs.prop_iprop#assoc Lfs.root in

  let max_o = ref 0 in

  pr2 "Checking accessibility of files";
  let all_files = w.Lfs.extfiles#assoc iroot in
  Common.execute_and_show_progress (all_files#cardinal) (fun k ->
   readdir_to_dir_list (metapath ^ "/files/") +> List.iter (fun s1 ->
     if s_to_i s1 < 2000 then (* not view files *)
       readdir_to_dir_list (metapath ^ "/files/" ^ s1) +> List.iter (fun s2 ->
         k();
         let o = (s_to_i s2) in
         if not (o $??$ all_files) then begin
           pr2 ("check_world: pb file " ^ (i_to_s o) ^
                " not accessible (you should move it in lost+found)");
           if !mv_lost_found_if_pb then
             Common.command2 ("mv " ^metapath^ "/files/" ^ s1 ^ "/" ^ s2 ^
                               " "  ^metapath^ "/lost+found/");
         end;
         max_o := max !max_o o;
       )
   );
  );

  if not !fsck_fast then begin
    pr2 "Checking coverability of files";
    let todel = ref [] in

    Common.execute_and_show_progress (all_files#cardinal) (fun k ->
      all_files#iter (fun o ->
        k();
        if not (Common.lfile_exists (obj_to_filename metapath o))
        then begin
          let descr = Lfs.string_of_descr_obj o w in
          pr2(spf "check_world: pb obj not on disk %d with descr= %s" o descr);
          push2 (o, descr) todel;
        end
      );
    );
    if !rm_obj_if_pb then
      !todel +> List.iter (fun (o, descr) ->
        Common.write_file (metapath^ "/lost+found/" ^i_to_s o^ ".old") descr;
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
let launch_real path_meta =
  if not (Sys.file_exists (path_meta ^ "/lfs_secu"))
  then failwith
    (spf "%s does not appear to be a meta-data LFS directory" path_meta);

  Common.command2("mkdir -p "^ path_meta ^ "/files");


  Lfs._realfs := true;

  Lfs.core_get_fcontent__id  :=
    (function
    | Lfs.Real o -> Common.read_file (obj_to_filename path_meta o)
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
        (try Common.filesize (obj_to_filename path_meta o)
         with _ -> 0
         ) +> Common.size_mo_ko
    | _ -> raise Impossible
    );

  Lfs.core_get_date_fcontent__today :=
   (function
    | (Lfs.Real o) ->
          let file = obj_to_filename path_meta o in
          let stat = Unix.LargeFile.lstat file in (* lstat for symlinks *)
          let float_time = stat.Unix.LargeFile.st_mtime in
          Unix.localtime float_time
    | _ -> raise Impossible
    );

  Lfs.core_update_fcontent__fst :=
    (function (content, o) ->
      (*  CONFIG,  can comment if want lazy update (or no update at all) *)
      Common.write_file (obj_to_filename path_meta o) (content());
      (*  CONFIG, can comment if trust poffs :) *)
      Common.command2("mkdir " ^ (obj_to_path path_meta o) ^ "/RCS");
      Common.command2("cd "^(obj_to_path path_meta o) ^
                         "; cp data _backup; echo bidon | ci -l _backup");
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
    (fun id -> Ioplugins.uninteract_logic (obj_to_filename path_meta id));

  (* Lfs.core_hook_transducer := (fun id prop -> ##SPEC *)
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


  Lfs.hook_action_check_world +> Common.add_hook_action (fun w ->
    check_world_real path_meta w
  );


  Lfs.hook_action_mkfile +> Common.add_hook_action (fun (o,filename) ->
    Common.command2("mkdir -p " ^ (obj_to_path path_meta o));
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
    Unix.close (Unix.openfile (obj_to_path path_meta o ^ "/" ^ filename)
                   [Unix.O_CREAT] 0o640);
    (* note: could do just a ln (would be faster), but -s is better/clearer. *)
    Unix.symlink filename (obj_to_path path_meta o ^ "/data");
  );

  Lfs.hook_action_rm +> Common.add_hook_action (fun o ->
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
  );

  (fun o -> obj_to_filename path_meta o)
