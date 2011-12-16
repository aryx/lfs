open Common

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let stat = ref true
let fsck = ref true
let fuse = ref true

(* or -wrapper_mode  ? *)
let semi_real_mode = ref true (* now the default *)

(* to debug *)
let exec_lfs_command = ref false
let exec_fuse_command = ref false

let timeout = ref 120
let print_backtrace = ref false

let use_glimpse = ref false
let use_transact = ref true

let fuse_allow_other = ref false

(*****************************************************************************)
(* Special action *)
(*****************************************************************************)

(*./mount-byte.lfs -verbose_level 2 -command_lfs -nofsck ~/meta-lfs ~/lfs/ *)
let action_lfs_command database =
  Path.cd_
     "/ext:bib/name:ridoux/parts";
  (*
    "/.best";
     /.best/.classic/agrep:combinator
     "/project:LFS";
     "/plugins/glimpse:ogg/prop_to_test_timeout";
  *)

  Lfs.ls_bis() +> iter print_endline;

  if false then begin
    Path.cd_ "/.best/date:yesterday";
    Lfs.ls_bis() +> iter print_endline;
  end;

  if false then begin
    Path.cd_ "/.best/props-misc^/agrep:combinator";
    Lfs.ls_bis() +> iter print_endline;
  end;

  if false then begin
  !Lfs.w.Lfs.files#iter (fun (idfile, file) ->

    let extr = file.Lfs.extrinsic in
    let filename = file.Lfs.filename in
    let extr2 =
      extr
      +> List.map (fun ip -> !Lfs.w.Lfs.iprop_prop#assoc ip)
      +> List.map (fun p -> Lfs.string_of_prop p)
    in
    pr2 (spf "%05d: %-40s = %s" idfile filename (extr2 +> Common.join "/"))

  );
  end;

  if false then begin
    let iprop = !Lfs.w.Lfs.prop_iprop#assoc (Lfs.Prop "project:LFS") in
    Lfs.w := {!Lfs.w with
      Lfs.extfiles = !Lfs.w.Lfs.extfiles#apply iprop (fun x ->
        x#del 2888)
    };
  end;

  if false then begin
     let _o = (Lfs.mkfile "xxx.txt" "" None) in
     (*commit_and_make_new_trans(); *)
     Gc.full_major ();
     (* zarb: otherwise get segfault when detarring usr.tar *)
  end;

  database.Lfs_persistent.final();
  ()


(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)

let main () =
  begin
    Lfs.copyright ();
    Common_extra.set_link();

    let options = [

      "-nostat",   Arg.Clear stat, "  dont launch stat";
      "-nofsck",   Arg.Clear fsck, "  dont launch fsck";
      "-nofuse",   Arg.Clear fuse, "  dont launch fuse";

      "-semi_real_mode", Arg.Set semi_real_mode,
      "  launch LFS in wrapper mode (default mode)";
      "-real_mode", Arg.Clear semi_real_mode,
      "  launch LFS in real mode";

      "-fscklong", Arg.Clear Lfs_real.fsck_fast,
      "  check for coverability";
      "-mv_lost_found",   Arg.Set Lfs_real.mv_lost_found_if_pb,
      "  if fsck detects a pb, then move file in lost+found";
      "-rm_obj",   Arg.Set Lfs_real.rm_obj_if_pb,
      "  if fsck cant find a file, then rm corresponding object";

      "-timeout",        Arg.Set_int timeout,
      "  <sec> interrupt LFS or buggy external plugins";
      "-print_backtrace",        Arg.Set print_backtrace,
      "  more information in log";

      "-stree_no_all",   Arg.Clear Flag_lfs.stree_index_all_properties,
      "  dont index all properties";
      "-use_glimpse", Arg.Set use_glimpse, "  guess what";

      "-bdb_size_tables", Arg.Set_int Flag_lfs.bdb_size_tables,
      " <int> to handle big LFS context";
      "-notransact",  Arg.Clear use_transact,
      "  dont use transaction";

      "-fuse_allow_other", Arg.Set fuse_allow_other,
      "  to allow other users to access your fs. cf also /etc/fuse.conf";

      "-command_lfs",  Arg.Set exec_lfs_command,
      "  launch only the internal command specified in mount.ml";
      "-command_fuse",  Arg.Set exec_fuse_command,
      "  launch only the internal command specified in lfs_fuse.ml";


      "-version",   Arg.Unit (fun () ->
        pr2 (spf "LFS version: %s, built %s"
                Config_lfs.version
                "$Date: 2008/06/08 12:32:06 $");
        exit 0;
      ),
      "  guess what";

    ]
      ++ (Common.cmdline_flags_devel ())
      ++ (Common.cmdline_flags_verbose ())
    in

    let usage_msg =
      ("Usage: " ^ Common.basename Sys.argv.(0) ^
          " [options] <LFS-meta-data-path> <mountpoint>\nOptions are:")
    in
    let args = Common.parse_options options usage_msg Sys.argv in

    (match args with
    | metapath::xs ->
        (* it seems that berkeley DB need absolute path *)
        let metapath = Common.relative_to_absolute metapath in
        pr2 metapath;

        (* Adjusts hooks, globals variables, fields, in lfs.ml,
         * such as the Lfs.w world global big record variable.
         *)

        let database, obj_to_filename, read_only =
          if not !semi_real_mode
          then
            let obj_to_f = Lfs_real.launch_real metapath in
            let db = Lfs_bdb.launch_bdb metapath !use_transact in
            Lfs.default_ls_mode := Lfs.Best;
            db, obj_to_f, false
          else begin
            let db, obj_to_f =
              Lfs_semireal.launch_semireal metapath !use_transact in

           use_glimpse := false;
          (* not needed for now, maybe later when add inotify *)
           Flag_lfs.use_idle := false;
           Lfs.lfs_allow_cd_parts := false;
           Lfs.default_ls_mode := Lfs.Best;

           db, obj_to_f, true (* read_only *)
          end
        in

        Lfs_extensions.install_inode_extension();
        Lfs_extensions.install_datetoday_extension();
        Lfs_extensions.install_stree_glimpse_and_co_extension
          metapath !use_glimpse;

        if !stat then Lfs.stat_world  !Lfs.w;
        if !fsck then Lfs.check_world !Lfs.w;

        if !exec_lfs_command
        then begin action_lfs_command database; exit 0 end;

        if !fuse then
          (match xs with
          | [mountpath] ->
              Common.start_log_file();

              Lfs_fuse.launch_fuse
                ~src:metapath
                ~dest:mountpath
                ~obj_to_filename
                ~transact_func:database
                ~timeout:!timeout
                ~print_backtrace:!print_backtrace
                ~read_only
                ~exec_fuse_command:!exec_fuse_command
                (Array.of_list
                    ((Sys.argv.(0)
                       ::"-s"::"-o"
                       ::("max_read=16384" ^
                             (if !fuse_allow_other
                             then ",allow_other"
                             else ""
                             ))
                       ::[mountpath])));

          | _ -> Arg.usage options usage_msg
          );
        database.Lfs_persistent.final();
        log (Common.profile_diagnostic_basic ());
        log (Common.profile_diagnostic ());

    | _ -> Common.usage usage_msg options
    );
  end

(*****************************************************************************)
let _ =
  if not !Sys.interactive then main ()
