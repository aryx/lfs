open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*
 * Just a prog to call the right transducers and build an LFS "database".
 *
 * Use the semi real mode of LFS. Just a wrapper over
 * an existing hierarchy of files. Smoother (and in the end better) than
 * switching completly to a "full" real mode LFS.
 *
 * So to use the db then use mount.lfs -semi_real_mode.
 *
 * Could be also called mk_lfs_wrapper.ml.
 *
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
let lfs_metapath = ref ""

let use_transact   = ref false
let index_symlinks = ref false
let use_glimpse    = ref false

(* For intrinsic transduced properties, so less coupling between transducer
 * and lfs core. Don't need anymore to create the toplevel properties
 * mentionned in transducers in this file.
 *)
let auto_create_attributes = ref true


let use_logic = ref true

let use_ogg_transducer = ref true
let use_mp3_transducer = ref true

let use_c_transducer   = ref false
let path_c_transducer = ref "/home/pad/c-yacfe/demos/c_transducer.byte"

let use_php_transducer   = ref false
let path_php_transducer = ref "/home/pad/c-pfff/demos/php_transducer.byte"



let action = ref ""
let test = ref false

(*****************************************************************************)
(* Some  debugging functions *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* could also use symlinks that points to this so that listing the
 * symlink will automatically create the properties :)
 *)
let prepared_props =
  (enum 1990 2007) +> List.map (fun i ->
    spf "date:*--*--%d" i
  ) ++
  [
    "date:*--Jan--2009";
    "date:*--Feb--2009";
    "date:*--Mar--2009";
    "date:*--Apr--2009";
    "date:*--May--2009";
    "date:*--Jun--2009";
    "date:*--Jul--2009";
    "date:*--Aug--2009";
    "date:*--Sep--2009";
    "date:*--Oct--2009";
    "date:*--Nov--2009";
    "date:*--Dec--2009";
  ]


(* I could repeat what is in basicenv.sh but better avoid clones
 * ex of line that is in basicenv.sh:
 *   mv "ext:c"  "type:Program/noprop_ext:c"
 *   mv "ext:h"  "type:Program/noprop_ext:h"
 *)
let extract_type_ext_info_of_basicenv ?(verbose=false) file =
  Common.cat file +> Common.map_filter (fun s ->
    if s =~ "^mv +\"\\(ext:[^\"/]+\\)/?\"[ \t]+\"\\(type:[^/]+\\)/noprop_.*"
    then begin
      let (ext_str, type_str) = matched2 s in
      if verbose then pr2 (spf "%s -> %s" ext_str type_str);
      Some (ext_str, type_str)
    end
    else None
  )

(* can then use lfs-tunes, also in semi_real mode. Could even dont use fuse at
 * all. So easier to install.

cd "$root"/logic:genre:/;
cp $src/p_logic/string_logic2.pl .

#cd 'genre:.*Rock.*'
#cd 'genre:(Disco|Dance|Funk)'
#cd 'genre:(Darkwave|New Wave)'
#cd 'genre:(Retro|Cult|Oldies)'
#cd 'genre:(Electronic|Techno|Trance)'
#cd 'genre:(Hip-Hop|Rap)'
#cd 'genre:(Comedy|Humour)'

 *)

(*****************************************************************************)
(* Extra actions helpers *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
(* Gentoo importer *)
(*---------------------------------------------------------------------------*)
let top_gentoo_info_dir = "/var/db/pkg"

type package = {
    name : string;
    category: string;
}

(* Format example of gentoo metadata CONTENTS file:
 *  dir /usr/bin
 *  obj /usr/bin/mpirun_dbg.totalview 8bb78406bfe57ed01e4815b44631ee06 12144396
 *  sym /usr/share/mpich/examples1/mpirun -> ../bin/mpirun 1214439687
 *)
let files_of_gentoo_CONTENTS file =
  let xs = Common.cat file in
  xs +> Common.map_filter (fun s ->
    let words = Common.split " " s in
    match words with
    | "obj"::file::xs ->
        Some file

    (* Take symlinks ? symlinks were certainly made because
     * of hierarchy limitation, so if use LFS wrapper, dont really
     * need to see them ?
     * update: in fact take symlinks :) cos some symlinks can
     * contain LFS queries/views so they can be very useful.
     *)
    | "sym"::file::xs ->
        Some file


    | "dir"::_ ->
        None

    | s::_ ->
        failwith ("unknown entry: " ^ s)
    | _ ->
        raise Impossible
  )


let gentoo_read_metadata_pkg_info ?(verbose=false) () =
  let h = Hashtbl.create 101 in
  let files =
    Common.cmd_to_list (spf "find %s -name CONTENTS" top_gentoo_info_dir) in

  files +> Common.index_list_and_total +> List.iter (fun (fullfile, i, total) ->
    pr2 (spf "PARSING: %s (%d/%d)" fullfile i total);

    let file =
      Common.filename_without_leading_path top_gentoo_info_dir fullfile in
    let package =
      if file =~ "\\(.*\\)/\\(.*\\)/CONTENTS"
      then
        let (s1, s2) = Common.matched2 file in
        { name = s2;
          category = s1;
        }
      else failwith ("wrong entry: " ^ file)
    in
    let files_of_package =
      files_of_gentoo_CONTENTS fullfile in

    files_of_package +> List.iter (fun afile ->
      if verbose
      then pr2 afile;

      Hashtbl.add h afile package;
    );
  );
  h


(*---------------------------------------------------------------------------*)
(* Ubuntu importer *)
(*---------------------------------------------------------------------------*)
let top_ubuntu_info_dir = "/var/lib/dpkg/info"

(* Those .list files do not have as good format as gentoo where can
 * see if file or dir, but even if here we put dirs too, they will be
 * eventually filtered out later as we only search for files. The only
 * pb is the diagnosis of only_in_ubuntu which will have some falst
 * positive. Also not as good category: :(
 *)

(* find %s -name *.list *)

let ubuntu_read_metadata_pkg_info() =
  raise Todo

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let lfs_src_path = Config_lfs.path
let lfs_basivenv = Config_lfs.path ^ "/data/basicenv.sh"

(*---------------------------------------------------------------------------*)
(* todo?
 * mkdir "file:";
 * cp $src/p_logic/size_logic .
 *
 * cd "$root/transducer:mail/transducer:emlx/";
 * cp $src/p_transducer/email_transducer.pl .
 *
 * cd "$root"/'adv_transducer:bib';
 * cp $src/p_adv_transducer/bibtex_adv_transducer .
 *
 * cd "$root"/'adv_transducer:tex';
 * cp $src/p_adv_transducer/latex_adv_transducer.pl .
 *
 * cd "$root"/'adv_transducer:txt';
 * cp $src/p_adv_transducer/txtpad_adv_transducer.pl .
 *
 * cd "$root"/'logic:functype:';
 * cp $src/p_logic/type_logic .
 *
 * cd "$root"/'adv_transducer:c';
 * cp $src/p_adv_transducer/c_adv_transducer.pl .
 *
 * cd "$root"/'adv_transducer:make';
 * cp $src/p_adv_transducer/makefile_adv_transducer.pl .
 *
 *)

let toplevel_properties =
  [
   "logic:";
   "transducer:";
   "adv_transducer:";

   "props-misc";
   "props-attr";

   "name:";
   "ext:";
   "date:";
   "size:";
   "dir:";
   "path:";

   "type:";
  ]


(* not that useful now that have auto_create_attributes *)
let other_properties = [
   (* gentoo *)
   "category:";
   "package:";

   (* music *)
   "artist:";
   "genre:";
   "title:";
   "album:";
   "year:";
   "time:";
   "comment:";
   "bitrate:";
   "frequency:";

   (* programs *)
   "function:";
   "nb_include:";
   "macrotop:";
]


(*---------------------------------------------------------------------------*)
let create_initial_hierarchy add_file_func db =
  if(not (Sys.file_exists (lfs_src_path ^"/p_transducer/ogg_transducer.pl")))
  then begin
    pr2 "We are not able to access some plugin like:";
    pr2 ("\t" ^ lfs_src_path ^ "/p_transducer/ogg_transducer.pl");
    pr2 "Have you set LFS_HOME to point to the source directory of LFS ? ";
    pr2 "do a source env.sh from the LFS source directory";
    failwith "transducer plugin not found";
  end;
  pr2 (spf "warning: we use transducer plugins from %s, do not remove those bins"
          lfs_src_path);

  if(not (Sys.file_exists (lfs_src_path ^"/p_logic/date_logic_via_logfun")))
  then begin
    pr2 "We are not able to access the logic plugin:";
    pr2 ("\t" ^ lfs_src_path ^ "/p_logic/date_logic_via_logfun");
    pr2 "Have you set LFS_HOME to point to the source directory of LFS ? ";
    pr2 "do a source env.sh from the LFS source directory";
    failwith "logic plugin not found";
  end;
  pr2 (spf "warning: we use logic plugins from %s, do not remove those bins"
          lfs_src_path);


  (* for enabling the transducer mechanism in Lfs.mkfile *)
  Lfs._realfs := true;
  Lfs.core_set_fcontent__fst :=
    (function
    | (Lfs.Core content, o) -> Lfs.Real o
    | _ -> raise Impossible
    );

  Lfs.hook_find_transducer :=
    (fun id prop conv ->
      if prop = (Lfs.Prop "transducer:")
      then
        let trans =
          Ioplugins.uninteract_transducer
            (Lfs_semireal.obj_to_filename db id)
            (Lfs_semireal.obj_to_filename db)
        in
        let trans' =
          (fun x ->
            let props = trans x in
            props +> List.iter (fun prop ->
              if (Lfs.is_vattr prop || Lfs.is_attr prop) &&
                !auto_create_attributes
              then
                let (Lfs.Prop sattr) = Lfs.attr_vattr prop in
                if !Lfs.w.Lfs.prop_iprop#haskey (Lfs.Prop sattr)
                then ()
                else begin
                  Path.dopath_ "/props-attr" (fun () ->
                    Lfs.mkdir sattr;
                  )
                end
            );
            props
          )
        in
        Left trans'
      else
        let trans =
          (Ioplugins.uninteract_adv_transducer
              (Lfs_semireal.obj_to_filename db id) conv)
        in
        Right trans
    );

  Lfs.hook_find_alogic :=
    (fun id -> Ioplugins.uninteract_logic (Lfs_semireal.obj_to_filename db id));


  (* For Lfs.transducer_system. Note that get some Not_found when
   * first called in mkfile, when empty file, as the obj_to_filename
   * has not yet been setup.
  *)
  Lfs.core_get_size_fcontent__slength :=
   (function
    | (Lfs.Real o) ->
        (try
            let file = Lfs_semireal.obj_to_filename db o in
            Common.filesize file
          with Not_found -> 0
        ) +> Common.size_ko
    | _ -> raise Impossible
    );
  Lfs.core_get_date_fcontent__today :=
   (function
    | (Lfs.Real o) ->
        (try
          let file = Lfs_semireal.obj_to_filename db o in
          let stat = Unix.LargeFile.lstat file in (* lstat for symlinks *)
          let float_time = stat.Unix.LargeFile.st_mtime in
          Unix.localtime float_time
        with Unix.Unix_error(_,"lstat",_) ->
          let file = Lfs_semireal.obj_to_filename db o in
          pr2 ("pb analyzing: " ^ file);
          Unix.localtime (Unix.time ())
        | Not_found ->
          Unix.localtime (Unix.time ())
        )
    | _ -> raise Impossible
   );




  Path.cd_ "/";
  toplevel_properties +> List.iter (fun s -> Lfs.mkdir s);


  (* As opposed to logic, do not have to make LFS always aware of the
   * transducers. We can do everything at build_db time.
   *)
  if !use_ogg_transducer then
    add_file_func (lfs_src_path ^ "/p_transducer/ogg_transducer.pl")
      ("ogg_transducer.pl", [], ["transducer:ogg"]) db;

  if !use_mp3_transducer then
    add_file_func (lfs_src_path ^ "/p_transducer/mp3_transducer.pl")
      ("mp3_transducer.pl", [], ["transducer:mp3"]) db;

  if !use_c_transducer then
    add_file_func (!path_c_transducer)
      ("c_transducer", [], ["transducer:c"]) db;
  if !use_c_transducer then
    add_file_func (!path_c_transducer)
      ("c_transducer", [], ["transducer:h"]) db;
  if !use_php_transducer then
    add_file_func (!path_php_transducer)
      ("php_transducer", [], ["transducer:php"]) db;
  if !use_php_transducer then
    add_file_func (!path_php_transducer)
      ("php_transducer", [], ["transducer:phpt"]) db;



  ()



(*---------------------------------------------------------------------------*)
let create_final_hierarchy add_file_func db =

  (* add attr in intrinsic, cos extrinsic are sanitized and we dont want
   * to get a logic_colon_data_colon
   *
   * todo ? maybe could also analyze basicenv.sh instead of duplicating
   * functionnality here ?
   *)

  if !use_logic then begin

  add_file_func (lfs_src_path ^ "/p_logic/date_logic_via_logfun")
    ("date_logic_via_logfun", [], ["logic:date:"]) db;

  add_file_func (lfs_src_path ^ "/p_logic/size_logic")
    ("size_logic", [], ["logic:size:"]) db;

  add_file_func (lfs_src_path ^ "/p_logic/string_logic")
    ("string_logic", [], ["logic:ext:"]) db;

  add_file_func (lfs_src_path ^ "/p_logic/string_logic")
    ("string_logic", [], ["logic:name:"]) db;

  add_file_func (lfs_src_path ^ "/p_logic/string_logic")
    ("string_logic", [], ["logic:dir:"]) db;

  add_file_func (lfs_src_path ^ "/p_logic/string_logic")
    ("string_logic", [], ["logic:path:"]) db;

  add_file_func (lfs_src_path ^ "/p_logic/string_logic")
    ("string_logic", [], ["logic:category:"]) db;

  add_file_func (lfs_src_path ^ "/p_logic/string_logic")
    ("string_logic", [], ["logic:package:"]) db;

  end;

  prepared_props +> List.iter (fun s -> Path.cd_ s);

  Path.cd_ "/";
  extract_type_ext_info_of_basicenv lfs_basivenv +> List.iter
    (fun (ext_str,type_str) ->
      Path.cd_ "/";
      (* need create prop first, otherwise assert fail in lfs.ml *)
      Path.cd_ ext_str;
      Path.cd_ type_str;
      Path.cd_ "/";
      let new_path = [(Lfs.Element (Lfs.Single (Lfs.Prop type_str)))] in
      Lfs.mvdir ext_str new_path  ext_str;
    );
  ()


(*---------------------------------------------------------------------------*)
let transduce file =
  let filename = Common.basename file in

  let (d,b,e) =
    match Common.dbe_of_filename_safe file with
    | Left (d,b,e) -> d,b, Some e
    | Right (d,b) -> d,b, None
  in

  (* extrinsinc *)
  let extr = Common.split "/" d in


  (* intrinsic *)
  let intr_dir = extr +> List.map (fun s ->
    "dir:" ^ Lfs.sanitize_lfs s
  ) in
  let intr_path =
    "path:" ^ (extr +> List.map Lfs.sanitize_lfs +> Common.join "__")
  in

  let _ext =
    match e with
    | Some e -> "ext:" ^ e
    | None -> "ext:NOEXT"
  in


  let intr_system =
    Lfs.transducer_system filename (Lfs.Real (0))
      +> List.map Lfs.string_of_prop
  in

  filename, extr, (*[ext]*) intr_system ++ intr_dir ++ [intr_path]

(*---------------------------------------------------------------------------*)
let add_file2 real_path (filename, extr, intr) db =
  (* escape stuff cos my data still contain some '&' and ':' and %
   * and $ and # or file named data
   *)

  let filename' =
    filename +> Lfs.sanitize_lfs +> (fun s ->
      (* To avoid some assert on valid_new_file_name. But normally this
       * assert is quite specific to real-mode ? So could move code
         * and so do not need this code below.
       *)
      if List.mem s ["data"; "_dircomputed";"_backup";"RCS"]
      then s ^ "__lfs_escaped_"
      else s
    )
  in

  let extr' =
    extr +> List.map Lfs.sanitize_lfs +> List.map (fun s ->
      s +> Str.global_replace (Str.regexp ":") "_colon_"
        +> (fun s ->
          if List.mem s ["parts"]
          then s ^ "__lfs_escaped_"
          else s
        )
    )
  in
  (* note that those intrinsic props are not from the transducer, but just
   * from the basic transducer_system like ext: and name:
   *)

  let intr' = intr +> List.map Lfs.sanitize_lfs in
  (*
  pr2 file;
  pr2_gen extr;
  pr2_gen intr;
  *)
  Path.cd_ "/";

  (* create the props-misc *)
  extr' +> List.iter (fun s ->
    Path.cd_ "/";
    if !Lfs.w.Lfs.prop_iprop#haskey (Lfs.Prop s)
    then ()
    else begin
      Path.cd_ "props-misc";
      Lfs.mkdir s;
    end
  );

  (* create the attribute automatically for intrinsic ? *)
  intr' +> List.iter (fun s ->
    let prop = Lfs.Prop s in
    Path.cd_ "/";
    if (Lfs.is_vattr prop || Lfs.is_attr prop) &&
        !auto_create_attributes
    then
      let (Lfs.Prop sattr) = Lfs.attr_vattr prop in
      if !Lfs.w.Lfs.prop_iprop#haskey (Lfs.Prop sattr)
      then ()
      else begin
        pr2 sattr;
        Path.cd_ "props-attr";
        Lfs.mkdir sattr;
      end
  );

  (* go to right directory *)
  Path.cd_ "/";
  (extr' ++ intr') +> List.iter (fun s ->
    Path.cd_ s;
  );

  let o = Lfs.mkfile filename'  "" None in

  (* must be done before write, cos write call obj_to_filename :) *)
  db.Lfs_semireal.real_path#add (o, real_path) +> ignore;

  (* needed to call file content transducer *)
  Lfs.write (Right o) "";

  db.Lfs_semireal.transact.Lfs_persistent.commit();

  ()

let add_file a b c =
  Common.profile_code "Build.add_file" (fun () -> add_file2 a b c)



(*---------------------------------------------------------------------------*)
let build_db_dirs ?(verbose=false) metapath dirs =

  assert(not (Common.null_string metapath));

  let db = Lfs_semireal.create_db metapath !use_transact in

  create_initial_hierarchy add_file db;

  let files =
    Common.cmd_to_list (spf "find %s -type f" (dirs +> Common.join " ")) in
  let filesbis =
    Common.cmd_to_list (spf "find %s -type l" (dirs +> Common.join " ")) in
  let files =
    if !index_symlinks
    then filesbis ++ files
    else files
  in


  Common.execute_and_show_progress (List.length files) (fun k ->
  files +> Common.index_list_and_total +> List.iter (fun (file, i, nbfiles) ->
    if verbose then pr2 (spf "PARSING: %s (%d/%d)" file i nbfiles);
    k();

    let (filename, extr, intr) =
      transduce file
    in
    add_file file (filename, extr, intr) db;
  ));

  create_final_hierarchy add_file db;

  Lfs_semireal.close_db db;
  ()

(*****************************************************************************)
(* Extra action *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
let build_db_pad_home metapath =
  let dirs =
    if !test
    then ["/home/pad/etc"]
    else [
      (* "/tmp/build_db_test";*)

     "/home/pad/mobile";
     "/home/pad/reference";

     "/home/pad/etc";
     "/home/pad/private";

     "/home/pad/admin";
    ]
  in
  build_db_dirs metapath dirs


(*---------------------------------------------------------------------------*)
let exclude_root_dirs =
  ["proc";"sys";"dev";
   "lost+found";  "tmp";
   "var";
   "root";
   "mnt"; "home";
  ]

let expect_root_dirs =
  "bin boot etc lib media opt sbin usr"
    +> Common.split " "

let root_dirs () =
  let dirs = Common.readdir_to_dir_list "/" in
  let diff1 = dirs $-$ exclude_root_dirs in

  (* assert that similar to what I  thought ? *)

  let dirs = dirs +> Common.exclude (fun s -> List.mem s exclude_root_dirs) in
  let dirs = dirs +> List.map (fun s -> "/" ^ s) in

  pr2_gen diff1;
  dirs



let build_db_gentoo ?(test=false) metapath =
  let dirs =
    if test
    then ["/bin";]
    else root_dirs ()
  in

  let db = Lfs_semireal.create_db metapath !use_transact in
  create_initial_hierarchy add_file db;

  let files =
    Common.cmd_to_list (spf "find %s -type f" (dirs +> Common.join " ")) in

  let hgentoo = gentoo_read_metadata_pkg_info () in
  let gentoofiles = Common.hash_to_list hgentoo +> List.map fst in

  let (common, only_gentoo, only_root) =
    Common.diff_two_say_set_eff gentoofiles files in

  files +> Common.index_list_and_total +> List.iter (fun (file, i, nbfiles) ->
    pr2 (spf "PARSING: %s (%d/%d)" file i nbfiles);

    let (filename, extr, intr) = transduce file in
    let extra_intr =
      match Common.hfind_option file hgentoo with
      | Some package ->
          ["package:" ^ package.name;
           "category:" ^ package.category;
          ]
      | None -> []
    in
    add_file file (filename, extr, intr ++ extra_intr) db;
  );
  if not test then begin

    (* can also be accessed via cd !category: *)
    pr2_xxxxxxxxxxxxxxxxx ();
    pr2 "only in /:";
    pr2_xxxxxxxxxxxxxxxxx ();
    only_root +> List.iter pr2;

    pr2_xxxxxxxxxxxxxxxxx ();
    pr2 "only in gentoo:";
    pr2_xxxxxxxxxxxxxxxxx ();
    only_gentoo +> List.iter pr2;

  end;

  create_final_hierarchy add_file db;

  Lfs_semireal.close_db db;
  ()


(*---------------------------------------------------------------------------*)

let dirs = ["/home/pad/reference"]

let extract_info_line s =
  if s =~ "^-.* \\([^ ]*\\)$"
  then
    let _file = matched1 s in
    let xs = Common.split "[ \t]+" s in
    match xs with
    | perm::ino::x1::x2::size::month::day::year::file ->
        let _has_space =
          match List.length file with
          | 0 -> raise (WrongFormat s)
          | 1 -> false
          | n -> true
        in
        let file = Common.join "" file in

        let year =
          if year =~ "^[0-9][0-9]:[0-9][0-9]$"
          then "2008"
          else year
        in
        Some (file, size, (day, month, year))
    | _ -> raise (WrongFormat s)
  else None

let string_of_specific_date (day, month, year) =
  (spf "%s %s %s" day month year)

let md5sum file =
  (match Common.cmd_to_list (spf "md5sum \"%s\"" file) with
  | [x] ->
      List.hd (Common.words x)
  | _ -> raise (WrongFormat "md5sum output")
  )


(* take output of two ls -lR *)
let fix_date ?(verbose=false) file1 file2 =
  let h1 = Hashtbl.create 101 in
  let h2 = Hashtbl.create 101 in

  let h3 = Hashtbl.create 101 in

  let current_dir = ref "" in

  Common.cat file1 +> List.iter (fun s ->

    if s =~ "^\\([/\\.].*\\):$"
    then current_dir := (matched1 s);

    extract_info_line s +> Common.do_option (fun (file, size, date) ->
      let (day, month, year) = date in
      if verbose then begin
        pr2 file;
        pr2 (spf "%s %s %s" day month year);
      end;
      Hashtbl.add h1 (file, size) (!current_dir^"/"^ file, date);
      Hashtbl.add h3 file (day, month, year);
    )
  );

  (* first pass on file2 *)
  Common.cat file2 +> List.iter (fun s ->

    if s =~ "^\\([/\\.].*\\):$"
    then current_dir := (matched1 s);

    extract_info_line s +> Common.do_option (fun (file, size, date) ->
      let (day, month, year) = date in
      if verbose then begin
        pr2 file;
        pr2 (spf "%s %s %s" day month year);
      end;
      Hashtbl.add h2 (file, size) (!current_dir^"/"^ file, date);
      (*Hashtbl.add h3 file (day, month, year);*)
    )
  );


  (* second pass on file2 *)
  Common.cat file2 +> List.iter (fun s ->

    if s =~ "^\\([/\\.].*\\):$"
    then current_dir := (matched1 s);


    extract_info_line s +> Common.do_option (fun (file, size, date) ->
      let path = !current_dir ^ "/" ^ file in
      match Common.hfind_option (file, size) h1 with
      | Some (path2, date2) ->
          let nbmatch_in1 = List.length (Hashtbl.find_all h1 (file, size)) in
          let nbmatch_in2 = List.length (Hashtbl.find_all h2 (file, size)) in

          let nbmatch_in3 = List.length (Hashtbl.find_all h3 file) in
          let sdate = string_of_specific_date date in
          let sdate2 = string_of_specific_date date2 in

          (* very conservative *)
          if nbmatch_in1 = 1 && nbmatch_in2 = 1 && nbmatch_in3 = 1
          then begin
            pr2 (spf "match: %s\nto this file: %s\nsize:%s,nbmatch_in1:%d, nbmatch_in2:%d\n%s vs %s\n"
                    path path2 size nbmatch_in1 nbmatch_in2 sdate sdate2);

            let (day, month, year) = date2 in
            let dmy2 = DMY (Day (s_to_i day),
                          month_of_string month,
                          Year (s_to_i year)) in
            let mtime2, _tm = Common.dmy_to_unixtime dmy2 in

            let (day, month, year) = date in
            let dmy = DMY (Day (s_to_i day),
                          month_of_string month,
                          Year (s_to_i year)) in
            let mtime, _tm = Common.dmy_to_unixtime dmy in

            assert (mtime2 <= mtime);

            if date = date2
            then pr2 "ALREADY SAME DATE"
            else
              Unix.utimes path 0.0 mtime2

          end;

      | None ->
          ()
    )
  );
  ()

(*---------------------------------------------------------------------------*)
let actions () = [
  "-test_gentoo_read_db", "",
  Common.mk_action_0_arg (fun () -> ignore(gentoo_read_metadata_pkg_info()));
  "-test_ubuntu_read_db", "",
  Common.mk_action_0_arg (fun () -> ignore(ubuntu_read_metadata_pkg_info()));
  "-test_extract_basicenv", "",
  Common.mk_action_0_arg (fun () ->
    ignore(extract_type_ext_info_of_basicenv lfs_basivenv)
  );
  "-build_db_pad_home", " <metapath>",
  Common.mk_action_1_arg build_db_pad_home;
  "-build_db_gentoo", " <metapath>",
  Common.mk_action_1_arg (fun metapath ->
    build_db_gentoo ~test:!test metapath);
(*
  "-build_db_ubuntu", " <metapath>",
  Common.mk_action_1_arg (fun metapath ->
    build_db_ubuntu ~test:!test metapath);
*)
  "-fix_date", " <file_good> <dile2>",
  Common.mk_action_2_arg fix_date;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)
let all_actions () =
  actions () ++
  []

let options () =
  [
    "-test", Arg.Set test, "test mode";
    "-lfs_metapath", Arg.Set_string lfs_metapath, "";
    "-use_glimpse", Arg.Set use_glimpse, "";

    "-no_logic", Arg.Clear use_logic, "";

    "-use_ogg_transducer", Arg.Set use_c_transducer, "";
    "-use_mp3_transducer", Arg.Set use_c_transducer, "";
    "-use_c_transducer", Arg.Set use_c_transducer, "";
    "-use_php_transducer", Arg.Set use_php_transducer, "";

  ] ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  [
  (* this can not be factorized in Common *)
  "-version",   Arg.Unit (fun () ->
    pr2 "version: $Date: 2008/06/08 12:32:06 $";
    raise (Common.UnixExit 0)
  ),
  "   guess what";
  ] ++
  [
   (let s = "-xxx" in s, Arg.Unit (fun () -> action := s),
    "   ");
   (let s = "-yyy" in s, Arg.Unit (fun () -> action := s),
    "   ");
  ] ++
  Common.options_of_actions action (all_actions())

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main () =

  Common_extra.set_link();

  let usage_msg =
    ("Usage: " ^ Common.basename Sys.argv.(0) ^
        " [options] -lfs_metapath <meta> <dirs>\nOptions are:")
  in
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with

    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) ->
        Common.do_action !action xs (all_actions())

    | _ when !action ="-xxx" ->
        pr2_gen (Lfs.sanitize_lfs "a/b/c/..Makefile");


    | _ when not (Common.null_string !action) ->
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs ->
        build_db_dirs !lfs_metapath (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | _ ->
        Common.usage usage_msg (options());
        failwith "too few arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main()
  )
