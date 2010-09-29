open Ofullcommon

open Lfs

(*****************************************************************************)
(* Unix world specificities *)
(*****************************************************************************)

(* TODO? the cache strategy is not correct. use ver for File too ? but
 * that means that when detar, there would be a lots of ls :(
 * 
 * cant erase all the entry, because whatif a file is currently in use ? 
 * morover with emacs, the file is closed each time :( so how keep it
 * in the cache ?
 *)

type path = string

type pathfile = string
type pathdir  = string
type pathview  = string

type unix_world = { 
  (* note_opti: peut pas se permettre d'appeler ls (via ls_id_of_name)
   * pour savoir quel est l'id d'un filename, (ou pour dire que ce
   * filename n'existe pas) pour pouvoir ensuite recuperer les infos
   * sur ce file. De meme apres un ls, l'appli (ou linux) appelle
   * getattr sur tous les elements du ls
   * 
   * => better to save info when do first ls that avoid then redo stuff
   * 
   * old: cache_path: (path, pathinfo) oassoc mais marche pas, (cf rix
   * beck bug entre autre). *)
  cache_path2: (pathdir, (path, pathinfo) oassoc) oassoc;

  (* each dir has his view, and must remember it, otherwise I think
   * user would have to do revert sans arret (even if no modif on other
   * views)
   * 
   * how pof works ? tools I think look at the date of the file (mtime)
   * => when modif a view, augment the dirty => the next getattr for a
   * view will generate a new view => with a more recent date => tools
   * will propose the revert 
   *)
  cache_view: (pathview, (idview * int)) oassoc;
  
  (* note_opti: to avoid retransduce something (file or view) which does no
   * need to *)
  dirty_files: pathfile oset; 
  } 

  (* timestamp for Dir, cos when there was an rmdir, the lookup on a
   * path can fail because perhaps this Dir involved the property that
   * was removed.
   * 
   * todo: robust: a better solution would be to record for each
   * property the set of path that involves this property, so when
   * rmdir this prop, can just invalidate all those paths in the cache.
   *)
  and pathinfo = 
    | Dir of (int * timestamp) 
    | File of idfile
    | View of (idview * timestamp)
  and idview = int


let hook_action_umount =    ref [fun p  -> log3 "hook"]


let simple_stats (*old:metapath*) obj_to_filename ?(read_only_dir=false) = 
  let prg_time = Unix.time ()  in
  fun pathinfo ->
    match pathinfo with 
    | Dir (i,_) -> 
        {
          st_kind = Unix.S_DIR;
          st_perm = 
            if read_only_dir
            then Common.file_perm_of [`R;`X] [`R;`X] [`R;`X]
            else Common.file_perm_of [`R;`W;`X] [`R;`X] [`R;`X]
          ;
          
          st_size = Int64.of_int i;
          st_atime = prg_time;    st_mtime = prg_time;    st_ctime = prg_time;
          st_uid = Unix.getuid ();    st_gid = Unix.getgid ();
          st_nlink = 1;
          st_dev = 0;    st_ino = 0;    
          Unix.LargeFile.st_rdev = 0;
        } 
    | (File o) -> 
        let x = Unix.LargeFile.lstat (obj_to_filename o) in
(* old:
        let realo = Unix.readlink (Lfs_real.obj_to_filename metapath o) in 
        let x = Unix.LargeFile.lstat 
          ((Lfs_real.obj_to_path metapath o) ^ "/" ^ realo) in
*)
        { x with
          (* st_kind = Unix.S_REG; *)
          st_uid = Unix.getuid ();    st_gid = Unix.getgid ();
          (*  to be able to find where is on the disk the file
           * (for instance to get back with RCS to a previous version, 
           * to undo a change) 
           *)
          st_nlink = o; 
          st_dev = 0;    st_ino = 0;    
          Unix.LargeFile.st_rdev = 0;
        } 
    | (View (o,_)) -> 
        let x = Unix.LargeFile.stat (obj_to_filename o) in
        { x with
          (* st_kind = Unix.S_REG; *)
          st_uid = Unix.getuid ();    st_gid = Unix.getgid ();
          st_nlink = 1;
          st_dev = 0;    st_ino = 0;    
          Unix.LargeFile.st_rdev = 0;
        } 


let xmp_read path dest offset = 
  let h = Unix.openfile path [Unix.O_RDONLY] 0 in 
  try
    ignore (Unix.LargeFile.lseek h offset Unix.SEEK_SET);
    let res = Unix.read h dest 0 (String.length dest) in
    Unix.close h;
    res
  with x ->
    Unix.close h; 
    raise x


let xmp_write path src offset =
  let h = Unix.openfile path [Unix.O_WRONLY] 0 in
  try
    ignore (Unix.LargeFile.lseek h offset Unix.SEEK_SET);
    let res=Unix.write h src 0 (String.length src) in
    Unix.close h;
    res
  with x ->
    Unix.close h;
    raise x


(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* zarb: if add a type to the func, then dont compile anymore,    
 * (unit -> 'a) -> 'a) = fun f ->
 * 
 * subtil: have to make sure that timeout is not intercepted before here, so
 * avoid exn handle such as try (...) with _ ->    cos timeout will not
 * bubble up enough. In such case, add a case before such as  
 *   with Timeout -> raise Timeout | _ -> ... 
 *)
let transact_with_timeout trans_func timeout print_backtrace = fun f -> 
  try
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout ));
    ignore(Unix.alarm timeout);
    let x = f() in
    ignore(Unix.alarm 0);
    trans_func.Lfs_persistent.commit();
    x
  with 
  | Timeout -> 
      log "timeout (we abort)";
      trans_func.Lfs_persistent.abort (); 
      raise Timeout;
  | e -> 
      (* subtil: important to disable the alarm before relaunching the exn, 
       * otherwise the alarm is still running.
       * robust?: and if alarm launch after the log (...) ? 
       *)
      log ("exn while in transaction (we abort too):" ^ Common.exn_to_s e);

      (* but apparently does not work very well in native mode :( so 
       * use mount-lfs.byte *)
      if print_backtrace then begin
        let tmpfile = Common.new_temp_file "trace" ".trace" in
        Common.redirect_stdout_stderr tmpfile (fun () -> 
          (* Features.Backtrace.print(); *)
          (* for 3.11 *)
          let s = Printexc.get_backtrace () in
          pr2 s;
        );
        Common.cat tmpfile +> List.iter (fun s -> 
          log ("trace:" ^ s);
        );
      end;
        
      

      ignore(Unix.alarm 0);
      (* normally should not, but better if more defensive *)
      trans_func.Lfs_persistent.abort (); 
      raise e


let unwind_protect_showexn f = 
  Common.unwind_protect f
    (fun e -> log ("pb with fuse exn = " ^ Common.exn_to_s e))




let create_idle_process mountpoint =
  let pid_idle = Unix.fork () in
  if pid_idle = 0
  then begin
    Thread.delay 10.0; (* enough time to let fuse started *)
    Common.forever (fun () -> 
      Common.command2 ("cd " ^ mountpoint ^ "/__for_idle");
      Thread.delay Flag_lfs.delay_idle;
    );
    pid_idle
  end
  else pid_idle



let encode_ambiguate s id = 
  Common.fileprefix s ^ 
    "<" ^ i_to_s id ^ ">"  ^ 
    (if Common.filesuffix s = "NOEXT" then "" else "." ^ (Common.filesuffix s))

let decode_ambiguate s id = 
  if s =~ "\\(.*\\)<\\([0-9]+\\)>\\(.*\\)"
  then
    let (file, id2, ext) = matched3 s in
    let _ = assert (s_to_i id2 = id) in
    file ^ ext
  else raise Impossible

let _ = Common.example (decode_ambiguate "toto<45>.c" 45  = "toto.c")


(*****************************************************************************)
(* Action *)
(*****************************************************************************)
let action_fuse_command ops = 

  if true then begin
    ignore(ops.Fuse.mknod "a/b/fab" 0o777);
  end;

  if false then begin
    ignore(ops.Fuse.getdir "/plugins/glimpse:ogg/prop_to_test_timeout");
  end;

  ()

  

(*****************************************************************************)
(* The main entry point *)
(*****************************************************************************)
let launch_fuse 
    ~src:path_meta
    ~dest:mountpoint
    ~obj_to_filename
    ~transact_func:trans_func 
    ~timeout 
    ?(print_backtrace=false)
    ?(exec_fuse_command=false) 
    ?(read_only=false)
    argv_fuse =

  (* shortcuts *)
  let simple_stats = 
    simple_stats (*path_meta*) obj_to_filename ~read_only_dir:read_only in
  let transact_with_timeout f = 
    transact_with_timeout trans_func timeout print_backtrace f in

  let pid_idle = 
    if !Flag_lfs.use_idle 
    then create_idle_process mountpoint 
    else -1
  in

  (* unix world *)
  let unixw = ref { 
    (* CONFIG? could use some oassoch ? *)
    cache_path2  = (new oassocb []);
    cache_view  = (new oassocb []);
    dirty_files = (new osetb Osetb.empty);
  }  
  in

  (* XXX1, and search too all the code using now this function. may 
   * have to do a few changes. 
   * should do different stuff when the item is a dir ?
   *)
  let add_cache_path_item (path, item) = 
    let cache = !unixw.cache_path2 in
    let (dir, base) = (dirname path, basename path) in
    let cache' = cache#apply_with_default dir 
      (fun e -> e#add (base, item))
      (fun () -> new oassocb [])
    in
    unixw := {!unixw with cache_path2 = cache'}
  in

  let del_cache_path_item path = 
    let cache = !unixw.cache_path2 in
    let (dir,base) = (dirname path, basename path) in
    let cache' = cache#apply_with_default dir 
      (fun e -> e#delkey base)
      (fun () -> new oassocb [])
    in
    (* XXX1 add also if it is a dir ?*)
    unixw := {!unixw with cache_path2 = cache'}
  in

  let update_cache_dir_entry path = 
    let cache = !unixw.cache_path2 in
    (* let cache = cache#replkey path (new oassocb []) in *)
    unixw := {!unixw with cache_path2 = cache}
  in

  let have_cache_entry path = 
    let cache = !unixw.cache_path2 in
    let (dir,base) = (dirname path, basename path) in
    cache#haskey dir &&  (cache#assoc dir)#haskey base
  in

  (* try first as is and then try basename  ?
   *  no because would have less precise info for the sizedir. better to start
   * by basename.
   *)
  let typeof path = Common.optionise (fun () -> 
    let cache = !unixw.cache_path2 in
    let (dir,base) = (dirname path, basename path) in
    (cache#assoc dir)#assoc base
  ) in
  

  let _dirtyrmdir = ref 0 in
  let _dirtyview = ref 0 in
  let _counterview = ref 2000000 in



  let generate_view = fun path s -> 
    incr _counterview; 
    let id = !_counterview in

    let o = id in

    Path.cd_ path;
    Common.command2("mkdir -p " ^ (Lfs_real.obj_to_path path_meta id));

    (* bugfix: do as in lfs_real.hook_add_file *)
    Unix.close (Unix.openfile (Lfs_real.obj_to_path path_meta o ^ "/" ^ "xxx") 
                   [Unix.O_CREAT] 0o640);
    (* note: could do just a ln (would be faster), but -s is better/clearer. *)
    Unix.symlink "xxx" (Lfs_real.obj_to_path path_meta o ^ "/data");


    Lfs.read s +> Common.write_file 
      (* obj_to_filename now read data as a link, so must have created 
       * the link first 
       *)
      ~file:(Lfs_real.obj_to_filename path_meta id); (* todo: lazy view *)
    (id, !_dirtyview)
  in



  (* getdir is called in some function => take care to not nest 
   * transact_with_timeout. Hence this nested_call param.
   *)
  let rec getdir_func nested_call = fun path -> 
    (if nested_call 
    then (fun f -> f ()) 
    else transact_with_timeout
    ) 
    (fun () ->
      log ("getdir:" ^ path);
      Path.cd_ path;

      Lfs.ls() +> (fun (ps, fs) ->
        Common.if_log3 (fun () -> 
          ps +> List.iter (fun (Prop p,_) -> 
            log3 ("dirs:" ^ p));
          fs +> List.iter (fun id -> 
            log3 ("files:" ^ Lfs.id_to_filename id));
        );

        update_cache_dir_entry path;
        (* XXX1 dircomputed was here to disambiguate stuff. When found
         * an entry for instance for /a in the cache, don't know if
         * here cos there was a ls done from / or that because there
         * was a ls done from /a. So we don't know if there was a ls
         * from /a. Then when lookup for a/xxx and a/xxx does not exist
         * in the cache, we should call ls. But if xxx really does not
         * exist, we don't want to call ls. We would like to answer no.
         * Indeed when launch for instance a command such as ls, there
         * is many lookup involved, for glibc, for path, ... and we
         * would like to answer fastly no to all those lookup. Hence
         * this dircomputed. *)
        add_cache_path_item (Filename.concat path  "_dircomputed", 
                            (Dir (0,!_dirtyrmdir)));


        (* XXX1 obsolete now ? *)
	fs +> iter (fun id -> 
          let s = Lfs.id_to_filename id in
          del_cache_path_item (Filename.concat path s)
        );

        (* no need to add in cache ? I think that Linux handle that
         * internally, so never get call on a getattr "." or ".." I think. *)
        ([".";".."])  
        ++
	  (ps +> map (fun ((Prop p),i) -> 
            add_cache_path_item (Filename.concat path p,(Dir(i,!_dirtyrmdir)));
            p)  
          )
	++
	  (fs +> map (fun id -> 
            let s = Lfs.id_to_filename id in
            let s =
              if Lfs.lfs_mode !w = Files
              then 
                let s = 
                  if have_cache_entry (Filename.concat path s)
                  then encode_ambiguate s id
                  else s
                in
                begin
                  add_cache_path_item (Filename.concat path s, (File id));
                  s
                end
              else 
                let valu = 
                  match optionise (fun () -> !unixw.cache_view#assoc (Filename.concat path s)) with
                  | Some (id, ver) when ver = !_dirtyview -> View (id, ver)
                  | _ -> 
                      let (id, ver) = generate_view path s in
                      unixw := {!unixw with cache_view = !unixw.cache_view#add (Filename.concat path s , (id, ver))};
                      (View (id, ver))
                in
                begin
                  add_cache_path_item (Filename.concat path s, valu);
                  s
                end
            in
            s
          )
          )
      );
    )
  in 
  let getattr_func = fun path -> 
    if path = "/__for_idle" 
    then begin 
      log3 "IDLEEEEEEEEEEEE"; 
      trans_func.Lfs_persistent.checkpoint();
      trans_func.Lfs_persistent.archives () +> List.iter (fun s -> 
        log ("erasing log file:" ^ path_meta ^ "/" ^ s);
        Common.command2 ("rm " ^ path_meta ^ "/" ^ s);
      );
          
      simple_stats (Dir (0, 0)) 
    end
    else 
      transact_with_timeout (fun () ->
      unwind_protect_showexn (fun () -> 
	log2 ("getattr:" ^ path);
	match typeof path with
	| Some (View (o, ver)) -> 
            if ver = !_dirtyview 
            then (simple_stats (View (o, ver)))
            else begin
              (*  not up-to-date view *)

              let (id, ver) = generate_view (dirname path) (basename path) in
              unixw := {!unixw with 
                cache_view = !unixw.cache_view#add (path , (id, ver))
              };
              add_cache_path_item (path, View (id, ver));
              simple_stats (View (id, ver))
            end
        | Some (Dir (i,ver)) when ver = !_dirtyrmdir -> 
            simple_stats (Dir (i,ver))
        | Some (File o) -> (simple_stats (File o))
	| _ -> 
            try Path.cd_ path; 
              add_cache_path_item (path, Dir (0,!_dirtyrmdir));
              simple_stats (Dir (0,!_dirtyrmdir))
            with 
            | Timeout -> raise Timeout
            | e -> 
                (* XXX1 *)
                (match typeof ((dirname path) ^ "/_dircomputed") with
                | Some _ -> 
                    log ("error getattr = " ^ Printexc.to_string e);
                    (* cant do just a failwith "error", have to return a
                     * specific error, otherwise mkdir/... dont work *)
                    raise (Unix.Unix_error (Unix.ENOENT, "stat", path)) 
                | None -> 
                    ignore(getdir_func true (dirname path));
                    (match typeof path with
                    | Some ((Dir (i, ver)) as x) when ver = !_dirtyrmdir -> 
                        (simple_stats x)
                    | Some ((File _ | View _) as x) -> (simple_stats x)
                    | _ -> 
                        log "error getattr";
                        (* cant do just a failwith "error", have to return
                         * a specific error, otherwise mkdir/... dont work *)
                        raise (Unix.Unix_error (Unix.ENOENT, "stat", path)) 
                    )
                )
      )
      )

  in

 (try 
   let ops = 
    { Fuse.default_operations with 

      Fuse.getdir  = getdir_func false;
      Fuse.getattr = getattr_func;

      read = (fun path dest offset -> 
	log ("read:" ^ path);
	match typeof path with
	| Some ((File o)|(View (o,_))) -> 
            xmp_read (obj_to_filename o) dest offset
        | _ -> raise Impossible
      );

      write = (fun path src offset -> 
	log ("write:" ^ path);
        unixw := {!unixw with dirty_files = !unixw.dirty_files#add path};
	match typeof path with
	| Some ((File o)|(View (o,_))) -> 
            xmp_write (obj_to_filename o) src offset
        | _ -> raise Impossible
      );

      truncate = (fun path size -> 
	log ("truncate:" ^ path);
        unixw := {!unixw with dirty_files = !unixw.dirty_files#add path};
	match typeof path with
	| Some ((File o)|(View (o,_))) -> 
            Unix.truncate (obj_to_filename o) 
              (Int64.to_int size)
        | _ -> raise Impossible
      );

	

      fopen =   (fun path flags -> 
       unwind_protect_showexn (fun () -> 
	log ("fopen:" ^ path);
        ignore(getattr_func path);
        (* todo:? lazy view contect *)
	match typeof path with
	| Some (File o) ->     
            ()
              (* CONFIG *)
              (* Unix.close (Unix.openfile (obj_to_filename o) flags 0) *)
              (* XXX2 cos pb with tar, with links *)
	| Some (View (o,_)) -> 
            Unix.close (Unix.openfile 
                           (obj_to_filename o) flags 0)
        | _ -> raise Impossible
        )
      );

       
      release = (fun path mode -> 
       transact_with_timeout (fun () ->
	log ("release:" ^ path);
        if !unixw.dirty_files#mem path then
          unwind_protect_showexn (fun () -> 
	    (match typeof path with
	    | Some (File o) -> 	
                Path.cd_ (dirname path); 
                Lfs.write (Right o) "";
	    | Some (View (o, ver)) when ver = !_dirtyview -> 	
                Path.cd_ (dirname path); 
                Lfs.write (Left (basename path)) 
                  (Common.read_file (obj_to_filename o));
                incr _dirtyview;
            | _ -> raise Impossible
            );
            unixw := {!unixw with dirty_files = !unixw.dirty_files#del path};
          )
       )
      );

      flush = (fun path -> 
	log ("flush:" ^ path);
      );
      (* tofix?: but seems dont work, when I do a sync in a shell, 
       * this function is not called :( *)
      fsync = (fun path ds -> 
	log ("fsync:" ^ path);
      );


      mknod = (fun path mode -> 
       transact_with_timeout (fun () ->
        unwind_protect_showexn (fun () -> 
	  log ("mknod:" ^ path);

          if read_only 
          then raise (Unix.Unix_error (Unix.ENOENT, "stat", path));

	  Path.cd_ (dirname path);
	  let o = Lfs.mkfile (basename path) "" None in
          Unix.chmod (Lfs_real.obj_to_path path_meta o ^ "/" ^ (basename path))
            mode;

          (* replace by better getattr ? no, cos when create file, if do
           * a clean gettattr then the file may not be here after the mknod
           * (cos of lfs), and so getattr will fail and the tool 
           * (cp, emacs, ...) will not continue. With perlfs we didnt have
           * such a pb cos mkfile was returning an inode and read_inode
           * worked. So no pb, but here no more the case
           * 
           * todo: dirty something 
           *)
          add_cache_path_item (path, (File o));

	  log ("mknod-ending:" ^ path);
          (* zarb: otherwise get segfault when detarring usr.tar *)
          (* Gc.full_major (); *)
          )
        );
      );

      mkdir = (fun path mode -> 
        transact_with_timeout (fun () ->
	  log ("mkdir:" ^ path);
	  Path.cd_ (dirname path);
	  Lfs.mkdir (basename path);
        )
      );


      unlink = (fun path -> 
        ignore(getattr_func path);
        transact_with_timeout (fun () ->
	  log ("unlink:" ^ path);

          if read_only 
          then raise (Unix.Unix_error (Unix.ENOENT, "stat", path));

          (* no more needed now that directly rm via the idfile *)
	  Path.cd_ (dirname path); 
	  match typeof path with
	  | Some (File o) ->    
              (* now file can be in the foo<inode>.c format, so cant rely
               * just on the filename.
	       * old: let o = Lfs.rm (Left (basename path)) in *)
              let _obis = Lfs.rm (Right o) in
              
              (* XXX1 todo: should invalidate other path, or make a 
                 dirty_something *)
              del_cache_path_item path;
          | _ -> raise (Unix.Unix_error (Unix.ENOENT, "stat", path))
        )
      );
      rmdir = (fun path -> 
       transact_with_timeout (fun () ->
	 log ("rmdir:" ^ path);

	 Path.cd_ (dirname path);
	 Lfs.rmdir (basename path);

         incr _dirtyrmdir;
         (* XXX1 *)
         del_cache_path_item path;
       )
      );

      (* Cant reuse the noprop trick, cos fuse does some work in our back :( 
       * Why use noprop trick in the first place ? cos if do 
       *   mv ext:c  type:Program/ then tools (mv, libc, ...) first try to see 
       * if there is already a dir type:Program/ext:c, and indeed there is :) 
       *  => forbidden to mv a dir in dir where there is such a subdir 
       *  => use the noprop trick 
       * But with fuse, when first fo mv ext:c type:Program/noprop,  
       * then fuse assume that now type:Program/noprop is a valid directory, 
       * and so  in the second mv ext:h type:Program/noprop,  as 
       * type:Program/noprop is assumed (without asking us) a dir, the mv
       * is translated in a   mv ext:h type:program/noprop/ext:h  :(   ) 
       *)
      rename = (fun oldpath newpath -> 
        ignore(getattr_func oldpath);
        (try (ignore(getattr_func newpath)) 
          with Timeout -> raise Timeout   | e -> ()
        );
        transact_with_timeout (fun () ->
	  log ("rename:" ^ oldpath ^ "|||" ^ newpath);

          if read_only 
          then raise (Unix.Unix_error (Unix.ENOENT, "stat", newpath));

	  let oldname = (basename oldpath) in
	  let newname = (basename newpath) in
	  Path.cd_ (dirname oldpath);
          log3 ("ic1:");
	  (match typeof oldpath with
	  | Some (File o1) -> 
	      (match typeof newpath with
              | Some (File o) -> 
                  (* if new exist then delete it (part of the 
                   * semantic/requirment of rename)
                   * coupling: copy paste of unlink 
                   *)
	          Path.cd_ (dirname newpath);
                  log3 ("ic2:");
	          let _o = Lfs.rm (Left (basename newpath)) in
                  log3 ("ic3:");
                  Path.cd_ (dirname oldpath); (*  go back to good dir *)
              | _ -> ()
              );
              let newname = 
                if ((oldname =~ ".*<[0-9]+>.*") && newname = oldname)
                then decode_ambiguate oldname o1
                else newname
              in

              Lfs.mv (Right o1) (Path.parse_path (dirname newpath)) newname;
              (* XXX1 todo: dirty something *)
              del_cache_path_item oldpath;
              (* todo: bug ? if dir with mutliple file, the file mv in
               * lfs may not be o1 *)
              add_cache_path_item (newpath, (File o1));
	  | Some (Dir _) -> (* XXX BUG with dirty_rmdir ? *)
	      Lfs.mvdir oldname (Path.parse_path (dirname newpath))
	        (* note: noprop tricks cos shell tools are not used to lfs *)
                (match () with
                (* old: | _ when newname = "noprop" -> oldname *)
                | _ when newname =~ "noprop_\\(.*\\)" -> matched1 newname
                | _ when newname = oldname -> raise Impossible
                | _ -> newname
                );
              incr _dirtyrmdir;
	  | _ -> raise Impossible
	  );
        )
      );

      chmod =   (fun path file_perm -> 
	log ("chmod:" ^ path);
	match typeof path with
	| Some (File o) -> 
            Unix.chmod (obj_to_filename o) file_perm
        | _ -> raise Impossible
      );

      chown =   (fun path uid gid -> 
	log ("chown:" ^ path);
	match typeof path with
	| Some (File o) -> 
            Unix.chown (obj_to_filename o) uid gid
        | _ -> raise Impossible
      );

      utime = (fun path accesstime modiftime -> 
	log ("utime:" ^ path);
	match typeof path with
	| Some (File o) -> 
            Unix.utimes (obj_to_filename o) 
              accesstime modiftime
        | Some (Dir _) -> 
            () (* CONFIG #XXX1 marre des messages *)
        | _ -> raise Impossible
      );
        
      (* For LFS a symlink is just a regular file (without content). So
       * the work is done here (and specially in getattr). O LFS dont know
       * about symlink (todo ?) 
       *)
      symlink = (fun sourcepath destpath -> 
        transact_with_timeout (fun () ->
	  log ("symlink:" ^ sourcepath ^ "|||" ^ destpath);

          if read_only 
          then raise (Unix.Unix_error (Unix.ENOENT, "stat", sourcepath));

          
	  Path.cd_ (dirname destpath);
	  let o = Lfs.mkfile (basename destpath) "" None in
          Common.command2 ("rm -f "^(Lfs_real.obj_to_path path_meta  o)^"/*"); 
          Common.command2 ("rm -f "^(Lfs_real.obj_to_path path_meta  o)^"/.*");
          
          Common.command2 ("cd " ^ (Lfs_real.obj_to_path path_meta o) ^ ";" ^
                           "ln -s \"" ^ sourcepath  ^ "\"  " ^ 
                              (basename destpath));
          (* robust?: if the link point to a lfs absolute path  ? 
           * recursive call to fuse => deadlock, ? 
           *
           * could do just a ln (would be faster), but -s is better/clearer *)
          Common.command2 ("cd " ^ (Lfs_real.obj_to_path path_meta o) ^ ";" ^
                           "ln -s \"" ^ (basename destpath) ^ "\"  data"); 

          (* todo: dirty XXX1 *)
          add_cache_path_item (destpath, (File o));
        )
      );

      readlink = (fun path -> 
	log ("readlink:" ^ path);
	match typeof path with
	| Some (File o) -> 
            Unix.readlink (obj_to_filename o)
(* old:
            let realo = Unix.readlink (obj_to_filename o) in
            Unix.readlink ((Lfs_real.obj_to_path path_meta o) ^ "/" ^ realo)
*)
        | _ -> raise Impossible
      );

      link = (fun path1 path2 -> 
        transact_with_timeout (fun () -> 
          log ("link:" ^ path1 ^ "|||" ^ path2);

          if read_only 
          then raise (Unix.Unix_error (Unix.ENOENT, "stat", path1));
	  
          assert_equal (dirname path1) (dirname path2);
          let file1 = (basename path1) in
          let _ofile1 = match typeof path1 with 
            | Some (File o) -> o 
            | _ -> failwith "pb"
          in
         
	  Path.cd_ (dirname path1);
          match (basename path2) with
          | s when s =~ "\\(<.*:of>\\)\\(.*\\)" -> 
              let (rel, file2) = matched2 s in
              Lfs.ln file1 rel file2
          | _ -> failwith "not good syntax for relation"
          (* does not mean anything, but just to make ln command happy
           * (and not report after link creation that there is no such file
           * as <father:of>xxx
           *)
          (* unixw := {!unixw with cache_path = 
             !unixw.cache_path#add (path2, (File ofile1))}; 
          *)
        );
      );
    } 
   in
   if exec_fuse_command 
   then begin 
     action_fuse_command ops;
     Common.pause();
     exit 0;
   end;

   (* ignore(Thread.create (fun () -> Fuse.main argv_fuse ops) ())) *)
   Fuse.main argv_fuse ops
     
   with e -> log ("pb with fuse exn = " ^ Printexc.to_string e)
 );

  (* todo: opti: could gcise the prop (and specially vattr) with 
   * empty extension 
   *)
  log "umount";

  if !Flag_lfs.use_idle 
  then Unix.kill pid_idle Sys.sigkill;

  trans_func.Lfs_persistent.final();
  hook_action_umount +> Common.run_hooks_action ();
  ()
