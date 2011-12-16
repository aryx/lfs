(************************************************************************************)
open Common
open Lfs

(* note: dont change the order between Fuse_bindings and Fuse, otherwise scope of record namespace pb *)
open Fuse_bindings
open Fuse

open Unix

(************************************************************************************)
let prg_time = time ()

let simple_stats ?(kind=S_REG) size =
  {
    LargeFile.st_kind = kind;
    st_perm =
     if kind = S_DIR
     then file_perm_of [`R;`X] [`R;`X] [`R;`X]
     else file_perm_of [`R;`W] [`R;] [`R]
	 ;
    st_size = size;
    st_uid = getuid ();
    st_gid = getgid ();
    st_atime = prg_time;
    st_mtime = prg_time;
    st_ctime = prg_time;
    st_nlink = 1;
    st_dev = 0;
    st_ino = 0;
    st_rdev = 0;
  }
(************************************************************************************)
let main argv_fuse =

  let htype = hcreate () in
  let typeof = (fun path ->
    try Some (hfind path htype)
    with _ ->
      try Path.cd_ path; Some (S_DIR, 0)
      with _ -> None
   ) in



  let _ = start_log_file() in


  Fuse.main argv_fuse
    {
      default_operations with

      getdir = (fun path ->
	  Path.cd_ path;
	  Lfs.ls() +> (fun (ps, fs) ->
	     (ps +> map (fun ((Prop p),i) -> htype +> hadd (path ^ "/" ^ p, (S_DIR, i)); p)  )
	      ++
	     (fs +> map (fun id -> let s = (wrap_file id !w).filename in
		 htype +> hadd (path ^ "/" ^ s, (S_REG,
						 let contents = (Lfs.read s) in
						 slength contents));
						 (*0));*)
		 s
	         )
	       )
	  );
	 );

      getattr = (fun path ->
	  match typeof path with
	  | Some (typ, size) -> (simple_stats ~kind:typ (size +> Int64.of_int))
	  | None -> failwith "error"

	  );

      read = (fun path dest offset ->
	  let offseti = Int64.to_int offset in
	  Path.cd_ (dirname path);
	  let contents = Lfs.read (basename path) in
	  let len = max 0 (min (String.length contents - offseti) (String.length dest)) in
	  String.blit contents offseti dest 0 len; len
	  );


      fopen =   (fun path flags -> () );
      release = (fun path mode -> ());


      mknod = (fun path mode ->
	  Path.cd_ (dirname path);
	  ignore(Lfs.mkfile (basename path) "" None);
	  );
      mkdir = (fun path mode ->
	  Path.cd_ (dirname path);
	  Lfs.mkdir (basename path);
	  );


      unlink = (fun path ->
	  Path.cd_ (dirname path);
	  Lfs.rm (basename path);
	  );
      rmdir = (fun path ->
	  Path.cd_ (dirname path);
	  Lfs.rmdir (basename path);
		);

      rename = (fun oldpath newpath ->
	let oldname = (basename oldpath) in
	let newname = (basename newpath) in
	Path.cd_ (dirname oldpath);
	(match typeof oldpath with
	| Some (S_REG,_) ->
	    Lfs.mv oldname (Path.parse_path (dirname newpath)) newname
	| Some (S_DIR,_) ->
	    Lfs.mvdir oldname (Path.parse_path (dirname newpath))
	          (* note: noprop tricks cos shell tools are not used to lfs *)
                  (match () with
                  | _ when newname = "noprop" -> oldname
                  | _ when newname = oldname -> raise Impossible
                  | _ -> newname
                  )
	| _ -> raise Impossible

	);

	);


(*
	write = xmp_write;
	truncate = Unix.LargeFile.truncate;

	chmod = Unix.chmod;
	chown = Unix.chown;
	utime = Unix.utimes;

	flush = (fun path -> ());
	fsync = (fun path ds -> Printf.printf "sync\n%!");

	link     = Unix.link;
	symlink  = Unix.symlink;
	readlink = Unix.readlink;
*)
    }

(************************************************************************************)
let _ =
  match (Array.to_list Sys.argv ) with
  | x::ys -> main (Array.of_list (x::"-s"::"-o"::"max_read=16384"::ys))
  | _ -> failwith "pb"

