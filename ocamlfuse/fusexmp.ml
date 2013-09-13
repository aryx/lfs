open Unix
open Fuse_bindings
open Fuse
open Thread

(* Read and write operations a la fuse *)

let xmp_read path dest offset =
  let h = openfile path [O_RDONLY] 0 in
    try
      ignore (LargeFile.lseek h offset SEEK_SET);
      let res = read h dest 0 (String.length dest) in
	close h;
	res
    with x ->
      close h;
      raise x

let xmp_write path src offset =
  let h = openfile path [O_WRONLY] 0 in
    try
      ignore (LargeFile.lseek h offset SEEK_SET);
      let res=write h src 0 (String.length src) in
	close h;
	res
    with x ->
      close h;
      raise x

(* Extended attributes helpers *)

let xattr = Hashtbl.create 256
let xattr_lock = Mutex.create ()

let with_xattr_lock f x =
  Mutex.lock xattr_lock;
  let v =
    try f x
    with e ->
      Mutex.unlock xattr_lock;
      raise e in
    Mutex.unlock xattr_lock;
    v

let lskeys t = with_xattr_lock (Hashtbl.fold (fun k v l -> (k::l)) t) []
let init_attr xattr path = if not (Hashtbl.mem xattr path) then Hashtbl.add xattr path (Hashtbl.create 256)

(* call to Fuse.main and creation of the filesystem *)

let _ =
  main Sys.argv
    {
      default_operations with
	getattr = LargeFile.lstat;
	getdir = (fun path -> Array.to_list (Sys.readdir path));
	readlink = Unix.readlink;
	utime = Unix.utimes;
	fopen = (fun path flags -> Unix.close (Unix.openfile path flags 0));
	read = xmp_read;
	write = xmp_write;
	mknod = (fun path mode -> close (openfile path [O_CREAT;O_EXCL] mode));
	mkdir = Unix.mkdir;
	unlink = Unix.unlink;
	rmdir = Unix.rmdir;
	symlink = Unix.symlink;
	rename = Unix.rename;
	link = Unix.link;
	chmod = Unix.chmod;
	chown = Unix.chown;
	truncate = Unix.LargeFile.truncate;
	release = (fun path mode -> ());
	flush = (fun path -> ());
	fsync = (fun path ds -> Printf.printf "sync\n%!");
	listxattr = (fun path -> init_attr xattr path;lskeys (Hashtbl.find xattr path));
	getxattr = (fun path attr ->
		    with_xattr_lock (fun () ->
				       init_attr xattr path;
				       Hashtbl.find (Hashtbl.find xattr path) attr) ());
	setxattr = (fun path attr value flag ->
		    with_xattr_lock (fun () ->
				       init_attr xattr path;
				       Hashtbl.replace (Hashtbl.find xattr path) attr value) ());
	removexattr = (fun path attr ->
		       with_xattr_lock (fun () ->
					  init_attr xattr path;
					  Hashtbl.remove (Hashtbl.find xattr path) attr) ());
    }
