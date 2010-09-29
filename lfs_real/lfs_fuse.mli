(* take the metapath, the transaction function, and the fuse argv, 
 * and launch fuse.
 *)
val launch_fuse: 
  src:string -> 
  dest:string -> 
  obj_to_filename:(Lfs.objet -> Common.filename) -> 
  transact_func:Lfs_persistent.database -> 
  timeout:int -> 
  ?print_backtrace:bool -> 
  ?exec_fuse_command:bool -> 
  ?read_only:bool -> 
  Fuse_bindings.str array -> 
  unit

val hook_action_umount : (unit -> unit) list ref

