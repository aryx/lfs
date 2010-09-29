(* take a meta path, load the world from the disk, and return
 * transaction functions that can be used by the caller such as fuse
 * that will need to commit or abort after each filesystem operation.
 * 
 * effect!: modify Lfs.w (LFS world global variable) !
 *)
val launch_bdb: string -> bool (* use_transact *) -> Lfs_persistent.database
