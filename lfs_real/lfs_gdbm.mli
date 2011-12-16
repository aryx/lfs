(* take a meta path, load the world from the disk, and return
 * transaction functions that can be used by caller such as fuse
 * that will need to close for instance the database.
 *
 * effect!: modify Lfs.w (LFS world global variable) !
 *)
val launch_gdbm: string -> Lfs_persistent.database
