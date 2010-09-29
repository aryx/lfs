open Common

(* real mode means that the object in the world will point to real file
 * on the disk. It's different from lfs_persistent that talks about
 * how to store the index. lfs_real talks about how to store the files.
 *)

val obj_to_path:     string (* metapath *) -> Lfs.idfile -> filename
val obj_to_filename: string (* metapath *) -> Lfs.idfile -> filename

val fsck_fast : bool ref
val mv_lost_found_if_pb : bool ref
val rm_obj_if_pb : bool ref

(* this function is also added in Lfs.hook_action_check_world *)
val check_world_real : string (* metapath *) -> Lfs.world -> unit


(* Take metadata-path and adjust the hooks in Lfs to prepare it for
 * working in real mode, which means that the object in the world will
 * point to real file on the disk.
 * 
 * effect!: on the hooks in Lfs 
 * 
 * update: because I now have lfs_semireal.ml, lfs_fuse  must be independent
 * of lfs_real and so now we must give to lfs_fuse a obj_to_filename.
 *)
val launch_real: 
  string (* metapath *) -> (Lfs.idfile -> filename)
