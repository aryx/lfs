open Common 

type database = {
  metapath : string;
  lfs_world : Lfs.world ref;
  transact : Lfs_persistent.database;
  real_path : (Lfs.objet, filename) Oassoc.oassoc;
}
val create_db : metapath:filename -> use_transact:bool -> database
val open_db : metapath:filename -> use_transact:bool -> database

val close_db : database -> unit


val obj_to_filename : database -> Lfs.idfile -> filename

val fsck_fast : bool ref
val rm_obj_if_pb : bool ref

val launch_semireal :
  metapath:filename ->
  use_transact:bool ->
  Lfs_persistent.database * (Lfs.idfile -> filename)
