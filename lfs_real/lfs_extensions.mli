
(* effects!: the following functions modify some hooks in Lfs *)

val install_inode_extension: unit -> unit
val install_datetoday_extension: unit -> unit
val install_stree_glimpse_and_co_extension: 
  string (* metapath *) -> bool (* use_glimpse *) -> unit
