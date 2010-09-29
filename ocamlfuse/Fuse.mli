type context = Fuse_bindings.__fuse_context
val get_context : unit -> context
type xattr_flags = AUTO | CREATE | REPLACE
type statfs = {
  f_bsize : int;
  f_blocks : int;
  f_bfree : int;
  f_bavail : int;
  f_files : int;
  f_ffree : int;
  f_namelen : int;
}
type operations = {
  getattr : string -> Unix.LargeFile.stats;
  readlink : string -> string;
  getdir : string -> string list;
  mknod : string -> int -> unit;
  mkdir : string -> int -> unit;
  unlink : string -> unit;
  rmdir : string -> unit;
  symlink : string -> string -> unit;
  rename : string -> string -> unit;
  link : string -> string -> unit;
  chmod : string -> int -> unit;
  chown : string -> int -> int -> unit;
  truncate : string -> int64 -> unit;
  utime : string -> float -> float -> unit;
  fopen : string -> Unix.open_flag list -> unit;
  read : string -> string -> int64 -> int;
  write : string -> string -> int64 -> int;
  release : string -> Unix.open_flag list -> unit;
  flush : string -> unit;
  statfs : string -> statfs;
  fsync : string -> bool -> unit;
  listxattr : string -> string list;
  getxattr : string -> string -> string;
  setxattr : string -> string -> string -> xattr_flags -> unit;
  removexattr : string -> string -> unit;
}
val op_names_of_operations : operations -> Fuse_bindings.fuse_operation_names
val default_operations : operations
val main : Fuse_bindings.str array -> operations -> unit
