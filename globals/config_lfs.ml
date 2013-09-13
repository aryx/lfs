let version = "0.5.1"

let path =
  try (Sys.getenv "LFS_HOME")
  with Not_found->"/usr/local/share/lfs"

(* old:    Filename.concat (Sys.getenv "HOME") "/c-lfs" *)
