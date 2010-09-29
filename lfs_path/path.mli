val parse_path: string -> Lfs.path

val cd_: string -> unit
val mv_: Lfs.filename -> string -> unit
val mvdir_: string -> string -> unit
val mkdir_: string -> unit
val mkfile_: string -> Lfs.filecontent -> unit
val dopath_ : string -> (unit -> 'a) -> 'a
(* val adjust_func_for_relation : unit -> unit *)
