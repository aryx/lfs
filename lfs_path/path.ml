open Common

open Lfs

(*****************************************************************************)
let (parse_path: string -> path) = fun s -> 
  try Pathparser.main Pathlexer.token (Lexing.from_string s)
  with x -> 
    log ("bad path:" ^ s ^ "\n");
    raise x

(*****************************************************************************)
(* sugar
 *  caml sux, need wrapper and no default value for parameters 
 *  => some sugar (otherwise cd "art" become cd (Element Single Prop "art")
 *  => command_ = command with little sugar, must keep in mind that same
 *   spirit
 * 
 * put here cos used both by demo and lfs_fuse.
 *)

let dopath_ path f = Lfs.dopath (parse_path path) f

let cd_ path      = List.iter Lfs.cd (parse_path path)
let mv_ s path    = Lfs.mv    (Left s) (parse_path path) s
let mvdir_ s path = Lfs.mvdir s (parse_path path) s

let mkdir_  path         = 
  dopath_ (dirname path) (fun () -> 
    Lfs.mkdir (basename path))

let mkfile_ path content = 
  dopath_ (dirname path) (fun () -> 
    ignore(Lfs.mkfile (basename path) content None))

(* because I don't want that my clean lfs.ml depend on path.ml, 
 * just because some extension do.
 * ugly:
 *)
let adjust_func_for_relation () = 
  Lfs.relation_parse_path := parse_path

let _ = Lfs.relation_parse_path := parse_path
