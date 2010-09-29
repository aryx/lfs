
(* There are multiple libraries to make LFS data structures persistent: 
 *  - GDBM
 *  - berkeley DB
 *  - QDBM.
 * 
 * Some of those libaries support "transactions" by exporting funtions such as commit/abort. 
 * Those functions must then be used by callers such as Fuse that will need
 * to commit or abort after each filesystem operation. The interface
 * below tries to abstract away from the choice of the concrete persistent
 * library.
 * 
 * It is a record but in fact it's an object. I use closure to hide the
 * representation. I could maybe use classes instead, as I did for the
 * oassoc.
 * 
 * So this type contains methods (final, commit, abort, etc) and the
 * different databases, hidden inside the closures and hidden inside
 * the oassoc classes in lfs.ml world. So, this file roughly
 * corresponds to a kind of assoc.ml, but agglomerates multiple oassoc
 * under the same umbrella type.
 * 
 *)

type database = {
  final:  unit -> unit;

  (* only for transactions *)
  commit: unit -> unit;
  abort:  unit -> unit;

  (* maybe quite specific to berkeley DB ... *)
  checkpoint: unit -> unit;
  archives: unit -> string list;
} 


