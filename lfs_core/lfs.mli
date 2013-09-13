open Ofullcommon

val copyright : unit -> unit

(*****************************************************************************)
(* The main "user" LFS interface (used by demo.ml for example) *)
(*****************************************************************************)
type property = Prop of string

(* an object may represent either a file, or a part of file *)
type objet = identity
  and identity = int

(* interface of the logic plugin *)
type logic = property -> property -> bool (* mean: a |= b ? *)


(* the built-in basic propositional logic *)
type formula =
  | Single of property
  | And    of formula * formula
  | Or     of formula * formula
  | Not    of formula

val string_of_prop: property -> string

(*---------------------------------------------------------------------------*)
type filename = string

type filecontent = string
type partcontent = string

(* LFS can work in two modes:
 *   - core LFS: all is in memory, which is convenient to debug, test,
 *     experiment new features,
 *   - real LFS: files are on the disk (but possibly not the index)
 * update: in fact there is now a third mode, semi-real LFS which is
 *  really similar to the real mode, it's just that the files are not
 *  stored in a meta-lfs/.
 *
 * Depending on the mode, some field value may be interpreted in different way:
 * - the value contain directly the content (in core LFS),
 * - the value contain an identifier from where we will be able
 *   to get the path to a disk file (in real LFS).
 *
 * So have in mind fakecontent = filecontent when work in core LFS.
 * If want print some beautiful code,  s/fake/file, and s/core.*__(.+)/$1/
 * By default LFS work in core mode. To switch to real mode, see the end
 * of this file that tells how to customize LFS.
 *)
type fakecontent = Core of filecontent | Real of objet


(* interface of the transducers plugins *)
type transducer     = fakecontent -> property set
type adv_transducer = partcontent list -> property set list

(* normally the transducer just need the content but for the name: and so
 * on in fact we care more about the filename, hence as a special case
 * the system transducer also get the filename.
 *)
val transducer_system: filename -> transducer
(*---------------------------------------------------------------------------*)
(* The shell interface *)
(*---------------------------------------------------------------------------*)

type path_element = Slash | Dot | DotDot | Element of formula
type path = path_element list

(* some functions can accept either a filename or directly an id *)
type filename_or_id = (filename, objet) either

type plugin =
   | Logic of logic
   | Trans of transducer
   | AdvTrans of adv_transducer

(* effect!: all the following functions operate by working or doing some
 * effects internally on the world (w) defined later in this file.
 *)

val make_default_world: unit -> unit

(* some functions can be made more efficient if do less checks *)
val lfs_check : bool ref
val lfs_allow_cd_parts : bool ref

val pwd:     unit -> formula

val cd:      path_element -> unit
val ls:      unit -> (property * int) set * objet set

val mkdir:   string -> unit
val mkfile:  filename -> filecontent -> plugin option -> objet

val rm:      filename_or_id -> objet
val mv:      filename_or_id -> path -> filename -> unit

val rmdir:   string -> unit
val mvdir:   string -> path -> string -> unit

val read:    filename -> filecontent
val write:   filename_or_id  -> filecontent -> unit
(* todo: change what follows = filecontent argument is in fact used
 * only when it is a view =>
 *)

val ln: filename -> string -> filename -> unit

(* some helpers *)
val dopath: path -> (unit -> 'a) -> 'a
val id_to_filename: objet -> filename (* use also internally the global w *)
val ls_bis : unit -> string list

val sanitize_lfs : string -> string

(*---------------------------------------------------------------------------*)
(*---------------------------------------------------------------------------*)

(* valued attribute (vattr) are important in LFS, used to provide advanced
 * logic. Here we define some accessor to a vattr *)
val is_attr:  property -> bool
val is_vattr: property -> bool
val value_vattr: property -> property
val attr_vattr: property -> property

type misc_options = {
  ls_mode:    ls_mode;
  mkdir_mode: mkdir_mode;
  cd_mode:    cd_mode;
  view_mode:  view_mode;
}
  and ls_mode    =
    | Strict | Relaxed
    | Ext | Int
    | Best | CA
    | Classic | Parents
   (* less: could also do ExtBoth, FileNumbers of int, ...  *)
  and mkdir_mode = Normal | Compat
  and cd_mode    = AllowJump | NoJump
  and view_mode  =
    | SingleV of property
    | OrV  of property * property
    | NotV of property
    | SpecialV

val default_ls_mode : ls_mode ref

(*****************************************************************************)
(* Interface to manipulate (load, test, stat) the world *)
(*****************************************************************************)
(* mainly used by real mode of LFS *)

(* Need access to w in real LFS and in extensions so have to export
 * its type. I use an object technique to have generic code, to allow
 * the data structure to be either in mem or on disk. For instance
 * sometimes oassoc can be a oassoch, that is a hash table in memory,
 * or sometimes it can be oassocdbm or oassocbdb in which case the
 * assoc is made persistent and use internally some Btree provided by
 * GDBM or Berkeley DB. *)
type world = {
  graphp : iproperty ograph;

  prop_iprop : (property, iproperty) oassoc;
  iprop_prop : (iproperty, property) oassoc;

  cache_is_formula : (property, property oset) oassoc;

  files    : (idfile, file) oassoc;
  extfiles : (iproperty, idfile oset) oassoc;

  parts     : (idfile, parts_info) assoc;
  extparts  : (iproperty, idpart oset) oassoc;
  partsfile : (idpart, idfile) oassoc;

  (* used only in core LFS (or to put solver in builtin) *)
  plugins : (idfile, plugin) assoc;

  pwd_history : (formula * whichmode * misc_options) stack;
}

 and whichmode = Files | Parts
 and parts_info (* hidden, no need to expose *)

 and idfile = objet
 and idpart = objet

 and iproperty = Iprop of int

 and file = {
   filename : filename;
   extrinsic : iproperty set;
   intrinsic : iproperty set;
   fcontent : fakecontent;
 }
 and part = {
   fromfile     : idfile;
   pdescription : iproperty set;
   pcontent     : partcontent;
 }

val lfs_mode: world -> whichmode

val default_world : unit -> world

val string_of_descr_obj : objet -> world -> string
val string_extr_of_descr_obj : objet -> world -> string



val stat_world:  world -> unit
val check_world: world -> unit

(* !!the big global world variable!! *)
val w : world ref


val iroot : iproperty
val root : property


val emptysb: unit -> 'a oset
val empty_ext: unit -> objet oset

val empty_partsinfo : (unit -> (idpart, part) oassoc) ref
val empty_extparts :  (unit -> (iproperty, objet oset) oassoc) ref

(*****************************************************************************)
(* Interface to customize LFS (core vs real) *)
(*****************************************************************************)

val _realfs : bool ref

val core_get_fcontent__id           : (fakecontent -> filecontent) ref
val core_get_size_fcontent__slength : (fakecontent -> string)      ref
val core_get_date_fcontent__today   : (fakecontent -> Unix.tm)      ref
val core_set_fcontent__fst          : (fakecontent * objet -> fakecontent) ref
val core_update_fcontent__fst       :
  ((unit -> filecontent) * idfile -> fakecontent) ref

(* can use this hook to create/del some real data on disk *)
val hook_action_mkfile : ((objet * filename) -> unit) list ref
val hook_action_rm :     (objet -> unit) list ref

(* effect!: on w. Could do it other way cos less prop, but just to be
 * consistent with adv_itransducer *)
type itransducer     = fakecontent -> iproperty set

(* effect!: on w, cos of iprop (cant keep all the strings in mem, and
 * in descr when use disk) *)
type adv_itransducer = partcontent list -> iproperty set list

(* the plugin can be on a disk somewhere *)
val hook_find_alogic :
  (idfile -> logic) ref
val hook_find_transducer :
  (idfile -> property -> (property -> iproperty option) ->
    ((transducer, adv_itransducer) either)) ref


val hook_action_check_world : (world -> unit) list ref

(*---------------------------------------------------------------------------*)
(* was for Spec vs Impl LFS I think *)
(*---------------------------------------------------------------------------*)
val wrap_dir: property * int -> string

val wrap_logic: logic -> (property -> bool) -> logic
val wrap_transducer: (filecontent -> property set) -> transducer


(*****************************************************************************)
(* Interface to extend LFS *)
(*****************************************************************************)

(* interface to some of the core functions of LFS (used by extensions) *)
type context = {
  logic_cache : iproperty ograph;
  extensions : (iproperty, objet oset) oassoc;
  conv_iprop : (iproperty, property) oassoc;
  conv_prop : (property, iproperty) oassoc;
}
val context: world -> context

val ext: formula -> context -> objet oset
val dirs: formula -> misc_options -> context -> (property * int) oset

(* effect!: on w, cos of iprop (cant keep all the strings in mem,
 * and in descr when use disk) *)
val check_and_add_property: property -> iproperty option


(* This ref is set in path.ml to Path.parse_path. I use that trick cos
 * I dont want make lfs.ml to depend on parsepath just because of
 * relations. A cleaner soluce would be to handle relation also in
 * parsepath, with a formula_extended that include new constructors *)
val relation_parse_path : (string -> path) ref


(*---------------------------------------------------------------------------*)
(* the hooks *)
(*---------------------------------------------------------------------------*)

val hook_is_special_prop : (property -> (property -> bool) -> bool) ref

val hook_compute_ext :
  ((property * context) -> ((property * context) -> objet oset) ->
    objet oset) ref

val hook_action_add_prop : (property -> unit) list ref
val hook_action_add_file : (objet -> unit) list ref
val hook_action_del_file : (objet -> unit) list ref
val hook_action_change_file :  (objet -> unit) list ref
