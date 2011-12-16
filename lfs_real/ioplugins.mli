open Common

(* Transform a file on disk into an OCaml LFS plugin. It is a kind of
 * 'eval' as in scheme. It just create pipes with the (executable)
 * file, and adapt an untyped stream based communication into a
 * function that looks clean and well typed.
 *
 * For the transducers in addition to the file on disk, they also take
 * auxillary functions passed by LFS that enable the uninteract functions
 * to not depend on LFS internals:
 * - for the advanced transducer it's the
 *   conversion function to iproperty that modifies internally LFS world.
 * - for the transducer it's the id_to_filename function.
 *)

val uninteract_logic:
  filename -> Lfs.logic

val uninteract_adv_transducer:
  filename -> (Lfs.property -> Lfs.iproperty option) -> Lfs.adv_itransducer
val uninteract_transducer:
  filename -> (Lfs.idfile -> filename) -> Lfs.transducer
