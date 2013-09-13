(***********************************************************************)
(*                                                                     *)
(*            The "agrep" library for Objective Caml                   *)
(*                                                                     *)
(*         Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id: agrep.mli,v 1.2 2002/02/02 09:29:01 xleroy Exp $ *)

(** String searching with errors *)

type pattern
       (** The type of compiled search patterns *)

val pattern: ?transl:string -> string -> pattern
       (** Compile a search pattern.  The syntax for patterns is
           similar to that of the Unix shell.  The following constructs
           are recognized:
         - [?    ]  match any single character
         - [*    ]  match any sequence of characters
         - [[..] ]  character set: ranges are denoted with [-], as in [[a-z]];
                    an initial [^], as in [[^0-9]], complements the set
         - [&    ]  conjunction (e.g. [sweet&sour])
         - [|    ]  alternative (e.g. [high|low])
         - [(..) ]  grouping
         - [\    ]  escape special characters; the special characters
                    are [\?*[]&|()].

           The optional argument [transl] is a character translation table.
           This is a string [s] of length 256 that ``translates'' a
           character [c] to the character [s.(Char.code c)].  A character
           of the text matches a character of the pattern if they both
           translate to the same character according to [transl].
           If [transl] is not provided, the identity translation
           (two characters match iff they are equal) is assumed.
           Useful predefined translation tables are provided in
           {!Agrep.Iso8859_15}.
        *)

exception Syntax_error of int
       (** Exception thrown by {!Agrep.pattern} when the given pattern
           is syntactically incorrect.  The integer argument is the
           character number where the syntax error occurs. *)

val pattern_string: ?transl:string -> string -> pattern
       (** [Agrep.pattern_string s] returns a pattern that matches exactly
           the string [s] and nothing else.  The optional parameter
           [transl] is as in {!Agrep.pattern}. *)

val string_match:
  pattern -> ?numerrs:int -> ?wholeword: bool -> string -> bool
       (** [string_match pat text] tests whether the string [text]
           matches the compiled pattern [pat].  The optional parameter
           [numerrs] is the number of errors permitted.  One error
           corresponds to a substitution, an insertion or a deletion
           of a character.  [numerrs] default to 0 (exact match).
           The optional parameter [wholeword] is [true] if the pattern must
           match a whole word, [false] if it can match inside a word.
           [wholeword] defaults to [false] (match inside words). *)

val substring_match:
  pattern -> ?numerrs:int -> ?wholeword: bool ->
  string -> pos:int -> len:int -> bool
       (** Same as {!Agrep.string_match}, but restrict the match to the
           substring of the given string starting at character number
           [pos] and extending [len] characters. *)

val errors_substring_match:
  pattern -> ?numerrs:int -> ?wholeword: bool ->
  string -> pos:int -> len:int -> int
       (** Same as {!Agrep.substring_match}, but return the smallest number
           of errors such that the substring matches the pattern.
           That is, it returns [0] if the substring matches exactly,
           [1] if the substring matches with one error, etc.
           Return [max_int] if the substring does not match the pattern
           with at most [numerrs] errors. *)

module Iso8859_15: sig
  val case_insensitive: string
      (** Translation table identifying uppercase and lowercase letters. *)
  val accent_insensitive: string
      (** Translation table identifying accented letters with the corresponding
          non-accented letters, while still preserving case. *)
  val case_and_accent_insensitive: string
      (** Translation table identifying accented letters with the corresponding
          non-accented letters, and uppercase and lowercase letters. *)
end
  (** Useful translation tables for the ISO 8859-15 (Latin-1 with Euro)
      character set. *)
