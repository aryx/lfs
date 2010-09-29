/***********************************************************************/
/*                                                                     */
/*            The "agrep" library for Objective Caml                   */
/*                                                                     */
/*         Xavier Leroy, projet Cristal, INRIA Rocquencourt            */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id: skeleton.h,v 1.2 2002/02/02 09:29:02 xleroy Exp $ */

static value FUNCTION_NAME(ulong * table, uint pattern_length,
                           uchar * text, mlsize_t length)
{
  ulong R0;
#if NERRS >= 1
  ulong R1, R0before;
#endif
#if NERRS >= 2
  ulong R2, R1before;
#endif
#if NERRS == 3
  ulong R3, R2before;
#endif
  ulong Found, Ssharp;
#ifdef WHOLE_WORD
  ulong word_boundary;
#endif

  Ssharp = table[256];
  Found = 1UL << pattern_length;
  R0 = 1;
#if NERRS >= 1
  R1 = 3;
#endif
#if NERRS >= 2
  R2 = 7;
#endif
#if NERRS == 3
  R3 = 0xF;
#endif
  for (/*nothing*/; length > 0; length--, text++) {
    ulong S = table[*text];
#if NERRS >= 1
    R0before = R0;
#endif
#if NERRS >= 2
    R1before = R1;
#endif
#if NERRS == 3
    R2before = R2;
#endif
#ifdef WHOLE_WORD
    word_boundary = word_constituent[text[0]] ^ word_constituent[text[1]];
#  define MATCH_EMPTY word_boundary
#else
#  define MATCH_EMPTY 1
#endif
    R0 = ((R0 & S) << 1) | (R0 & Ssharp) | MATCH_EMPTY;
#if NERRS >= 1
    R1 = (((R1 & S) | R0before | R0) << 1) 
         | R0before
         | (R1 & Ssharp)
         | MATCH_EMPTY;
#endif
#if NERRS >= 2
    R2 = (((R2 & S) | R1before | R1) << 1) 
         | R1before
         | (R2 & Ssharp)
         | MATCH_EMPTY;
#endif
#if NERRS == 3
    R3 = (((R3 & S) | R2before | R2) << 1) 
         | R2before
         | (R3 & Ssharp)
         | MATCH_EMPTY;
#endif
    if ((R0 & Found) && MATCH_EMPTY) return Val_int(0);
#if NERRS >= 1
    if ((R1 & Found) && MATCH_EMPTY) return Val_int(1);
#endif
#if NERRS >= 2
    if ((R2 & Found) && MATCH_EMPTY) return Val_int(2);
#endif
#if NERRS == 3
    if ((R3 & Found) && MATCH_EMPTY) return Val_int(3);
#endif
  }
  return Val_long(CAML_MAX_INT);
}

#undef MATCH_EMPTY
