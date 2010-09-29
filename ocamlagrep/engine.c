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

/* $Id: engine.c,v 1.2 2002/02/02 09:29:02 xleroy Exp $ */

#include <stdlib.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

typedef unsigned char uchar;
typedef unsigned int uint;
typedef unsigned long ulong;

#define BITS_PER_WORD (8 * sizeof(ulong))

#define Setbit(ptr,nbit) \
  ((ptr)[(nbit) / BITS_PER_WORD] |= (1UL << ((nbit) % BITS_PER_WORD)))

#define CAML_MAX_INT ((1L << (8 * sizeof(value) - 2)) - 1)

CAMLprim value caml_agrep_new_bitmatrix(value v_patlen, value v_nentries)
{
  ulong nwords = (Long_val(v_patlen) + BITS_PER_WORD - 1) / BITS_PER_WORD;
  ulong size = nwords * Long_val(v_nentries);
  value res = alloc(size, Abstract_tag);
  memset((ulong *) res, 0, size * sizeof(ulong));
  return res;
}

CAMLprim value caml_agrep_set_bit(value v_matrix, value v_patlen,
                                  value v_index, value v_bitnum)
{
  ulong nwords = (Long_val(v_patlen) + BITS_PER_WORD - 1) / BITS_PER_WORD;
  ulong bitnum = Long_val(v_bitnum);
  Setbit((ulong *) v_matrix + nwords * Long_val(v_index), bitnum);
  return Val_unit;
}

unsigned char word_constituent[256] = {
  /* 0 - 31 */
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  /*   ! " # $ % & ' ( ) * + , - . / 0 1 2 3 4 5 6 7 8 9 : ; < = > ? */
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
  /* @ A B C D E F G H I J K L M N O P Q R S T U V W X Y Z [ \ ] ^ _ */
     0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
  /* ` a b c d e f g h i j k l m n o p q r s t u v w x y z { | } ~ \127 */
     0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
  /* 128-159 */
     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  /*   ¡ ¢ £ ¤ ¥ ¦ § ¨ © ª « ¬ ­ ® ¯ ° ± ² ³ ´ µ ¶ · ¸ ¹ º » ¼ ½ ¾ ¿ */
     0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,1,1,0,
  /* À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß */
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,
  /* à á â ã ä å æ ç è é ê ë ì í î ï ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ */
     1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1
};

/* Specialized versions of string matching code */

#undef WHOLE_WORD

#define FUNCTION_NAME match_0errs
#define NERRS 0
#include "skeleton.h"
#undef FUNCTION_NAME
#undef NERRS

#define FUNCTION_NAME match_1errs
#define NERRS 1
#include "skeleton.h"
#undef FUNCTION_NAME
#undef NERRS

#define FUNCTION_NAME match_2errs
#define NERRS 2
#include "skeleton.h"
#undef FUNCTION_NAME
#undef NERRS

#define FUNCTION_NAME match_3errs
#define NERRS 3
#include "skeleton.h"
#undef FUNCTION_NAME
#undef NERRS

#define WHOLE_WORD

#define FUNCTION_NAME match_word_0errs
#define NERRS 0
#include "skeleton.h"
#undef FUNCTION_NAME
#undef NERRS

#define FUNCTION_NAME match_word_1errs
#define NERRS 1
#include "skeleton.h"
#undef FUNCTION_NAME
#undef NERRS

#define FUNCTION_NAME match_word_2errs
#define NERRS 2
#include "skeleton.h"
#undef FUNCTION_NAME
#undef NERRS

#define FUNCTION_NAME match_word_3errs
#define NERRS 3
#include "skeleton.h"
#undef FUNCTION_NAME
#undef NERRS

/* General code: arbitrary errors, arbitrary pattern length */

static value match_general(ulong * table, ulong m,
                           ulong nerrs, int wholeword,
                           uchar * text, mlsize_t length)
{
  ulong nwords, n, j;
  ulong ** R;
  ulong * Rpbefore;
  ulong Found_offset, Found_mask;
  ulong * Ssharp;
  ulong * Rc, * Rp;
  ulong carry;
  ulong match_empty;
  long retcode;

  nwords = (m + BITS_PER_WORD - 1) / BITS_PER_WORD;
  R = stat_alloc((nerrs + 1) * sizeof(ulong *));
  for (n = 0; n <= nerrs; n++) R[n] = stat_alloc(nwords * sizeof(ulong));
  Rpbefore = stat_alloc(nwords * sizeof(ulong));
  /* Initialize Found */
  Found_offset = m / BITS_PER_WORD;
  Found_mask = 1UL << (m % BITS_PER_WORD);
  /* Initialize R */
  for (n = 0; n <= nerrs; n++) {
    memset(R[n], 0, nwords * sizeof(ulong));
    for (j = 0; j <= n; j++) Setbit(R[n], j);
  }
  /* Initialize Ssharp & match_empty */
  Ssharp = table + 256 * nwords;
  match_empty = 1;
  /* Main loop */
  for (/*nothing*/; length > 0; length--, text++) {
    ulong * S = table + (*text) * nwords;
    if (wholeword)
      match_empty = word_constituent[text[0]] ^ word_constituent[text[1]];
    /* Special case for 0 errors */
    Rc = R[0];
    carry = match_empty;
    for (j = 0; j < nwords; j++) {
      ulong Rcbefore = Rc[j];
      ulong toshift = Rcbefore & S[j];
      Rc[j] = (toshift << 1) | (Rcbefore & Ssharp[j]) | carry;
      carry = toshift >> (BITS_PER_WORD - 1);
      Rpbefore[j] = Rcbefore;
    }
    if (Rc[Found_offset] & Found_mask && match_empty)
      { retcode = 0; goto exit; }
    /* General case for > 0 errors */
    for (n = 1; n <= nerrs; n++) {
      Rp = Rc;
      Rc = R[n];
      carry = match_empty;
      for (j = 0; j < nwords; j++) {
        ulong Rcbefore = Rc[j];
        ulong toshift = (Rcbefore & S[j]) | Rpbefore[j] | Rp[j];
        Rc[j] = (toshift << 1)
              | Rpbefore[j]
              | (Rcbefore & Ssharp[j])
              | carry;
        carry = toshift >> (BITS_PER_WORD - 1);
        Rpbefore[j] = Rcbefore;
      }
      if (Rc[Found_offset] & Found_mask && match_empty)
        { retcode = n; goto exit; }
    }
  }
  /* Not found */
  retcode = CAML_MAX_INT;
  /* Cleanup */
 exit:
  for (n = 0; n <= nerrs; n++) free(R[n]);
  free(R);
  free(Rpbefore);
  return Val_long(retcode);
}

/* Entry point */

CAMLprim value caml_agrep_match(value v_text, value v_ofs, value v_len,
                                value v_patlen, value v_table,
                                value v_nerrs, value v_wholeword)
{
  uchar * text = &Byte_u(v_text, Long_val(v_ofs));
  mlsize_t len = Long_val(v_len);
  ulong patlen = Long_val(v_patlen);

  if (patlen < BITS_PER_WORD) {
    switch (((Long_val(v_nerrs)) << 1) | Int_val(v_wholeword)) {
    case 2*0+0: return match_0errs((ulong *) v_table, patlen, text, len);
    case 2*0+1: return match_word_0errs((ulong *) v_table, patlen, text, len);
    case 2*1+0: return match_1errs((ulong *) v_table, patlen, text, len);
    case 2*1+1: return match_word_1errs((ulong *) v_table, patlen, text, len);
    case 2*2+0: return match_2errs((ulong *) v_table, patlen, text, len);
    case 2*2+1: return match_word_2errs((ulong *) v_table, patlen, text, len);
    case 2*3+0: return match_3errs((ulong *) v_table, patlen, text, len);
    case 2*3+1: return match_word_3errs((ulong *) v_table, patlen, text, len);
    }
  }
  return match_general((ulong *) v_table, patlen,
                       Long_val(v_nerrs), Int_val(v_wholeword),
                       text, len);
}

CAMLprim value caml_agrep_match_bytecode(value * argv, int argn)
{
  return caml_agrep_match(argv[0], argv[1], argv[2], argv[3], argv[4],
                          argv[5], argv[6]);
}
