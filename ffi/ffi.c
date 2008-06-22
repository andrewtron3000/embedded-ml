#include <assert.h>
#include <inttypes.h>
#include <string.h>

#include "ffi.h"
#include "runtime-c.h"

#define MIN(a, b) ((a) < (b) ? (a) : (b))

/* unboxUnsigned : hptr -> int */
uint32_t unboxUnsigned( uint32_t *hptr )
{
  /*
   * make sure we're dealing with an untraced heap element.  (just an
   * integer)
   */
  assert ( hExtractTag(hptr) == HEAPuntracedmask );

  return Intval(hptr);
}

/* boxUnsigned : contextlen * int -> hptr */
uint32_t *boxUnsigned( uint32_t context_len, uint32_t x )
{
  return alloc_untraced(x, context_len);
}

/* unboxString : hptr -> char * */
void unboxString( uint32_t *hptr, char *buf, uint32_t buf_len )
{
  uint32_t string_len;
  uint32_t i;
  uint32_t *boxed_char;

  /*
   * make sure we're dealing with a traced string element.
   */
  assert ( hExtractTag(hptr) == HEAPtracedmask );

  string_len = Arraylen(hptr);

  for(i = 0; i < MIN(string_len, buf_len); i++)
  {
    boxed_char = Arrayval(hptr, i);
    buf[i] = (char) Intval(boxed_char);
  }
}

/* boxString : contextlen * string -> hptr */
uint32_t *boxString( uint32_t context_len, char *str )
{
  uint32_t str_len;
  uint32_t *hptr;
  uint32_t *first_char_ptr;
  int i;

  str_len = strlen(str);

  hptr = alloc_traced_string(str_len, context_len);

  if (str_len > 0)
  {
    /* pointer to the first character */
    first_char_ptr = hptr + str_len + 1;

    for (i = 0; i < str_len; i++)
    {
      /* first set the character's untraced heap node header */
      *(first_char_ptr + (i * 2)) = (uint32_t) (str[i]);

      /* then assign the character's heap node into the string heap node */
      *(hptr + i + 1) = (uint32_t) (first_char_ptr + (i * 2));
    }
  }

  return hptr;
}


/* unboxTuple : hptr * int -> hptr */
uint32_t *unboxTuple( uint32_t *hptr, uint32_t i )
{
  /*
   * make sure we're dealing with a traced string element.
   */
  assert ( hExtractTag(hptr) == HEAPtaggedmask );

  return Tupleval(hptr, i);
}

/* boxTuple : contextlen * hptr* -> hptr */ 


