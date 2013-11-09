#ifndef ARDUINO_TARGET
#include <assert.h>
#include <inttypes.h>
#include <string.h>

#include "ffi.h"
#include "runtime-c.h"
#endif

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
void unboxString( uint32_t *hptr, char *buf, uint32_t buf_len, uint32_t *str_len )
{
  uint32_t i;
  uint32_t string_len;
  uint32_t *boxed_char;

  /*
   * make sure we're dealing with a traced string element.
   */
  assert ( hExtractTag(hptr) == HEAPtracedmask );

  string_len = Arraylen(hptr);

  assert ( string_len <= (buf_len - 1) );

  for(i = 0; i < string_len; i++)
  {
    boxed_char = Arrayval(hptr, i);
    buf[i] = (char) Intval(boxed_char);
  }

  *str_len = string_len;
}

/* boxString : contextlen * string -> hptr */
uint32_t *boxString( uint32_t context_len, char *str, int str_len )
{
  uint32_t *hptr;
  uint32_t *first_char_ptr;
  int i;

  hptr = alloc_traced_string(str_len, context_len);

  if (str_len > 0)
  {
    /* pointer to the first character */
    first_char_ptr = hptr + str_len + 1;

    for (i = 0; i < str_len; i++)
    {
      /* assign each character into the new heap structure */
      *(first_char_ptr + (i * 2) + 1) = (uint32_t) (str[i]);
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
  assert ( hExtractTag(hptr) == HEAPtracedmask );

  return Tupleval(hptr, i);
}

/* boxTuple : contextlen * int array * tuple_len -> hptr */ 
/* WARNING: the tuple pointer below CANNOT be pointers into the heap!
   if so, these could be invalidated after the alloc_traced_string
   call within this function. */
uint32_t *boxTuple( uint32_t context_len, uint32_t *tuple, int tuple_len )
{
  uint32_t *hptr;
  uint32_t *first_ptr;
  int i;

  /* 
   * SUSP -- assuming tuple has the same heap structure as a string.
   */
  hptr = alloc_traced_string(tuple_len, context_len);

  if (tuple_len > 0)
  {
    /* pointer to the first tuple element */
    first_ptr = hptr + tuple_len + 1;

    for (i = 0; i < tuple_len; i++)
    {
      /* assign each tuple value into the new heap structure */
      *(first_ptr + (i * 2) + 1) = tuple[i];
    }
  }

  return hptr;
}
