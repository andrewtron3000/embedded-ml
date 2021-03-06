#ifndef __FFI_H__
#define __FFI_H__

#include "runtime-c.h"

/* unboxUnsigned : hptr -> int */
uint32_t unboxUnsigned( uint32_t *hptr );

/* boxUnsigned : contextlen * int -> hptr */
uint32_t *boxUnsigned( uint32_t context_len, uint32_t x );

/* unboxString : hptr -> char * */
void unboxString( uint32_t *hptr, char *buf, uint32_t buf_len, uint32_t *str_len);

/* boxString : contextlen * string -> hptr */
uint32_t *boxString( uint32_t context_len, char *str, int str_len );

/* unboxTuple : hptr * int -> hptr */
uint32_t *unboxTuple( uint32_t *hptr, uint32_t i );

/* boxTuple : contextlen * hptr * int -> hptr */ 
uint32_t *boxTuple( uint32_t context_len, uint32_t *tuple, int tuple_len );

#endif


