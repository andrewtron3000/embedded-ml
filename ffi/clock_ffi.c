/*
 * clock_ffi.c
 *
 * Posix clock support
 *
 */

#include <stdio.h>
#include <time.h>
#include <inttypes.h>
#include <assert.h>

#include "ffi.h"

uint32_t *clockGetTime( uint32_t context_len, uint32_t *stackvar )
{
  struct timespec t;
  int rc;
  uint32_t tuple_vals[2];

  rc = clock_gettime(CLOCK_MONOTONIC, &t);
  assert(rc == 0);

  tuple_vals[0] = t.tv_sec;
  tuple_vals[1] = t.tv_nsec;

  return boxTuple(context_len, tuple_vals, 2); 
}
