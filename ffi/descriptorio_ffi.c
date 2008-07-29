#include <stdio.h>                /* perror() */
#include <stdlib.h>               /* atoi() */
#include <unistd.h>               /* read() */
#include <assert.h>
#include <inttypes.h>

#include "ffi.h"

#define D_BUFFER_SIZE 65536
static uint8_t d_buffer[D_BUFFER_SIZE + 1];

uint32_t *descriptorRead( uint32_t context_len, uint32_t *tuple )
{
  int d;
  int bytes_to_read;
  int bytes_read;
  int i;

  d = (int) unboxUnsigned( unboxTuple(tuple, 0) );
  assert( d > 0 );

  bytes_to_read = (int) unboxUnsigned( unboxTuple(tuple, 1) );
  assert( bytes_to_read > 0 && bytes_to_read < D_BUFFER_SIZE );

  bytes_read = 0;
  while (bytes_to_read > 0)
  {
    i = read(d, (void *) &(d_buffer[bytes_read]), bytes_to_read);
    bytes_to_read -= i;
    bytes_read += i;
  }

  /* null terminate the result */
  d_buffer[bytes_read] = (uint8_t) 0;

  return boxString( context_len, 
                    (char *) d_buffer, 
                    bytes_read );
}

uint32_t *descriptorWrite( uint32_t context_len, uint32_t *tuple )
{
  int d;
  int bytes_written;
  int bytes_to_write;
  int i;

  d = (int) unboxUnsigned( unboxTuple(tuple, 0) );
  assert( d != 0 );

  unboxString( unboxTuple(tuple, 1), (char *) d_buffer, sizeof(d_buffer) );

  bytes_to_write = strnlen((char *) d_buffer, sizeof(d_buffer));

  bytes_written = 0;
  while(bytes_to_write > 0)
  {
    i = write(d, (void *) &(d_buffer[bytes_written]), bytes_to_write);
    bytes_to_write -= i;
    bytes_written += i;
  }

  return (uint32_t *) NULL;
}
