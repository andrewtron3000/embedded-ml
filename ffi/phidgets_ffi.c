#include <assert.h>
#include <inttypes.h>
#include <string.h>

#include "phidget21.h"
#include "ffi.h"

#define PHIDGETS_MAX_CHARS 20

uint32_t *phidgetsOpen( uint32_t context_len, uint32_t *stackvar )
{
  int rc;
  CPhidgetTextLCDHandle phid;
  int32_t serial_number;

  serial_number = (int32_t) unboxUnsigned( stackvar );

  rc = CPhidgetTextLCD_create( &phid ); 
  assert( rc == 0 );
  assert( (uint32_t) phid != 0 );

  rc = CPhidget_open( (CPhidgetHandle) phid, serial_number );
  assert( rc == 0 );

  rc = CPhidget_waitForAttachment( (CPhidgetHandle) phid, 0 );
  assert( rc == 0 );
 
  return boxUnsigned( context_len, (uint32_t) phid);
}

uint32_t *phidgetsClose( uint32_t context_len, uint32_t *stackvar )
{
  int rc;
  CPhidgetHandle phid;

  phid = (CPhidgetHandle) unboxUnsigned( stackvar );
  assert( phid != 0 );

  rc = CPhidget_close(phid);   
  assert( rc == 0 );

  rc = CPhidget_delete(phid);
  assert( rc == 0 );

  return (uint32_t *) NULL;
}

uint32_t *phidgetsSetDisplay( uint32_t context_len, uint32_t *tuple )
{
  int rc;
  char phidget_str[PHIDGETS_MAX_CHARS + 1];
  CPhidgetTextLCDHandle phid;
  int lineno;

  phid = (CPhidgetTextLCDHandle) unboxUnsigned( unboxTuple(tuple, 0) );
  assert( phid != 0 );

  lineno = (int) unboxUnsigned( unboxTuple(tuple, 1) );

  memset(phidget_str, 0x0, sizeof(phidget_str));

  unboxString( unboxTuple(tuple, 2), phidget_str, sizeof(phidget_str) );

  rc = CPhidgetTextLCD_setDisplayString(phid, lineno, phidget_str);
  assert( rc == 0 );

  return (uint32_t *) NULL;
}
