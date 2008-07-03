#include <assert.h>
#include <inttypes.h>
#include <string.h>

#include "phidget21.h"
#include "ffi.h"

#define PHIDGETS_MAX_CHARS 20
static char phidget_str[PHIDGETS_MAX_CHARS + 1];

uint32_t *phidgetsLCDOpen( uint32_t context_len, uint32_t *stackvar )
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

uint32_t *phidgetsIKOpen( uint32_t context_len, uint32_t *stackvar )
{
  int rc;
  CPhidgetInterfaceKitHandle phid;
  int32_t serial_number;

  serial_number = (int32_t) unboxUnsigned( stackvar );

  rc = CPhidgetInterfaceKit_create( &phid ); 
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
  CPhidgetTextLCDHandle phid;
  int lineno;

  phid = (CPhidgetTextLCDHandle) unboxUnsigned( unboxTuple(tuple, 0) );
  assert( phid != 0 );

  lineno = (int) unboxUnsigned( unboxTuple(tuple, 1) );
  assert( (lineno >= 0) && (lineno < 2) ); 

  unboxString( unboxTuple(tuple, 2), phidget_str, sizeof(phidget_str) );

  rc = CPhidgetTextLCD_setDisplayString(phid, lineno, phidget_str);
  assert( rc == 0 );

  return (uint32_t *) NULL;
}

uint32_t *phidgetsGetSensorRawValue( uint32_t context_len, uint32_t *tuple )
{
  int rc;
  int index;
  int raw_value;
  CPhidgetInterfaceKitHandle phid;

  phid = (CPhidgetInterfaceKitHandle) unboxUnsigned( unboxTuple(tuple, 0) );
  assert( phid != 0 );

  index = (int) unboxUnsigned( unboxTuple(tuple, 1) );
  assert( (index >= 0) && (index < 8) );

  rc = CPhidgetInterfaceKit_getSensorRawValue( phid, index, &raw_value );
  assert( rc == 0 );

  return boxUnsigned( context_len, (uint32_t) raw_value );
}

uint32_t *phidgetsGetOutputState( uint32_t context_len, uint32_t *tuple )
{
  int rc;
  int index;
  int output_state;
  CPhidgetInterfaceKitHandle phid;

  phid = (CPhidgetInterfaceKitHandle) unboxUnsigned( unboxTuple(tuple, 0) );
  assert( phid != 0 );

  index = (int) unboxUnsigned( unboxTuple(tuple, 1) );
  assert( (index >= 0) && (index < 8) );

  rc = CPhidgetInterfaceKit_getOutputState( phid, index, &output_state );
  assert( rc == 0 );

  return boxUnsigned( context_len, (uint32_t) output_state );
}

uint32_t *phidgetsSetOutputState( uint32_t context_len, uint32_t *tuple )
{
  int rc;
  int index;
  int desired_value;
  CPhidgetInterfaceKitHandle phid;

  phid = (CPhidgetInterfaceKitHandle) unboxUnsigned( unboxTuple(tuple, 0) );
  assert( phid != 0 );

  index = (int) unboxUnsigned( unboxTuple(tuple, 1) );
  assert( (index >= 0) && (index < 8) );

  desired_value = (int) unboxUnsigned( unboxTuple(tuple, 2) );
  assert( (desired_value == 0) || (desired_value == 1) );

  rc = CPhidgetInterfaceKit_setOutputState( phid, index, desired_value );
  assert( rc == 0 );

  return (uint32_t *) NULL;
}

