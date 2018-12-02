
#ifndef __arduino_ffi_h
#define __arduino_ffi_h

#include <stdint.h>

#define SPI_BUFFER_SIZE (256)
#define WIRE_BUFFER_SIZE (256)

uint32_t *arduinoPinMode( uint32_t context_len, uint32_t *tuple );
uint32_t *arduinoDigitalWrite( uint32_t context_len, uint32_t *tuple);
uint32_t *arduinoDigitalRead( uint32_t context_len, uint32_t *stackvar );

uint32_t *arduinoAnalogWrite( uint32_t context_len, uint32_t *tuple);
uint32_t *arduinoAnalogRead( uint32_t context_len, uint32_t *stackvar );

uint32_t *arduinoSPIBeginTransaction( uint32_t context_len, uint32_t *tuple ); 
uint32_t *arduinoSPIEndTransaction( uint32_t context_len, uint32_t *tuple );
uint32_t *arduinoSPITransfer( uint32_t context_len, uint32_t *tuple );
uint32_t *arduinoSPITransfer16( uint32_t context_len, uint32_t *tuple );
uint32_t *arduinoSPITransfer( uint32_t context_len, uint32_t *tuple );

uint32_t *arduinoWireBeginMaster( uint32_t context_len, uint32_t *stackvar ); 
uint32_t *arduinoWireBeginSlave( uint32_t context_len, uint32_t *stackvar ); 
uint32_t *arduinoWireRequestFrom( uint32_t context_len, uint32_t *tuple ); 
uint32_t *arduinoWireEndTransmission( uint32_t context_len, uint32_t *stackvar ); 
uint32_t *arduinoWireWriteValue( uint32_t context_len, uint32_t *stackvar ); 
uint32_t *arduinoWireWriteBuffer( uint32_t context_len, uint32_t *tuple );
uint32_t *arduinoWireAvailable( uint32_t context_len, uint32_t *stackvar ); 
uint32_t *arduinoWireReadBytes( uint32_t context_len, uint32_t *stackvar ); 
uint32_t *arduinoWireSetClock( uint32_t context_len, uint32_t *stackvar ); 

uint32_t *arduinoMillis( uint32_t context_len, uint32_t *stackvar );
uint32_t *arduinoMicros( uint32_t context_len, uint32_t *stackvar );
uint32_t *arduinoDelay( uint32_t context_len, uint32_t *stackvar );
uint32_t *arduinoDelayMicroseconds( uint32_t context_len, uint32_t *stackvar );

#endif
