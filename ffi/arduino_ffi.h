
#ifndef __arduino_ffi_h
#define __arduino_ffi_h

uint32_t *arduinoPinMode( uint32_t context_len, uint32_t *tuple );
uint32_t *arduinoDigitalWrite( uint32_t context_len, uint32_t *tuple);
uint32_t *arduinoDigitalRead( uint32_t context_len, uint32_t *stackvar );
uint32_t *arduinoMillis( uint32_t context_len, uint32_t *stackvar );
uint32_t *arduinoMicros( uint32_t context_len, uint32_t *stackvar );
uint32_t *arduinoDelay( uint32_t context_len, uint32_t *stackvar );
uint32_t *arduinoDelayMicroseconds( uint32_t context_len, uint32_t *stackvar );

#endif
