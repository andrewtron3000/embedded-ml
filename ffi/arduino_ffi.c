#include <stdint.h>
#include <assert.h>

/*
 * Digital section.
 */

uint32_t *arduinoPinMode( uint32_t context_len, uint32_t *tuple )
{
  uint32_t pin, mode;

  pin = unboxUnsigned( unboxTuple(tuple, 0) );
  mode = unboxUnsigned( unboxTuple(tuple, 1) );

  pinMode(pin, mode);

  return (uint32_t *) NULL;
}

uint32_t *arduinoDigitalWrite( uint32_t context_len, uint32_t *tuple)
{
  uint32_t pin, value;

  pin = unboxUnsigned( unboxTuple(tuple, 0) );
  value = unboxUnsigned( unboxTuple(tuple, 1) );

  digitalWrite(pin, value);

  return (uint32_t *) NULL;
}

uint32_t *arduinoDigitalRead( uint32_t context_len, uint32_t *stackvar )
{
  uint32_t pin;
  uint32_t value;

  pin = unboxUnsigned( stackvar );

  value = digitalRead(pin);

  return boxUnsigned( context_len, value );
}

/*
 * Analog section
 */

uint32_t *arduinoAnalogRead( uint32_t context_len, uint32_t *stackvar )
{
  uint32_t pin;
  uint32_t value;

  pin = unboxUnsigned( stackvar );

  value = analogRead(pin);

  return boxUnsigned( context_len, value );
}

uint32_t *arduinoAnalogWrite( uint32_t context_len, uint32_t *tuple )
{
  uint32_t pin, value;

  pin = unboxUnsigned( unboxTuple(tuple, 0) );
  value = unboxUnsigned( unboxTuple(tuple, 1) );

  analogWrite(pin, value);

  return (uint32_t *) NULL;
}

/*
 * SPI Section
 */

uint32_t *arduinoSPIBeginTransaction( uint32_t context_len, uint32_t *tuple ) 
{
   uint32_t speed_maximum;
   uint32_t data_order;
   uint32_t data_mode;

   speed_maximum = unboxUnsigned( unboxTuple(tuple, 0) );
   data_order = unboxUnsigned( unboxTuple(tuple, 1) );
   data_mode = unboxUnsigned( unboxTuple(tuple, 2) );

   SPI.beginTransaction(SPISettings(speed_maximum, data_order, data_mode));

   return (uint32_t *) NULL;
}

uint32_t *arduinoSPIEndTransaction( uint32_t context_len, uint32_t *tuple ) 
{
   SPI.endTransaction();

   return (uint32_t *) NULL;
}

uint32_t *arduinoSPITransfer( uint32_t context_len, uint32_t *tuple )
{
   uint32_t value;
   uint32_t received_value;

   value = unboxUnsigned( unboxTuple(tuple, 0) );

   received_value = SPI.transfer(value);

   return boxUnsigned( context_len, value );   
}

uint32_t *arduinoSPITransfer16( uint32_t context_len, uint32_t *tuple )
{
   uint32_t value;
   uint32_t received_value;

   value = unboxUnsigned( unboxTuple(tuple, 0) );

   received_value = SPI.transfer16(value);

   return boxUnsigned( context_len, value );   
}

uint32_t *arduinoSPITransfer( uint32_t context_len, uint32_t *tuple )
{
   uint32_t i;
   uint32_t array_size;
   uint8_t transmit_buffer[SPI_BUFFER_SIZE];
   uint32_t *receive_words[SPI_BUFFER_SIZE];

   array_size = Arraylen(tuple);
   assert(array_size <= SPI_BUFFER_SIZE);

   /*
    * Work on preparing a transmit buffer.
    */
   for (i = 0; i < array_size; i++)
   {
      transmit_buffer[i] = Arrayval(tuple, i);
   }

   /*
    * Do the SPI transfer.
    */
   SPI.transfer(transmit_buffer, array_size);

   /*
    * Now work on preparing a received data tuple.
    */
   for (i = 0; i < array_size; i++)
   {
      receive_words[i] = buffer[i];
   }

   return boxTuple(context_len, receive_words, array_size);
}

/*
 * Wire/I2C Section
 */

uint32_t *arduinoWireBeginMaster( uint32_t context_len, uint32_t *stackvar ) 
{
   Wire.begin();

   return (uint32_t *) NULL;
}

uint32_t *arduinoWireBeginSlave( uint32_t context_len, uint32_t *stackvar ) 
{
   uint32_t address;

   address = unboxUnsigned( stackvar );

   Wire.begin(address);

   return (uint32_t *) NULL;
}

uint32_t *arduinoWireRequestFrom( uint32_t context_len, uint32_t *tuple ) 
{
   uint32_t address = unboxUnsigned( unboxTuple(tuple, 0) );
   uint32_t quantity = unboxUnsigned( unboxTuple(tuple, 1) );
   uint32_t stop = unboxUnsigned( unboxTuple(tuple, 2) );
   uint32_t bytes;

   if (stop > 0)
   {
      bytes = Wire.requestFrom(address, quantity, true);
   }  
   else
   {
      bytes = Wire.requestFrom(address, quantity, false);
   }

   return boxUnsigned( context_len, bytes );   
}

uint32_t *arduinoWireEndTransmission( uint32_t context_len, uint32_t *stackvar ) 
{
   uint32_t status;

   stop = unboxUnsigned( stackvar );

   if (stop > 0)
   {
      status = Wire.endTransmission(true);
   }
   else 
   {
      status = Wire.endTransmission(false);
   }

   return boxUnsigned( context_len, status );   
}

uint32_t *arduinoWireWriteValue( uint32_t context_len, uint32_t *stackvar ) 
{
   uint32_t written;
   uint32_t value;

   value = unboxUnsigned( stackvar );

   written = Wire.write(value)

   return boxUnsigned( context_len, written );   
}

uint32_t *arduinoWireWriteBuffer( uint32_t context_len, uint32_t *tuple )
{
   uint32_t i;
   uint32_t array_size;
   uint8_t transmit_buffer[WIRE_BUFFER_SIZE];
   uint32_t written;

   array_size = Arraylen(tuple);
   assert(array_size <= WIRE_BUFFER_SIZE);

   /*
    * Work on preparing a transmit buffer.
    */
   for (i = 0; i < array_size; i++)
   {
      transmit_buffer[i] = Arrayval(tuple, i);
   }

   /*
    * Do the Wire transfer.
    */
   written = Wire.write(transmit_buffer, array_size);

   return boxUnsigned( context_len, written );   
}

uint32_t *arduinoWireAvailable( uint32_t context_len, uint32_t *stackvar ) 
{
   uint32_t num_bytes_read;

   num_bytes_read = Wire.available();

   return boxUnsigned( context_len, num_bytes_read );   
}

uint32_t *arduinoWireReadBytes( uint32_t context_len, uint32_t *stackvar ) 
{
   uint32_t byte_read;

   byte_read = Wire.available();

   return boxUnsigned( context_len, byte_read );   
}

uint32_t *arduinoWireSetClock( uint32_t context_len, uint32_t *stackvar ) 
{
   uint32_t frequency;

   frequency = unboxUnsigned( stackvar );

   Wire.setClock(frequency)

   return (uint32_t *) NULL;
}

/*
 * Time Section
 */

uint32_t *arduinoMillis( uint32_t context_len, uint32_t *stackvar )
{
  uint32_t value;

  value = millis();

  return boxUnsigned( context_len, value );
}

uint32_t *arduinoMicros( uint32_t context_len, uint32_t *stackvar )
{
  uint32_t value;

  value = micros();

  return boxUnsigned( context_len, value );
}

uint32_t *arduinoDelay( uint32_t context_len, uint32_t *stackvar )
{
  uint32_t ms;

  ms = unboxUnsigned( stackvar );

  delay(ms);

  return (uint32_t *) NULL;
}

uint32_t *arduinoDelayMicroseconds( uint32_t context_len, uint32_t *stackvar )
{
  uint32_t us;

  us = unboxUnsigned( stackvar );

  delayMicroseconds(us);

  return (uint32_t *) NULL;
}
