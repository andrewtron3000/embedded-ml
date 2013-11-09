
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
