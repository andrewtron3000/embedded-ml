#include "runtime-c.h"
#include "sandmark.h"

uint32_t my_availc()
{
  return Serial.available();
}

uint32_t my_getc()
{
  static uint32_t just_returned_cr = 0;
  uint32_t c;

  /* Wait for data */
  while (Serial.available() < 1)
    {
      ;
    }

  /*
   * When we see a carriage return, send a linefeed too 
   */
  if (just_returned_cr == 1)
    {
      c = Serial.read();
      c = 10;
      just_returned_cr = 0;
    }
  else
    {
      c = Serial.peek();
      if (c == 13)
	{
	  just_returned_cr = 1;
	}
      else
	{
	  c = Serial.read();
	}
    }

  Serial.write(c); /* local echo */
  return c;
}

uint32_t my_putc(uint32_t c)
{
  Serial.write(c);

  /* send a carriage return after a linefeed */
  if (c == 10)
    {
      Serial.write(13);
    }

  return 0;
}

void heap_error(Heap_error_t e) {
  digitalWrite(13, HIGH);
  Serial.write("Got heap error");
  Serial.write(e);
  Serial.write("\n");
}

void*(*next_f)();


void setup() {
  IO_functions_type io;

  io.availc = my_availc;
  io.getc = my_getc;
  io.putc = my_putc;

  initializeIO(&io);
  initializeHeap(heap_error);

  next_f = _mainentry;

  pinMode(13, OUTPUT);
  digitalWrite(13, LOW);
}

void loop() {
  next_f = (void* (*)()) next_f();
}
