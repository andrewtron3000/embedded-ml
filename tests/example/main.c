#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>
#include <termios.h>
#include <sys/ioctl.h>

#include "runtime-c.h"
#include "sandmark.h"

uint32_t my_availc() {
  uint32_t bytes;
  ioctl(0, FIONREAD, &bytes);
  return bytes;
}

uint32_t my_getc() {
  return getc(stdin);
}

uint32_t my_putc(uint32_t x) {
  return putc(x, stdout);
}

void heap_error(Heap_error_t e) {
  printf("Got heap error %d\n", e);
}

struct termios old_stdin_tio, new_stdin_tio;
struct termios old_stdout_tio, new_stdout_tio;
int setup_terminal()
{
  /*
   *  First change the buffering scheme related to stdio.
   */
  setvbuf(stdin, NULL, _IONBF, 0);
  setvbuf(stdout, NULL, _IONBF, 0);

  /*
   *  Next change the terminal driver buffering scheme.
   */
  /* get the terminal settings for stdin and stdout */
  tcgetattr(0, &old_stdin_tio);
  tcgetattr(1, &old_stdout_tio);

  /* we want to keep the old setting to restore them at the end */
  new_stdin_tio=old_stdin_tio;
  new_stdout_tio=old_stdout_tio;

  /* disable canonical mode (buffered i/o) and local echo */
  new_stdin_tio.c_lflag &=(~ICANON);
  new_stdout_tio.c_lflag &=(~ICANON);

  /* set the new settings immediately */
  tcsetattr(0, TCSANOW, &new_stdin_tio);
  tcsetattr(1, TCSANOW, &new_stdout_tio);

  return 0;
}

int restore_terminal()
{
  /* restore the former terminal settings */
  tcsetattr(0, TCSANOW, &old_stdin_tio);
  tcsetattr(1, TCSANOW, &old_stdout_tio);

  return 0;
}

int main(char **argv, int argc) {
  void*(*f)();
  IO_functions_type io;

  io.availc = my_availc;
  io.getc = my_getc;
  io.putc = my_putc;
  
  setup_terminal();

  /*
   *  Now run the main program.
   */
  initializeIO(&io);
  initializeHeap(heap_error);
  f = _mainentry;
  while (f != 0)
    {
      f = (void* (*)()) f();
    }
  
  restore_terminal();

  return 0;
 }

