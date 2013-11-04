/* Runtime system for the C backend of Humlock */

#ifndef ARDUINO_TARGET
#include "runtime-c.h"
#include "labels.h"
#endif

#undef PRINT_HEAPSIZE

uint32_t *temp;
uint32_t *stackframe[NUM_STACK_VARS];
uint32_t newtag;
uint32_t *exception_handler[1];

/* Now define the availc function */
#ifndef ARDUINO_TARGET
/* returns bytes available on stdin */
uint32_t availc()
{
  uint32_t bytes;
  ioctl(0, FIONREAD, &bytes);
  return bytes;
}
#else
/* returns bytes available on stdin */
uint32_t availc()
{
  return Serial.available();
}

uint32_t arduino_getc()
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

uint32_t arduino_putc(uint32_t c)
{
  Serial.write(c);

  /* send a carriage return after a linefeed */
  if (c == 10)
    {
      Serial.write(13);
    }
}
#endif

#ifndef ARDUINO_TARGET
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
#endif

int engine()
{
  void*(*f)();

#ifndef ARDUINO_TARGET
  setup_terminal();
#endif

  /*
   *  Now run the main program.
   */
  initializeHeap();
  f = _mainentry;
  while (f != 0)
    {
      f = (void* (*)()) f();
    }
  
#ifndef ARDUINO_TARGET
  restore_terminal();
#endif

  return 0;
}

void efficient_copy(void *d, void *s, uint32_t words)
{
  uint32_t i;

  for (i=0; i<words; i++)
  {
    *((uint32_t *)d + i) = *((uint32_t *)s + i);
  }
}

void efficient_set(void *d, uint32_t target, uint32_t words)
{
  uint32_t i;

  for (i=0; i<words; i++)
  {
    *((uint32_t *)d + i) = target;
  }
}

static uint32_t HEAPtotalsize = NUM_HEAP_WORDS;
static uint32_t HEAPbuffer[NUM_HEAP_WORDS * 2];
static uint32_t *HEAP;
static uint32_t HEAPactive;
static uint32_t HEAPinactive;
static uint32_t HEAPnextunused;
static uint32_t HEAPnextunusedinactive;
static uint32_t HEAPscavengeindex;

void initializeHeap ( void )
{
  HEAP = HEAPbuffer;
  HEAPnextunused = 0;
  HEAPnextunusedinactive = 0;
  HEAPscavengeindex = 0;
  HEAPactive = 0;
  HEAPinactive = HEAPtotalsize;
}

#define hWordsRemaining (HEAPtotalsize - HEAPnextunused)

void hSwitchHeaps ( void )
{
  if (HEAPactive == 0)
  {
    HEAPinactive = 0;
    HEAPactive = HEAPtotalsize;
  }
  else
  {
    HEAPactive = 0;
    HEAPinactive = HEAPtotalsize;
  }

  HEAPnextunused = HEAPnextunusedinactive;
  HEAPnextunusedinactive = 0;
  HEAPscavengeindex = 0;
}

uint32_t hExtractTag ( uint32_t *h )
{
  return (*h & HEAPtagmask);
}

void setForwardBit ( uint32_t *new_el, uint32_t *old )
{
  *old = *old | HEAPforwardbiton;
  *(old + 1) = (uint32_t) new_el;
}

uint32_t isForwardBitSet ( uint32_t tag )
{
  return (tag & HEAPforwardbiton);
}

uint32_t *hCopyToInactive ( uint32_t *old )
{
  uint32_t *target;
  uint32_t i, len;

  target = HEAP + HEAPinactive + HEAPnextunusedinactive;

  switch (hExtractTag(old))
  {
  case HEAPuntracedmask:
  case HEAPtaggedmask:
    *target = (*old & HEAPforwardbitoff);
    *(target + 1) = *(old + 1);
    HEAPnextunusedinactive += 2;
    break;
  case HEAPtracedmask:
    *target = *old & HEAPforwardbitoff;
    len = *old & HEAPmask;
    if (len > 0)
    {
      for (i = 0; i < len; i++ )
      {
        *(target + i + 1) = *(old + i + 1);
      }
    }
    else
    {
      *(target + 1) = 0;
    }
    HEAPnextunusedinactive += len + 1;
    if (len == 0)
    {
      HEAPnextunusedinactive += 1;
    }
    break;
  default:
    assert ( 1 == 0 );
    break;
  }

  return target;
}

uint32_t *hEvacuateNode ( uint32_t *old )
{
  uint32_t tag;
  uint32_t *new_el;

  tag = *old;
  if (isForwardBitSet(tag))
  {
    new_el = (uint32_t *) *(old + 1);
  }
  else
  {
    new_el = hCopyToInactive (old);
    setForwardBit (new_el, old);
  }

  return new_el;
}

void hScavenge ( void )
{
  uint32_t inactive_idx, scavenge_idx;
  uint32_t i, len, dx;
  uint32_t *scavenge_ptr;
  uint32_t *old_ptr;

  inactive_idx = HEAPinactive + HEAPnextunusedinactive;
  scavenge_idx = HEAPinactive + HEAPscavengeindex;

  while (inactive_idx > scavenge_idx)
  {
    scavenge_ptr = HEAP + scavenge_idx;
    switch (hExtractTag(scavenge_ptr))
    {
    case HEAPuntracedmask:
      dx = 2;
      break;
    case HEAPtaggedmask:
      if ((uint32_t *) *(scavenge_ptr + 1) > 0)
      {
        *(scavenge_ptr + 1) = (uint32_t) hEvacuateNode ( (uint32_t *) *(scavenge_ptr + 1) );
      }
      dx = 2;
      break;
    case HEAPtracedmask:
      len = *scavenge_ptr & HEAPmask;
      for (i = 0; i < len; i++)
      {
        *(scavenge_ptr + i + 1) = (uint32_t) hEvacuateNode ( (uint32_t*) *(scavenge_ptr + i + 1) );
      }
      dx = len + 1;
      if (len == 0)
      {
        dx += 1;
      }
      break;
    default:
      assert( 1 == 0 );
      break;
    }
    HEAPscavengeindex += dx;

    inactive_idx = HEAPinactive + HEAPnextunusedinactive;
    scavenge_idx = HEAPinactive + HEAPscavengeindex;
  }
}

void hTransferStackFrame ( uint32_t context_len )
{
  uint32_t i;
  uint32_t *heap_ptr;

  if (*exception_handler != 0)
  {
    *exception_handler = hEvacuateNode ( *exception_handler );
  }
  
  for (i = 0; i < context_len; i++)
  {
    heap_ptr = *(stackframe + i);
    if (heap_ptr != 0)
    {
      *(stackframe + i) = hEvacuateNode ( *(stackframe + i) );
    }
  }
}

void dumpHeapElement ( uint32_t *h )
{
  uint32_t tag;
  uint32_t i, len;

  tag = hExtractTag(h);

  fprintf(stderr, "nodeaddr = %p ", h);
  fprintf(stderr, "< hdr=%08x ", *h);
  switch(tag)
  {
  case HEAPuntracedmask:
    fprintf(stderr, "value=%08x ", *(h+1));
    break;
  case HEAPtaggedmask:
    fprintf(stderr, "tag=%08x ", *(h+1));
    break;
  case HEAPtracedmask:
    len = *h & HEAPmask;
    for (i = 0; i < len; i++)
    {
      fprintf(stderr, "addr=%08x ", *(h+1+i));
    }
    break;
  default:
    assert( 1 == 0 );
    break;
  }
  fprintf(stderr, ">\n");
}

void hScan ( void )
{
  uint32_t active_idx, scan_idx;
  uint32_t len, dx;
  uint32_t *scan_ptr;

  active_idx = HEAPactive + HEAPnextunused;
  scan_idx = HEAPactive;

  while (active_idx > scan_idx)
  {
    scan_ptr = HEAP + scan_idx;
    dumpHeapElement(scan_ptr);
    switch (hExtractTag(scan_ptr))
    {
    case HEAPuntracedmask:
    case HEAPtaggedmask:
      dx = 2;
      break;
    case HEAPtracedmask:
      len = *scan_ptr & HEAPmask;
      dx = len + 1;
      if (len == 0)
      {
        dx += 1;
      }
      break;
    default:
      assert( 1 == 0 );
      break;
    }
    scan_idx += dx;
  }
}

void hGarbageCollect ( uint32_t context_len )
{
  hTransferStackFrame ( context_len );
  hScavenge();
  hSwitchHeaps();
/*  hScan(); */
}

void checkHeapForSpace ( uint32_t context_len, uint32_t size_in_words )
{
  if (size_in_words > hWordsRemaining)
  {
    hGarbageCollect ( context_len );
#ifdef PRINT_HEAPSIZE
    printf("Heap Size = %d\n", HEAPnextunused * sizeof(uint32_t));
#endif
    if (size_in_words > hWordsRemaining)
    {
      assert( 1 == 0 );
    }
  }
}

uint32_t *hAlloc( uint32_t context_len, uint32_t size_in_words )
{
  uint32_t *new_ptr;

  checkHeapForSpace ( context_len, size_in_words );
  new_ptr = HEAP + HEAPactive + HEAPnextunused;
  HEAPnextunused += size_in_words;
  return new_ptr;
}

uint32_t *alloc_untraced(uint32_t value, uint32_t context_len)
{
  uint32_t *ptr;

  ptr = hAlloc(context_len, 2);

  *ptr = 0;
  *(ptr + 1) = value;

  return ptr;
}

uint32_t *alloc_traced_string(uint32_t traced_size_in_words, uint32_t context_len)
{
  uint32_t size;
  uint32_t *ptr;
  uint32_t i;
  uint32_t *first_char_ptr;

  size = traced_size_in_words;

  size += (size * 2) + 1;

  /* get one more if asked for a zero length array */
  if (traced_size_in_words == 0)
    size = size + 1;

  ptr = hAlloc(context_len, size);

  /* fill in header word */
  *ptr = ( (traced_size_in_words & HEAPmask) |
           HEAPtracedmask );

  if (traced_size_in_words > 0)
  {
    /* pointer to the first character */
    first_char_ptr = ptr + traced_size_in_words + 1;

    for (i = 0; i < traced_size_in_words; i++)
    {
      /* first set the character's untraced heap node header */
      *(first_char_ptr + (i * 2)) = 0;

      /* then assign the character's heap node into the string heap node */
      *(ptr + i + 1) = (uint32_t) (first_char_ptr + (i * 2));
    }
  }
  else
  {
    /* set next word to zero.  this is important for 0 length arrays */
    *(ptr + 1) = 0;
  }

  return ptr;
}

uint32_t *alloc_traced_array(uint32_t traced_size_in_words, uint32_t context_len)
{
  uint32_t size;
  uint32_t *ptr;

  /* one extra word for the header */
  if (traced_size_in_words > 0)
    size = traced_size_in_words + 1;
  else
    size = traced_size_in_words + 2;

  ptr = hAlloc(context_len, size);

  /* fill in header word */
  *ptr = ( (traced_size_in_words & HEAPmask) |
           HEAPtracedmask );
  
  /* set next word to zero.  this is important for 0 length arrays */
  *(ptr + 1) = 0;

  return ptr;
}

uint32_t *alloc_tagged(uint32_t tag, uint32_t context_len)
{
  uint32_t *ptr;

  ptr = hAlloc(context_len, 2);

  *ptr = (tag & HEAPmask) | HEAPtaggedmask;
  
  return ptr;
}

/* End of runtime system */

