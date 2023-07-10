/* Runtime system for the C backend of Humlock */

#include "runtime-c.h"

#undef PRINT_HEAPSIZE

unsigned long *temp;
unsigned long *stackframe[NUM_STACK_VARS];
unsigned long newtag;
unsigned long *exception_handler[1];

Heap_error_fn_t heap_error_callback;

void efficient_copy(void *d, void *s, unsigned long words)
{
  unsigned long i;

  for (i=0; i<words; i++)
  {
    *((unsigned long *)d + i) = *((unsigned long *)s + i);
  }
}

void efficient_set(void *d, unsigned long target, unsigned long words)
{
  unsigned long i;

  for (i=0; i<words; i++)
  {
    *((unsigned long *)d + i) = target;
  }
}

static unsigned long HEAPtotalsize = NUM_HEAP_WORDS;
static unsigned long HEAPbuffer[NUM_HEAP_WORDS * 2];
static unsigned long *HEAP;
static unsigned long HEAPactive;
static unsigned long HEAPinactive;
static unsigned long HEAPnextunused;
static unsigned long HEAPnextunusedinactive;
static unsigned long HEAPscavengeindex;

#define hWordsRemaining (HEAPtotalsize - HEAPnextunused)

void initializeHeap ( Heap_error_fn_t f )
{
  HEAP = HEAPbuffer;
  HEAPnextunused = 0;
  HEAPnextunusedinactive = 0;
  HEAPscavengeindex = 0;
  HEAPactive = 0;
  HEAPinactive = HEAPtotalsize;
  heap_error_callback = f;
}


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

unsigned long hExtractTag ( unsigned long *h )
{
  return (*h & HEAPtagmask);
}

void setForwardBit ( unsigned long *new_el, unsigned long *old )
{
  *old = *old | HEAPforwardbiton;
  *(old + 1) = (unsigned long) new_el;
}

unsigned long isForwardBitSet ( unsigned long tag )
{
  return (tag & HEAPforwardbiton);
}

unsigned long *hCopyToInactive ( unsigned long *old )
{
  unsigned long *target;
  unsigned long i, len;

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
    heap_error_callback(HEAP_GENERAL_ERROR);
    break;
  }

  return target;
}

unsigned long *hEvacuateNode ( unsigned long *old )
{
  unsigned long tag;
  unsigned long *new_el;

  tag = *old;
  if (isForwardBitSet(tag))
  {
    new_el = (unsigned long *) *(old + 1);
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
  unsigned long inactive_idx, scavenge_idx;
  unsigned long i, len, dx;
  unsigned long *scavenge_ptr;

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
      if ((unsigned long *) *(scavenge_ptr + 1) > 0)
      {
        *(scavenge_ptr + 1) = (unsigned long) hEvacuateNode ( (unsigned long *) *(scavenge_ptr + 1) );
      }
      dx = 2;
      break;
    case HEAPtracedmask:
      len = *scavenge_ptr & HEAPmask;
      for (i = 0; i < len; i++)
      {
        *(scavenge_ptr + i + 1) = (unsigned long) hEvacuateNode ( (unsigned long*) *(scavenge_ptr + i + 1) );
      }
      dx = len + 1;
      if (len == 0)
      {
        dx += 1;
      }
      break;
    default:
      dx = 0;
      heap_error_callback(HEAP_GENERAL_ERROR);
      break;
    }
    HEAPscavengeindex += dx;

    inactive_idx = HEAPinactive + HEAPnextunusedinactive;
    scavenge_idx = HEAPinactive + HEAPscavengeindex;
  }
}

void hTransferStackFrame ( unsigned long context_len )
{
  unsigned long i;
  unsigned long *heap_ptr;

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

#ifdef PRINT_HEAP_INFO
void dumpHeapElement ( unsigned long *h )
{
  unsigned long tag;
  unsigned long i, len;

  tag = hExtractTag(h);

  printf("nodeaddr = %p ", h);
  printf("< hdr=%08x ", *h);
  switch(tag)
  {
  case HEAPuntracedmask:
    printf("value=%08x ", *(h+1));
    break;
  case HEAPtaggedmask:
    printf("tag=%08x ", *(h+1));
    break;
  case HEAPtracedmask:
    len = *h & HEAPmask;
    for (i = 0; i < len; i++)
    {
      printf("addr=%08x ", *(h+1+i));
    }
    break;
  default:
    heap_error_callback(HEAP_GENERAL_ERROR);
    break;
  }
  printf(">\n");
}
#endif

#ifdef PRINT_HEAP_INFO
void hScan ( void )
{
  unsigned long active_idx, scan_idx;
  unsigned long len, dx;
  unsigned long *scan_ptr;

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
      heap_errror_callback(HEAP_GENERAL_ERROR);
      break;
    }
    scan_idx += dx;
  }
}
#endif

void hGarbageCollect ( unsigned long context_len )
{
  hTransferStackFrame ( context_len );
  hScavenge();
  hSwitchHeaps();
#if PRINT_HEAP_INFO
  /*  hScan(); */
#endif
}

void checkHeapForSpace ( unsigned long context_len, unsigned long size_in_words )
{
  if (size_in_words > hWordsRemaining)
  {
    hGarbageCollect ( context_len );
#ifdef PRINT_HEAP_INFO
    printf("Heap Size = %d\n", HEAPnextunused * sizeof(unsigned long));
#endif
    if (size_in_words > hWordsRemaining)
    {
      heap_error_callback(HEAP_NO_SPACE);
    }
  }
}

unsigned long *hAlloc( unsigned long context_len, unsigned long size_in_words )
{
  unsigned long *new_ptr;

  checkHeapForSpace ( context_len, size_in_words );
  new_ptr = HEAP + HEAPactive + HEAPnextunused;
  HEAPnextunused += size_in_words;
  return new_ptr;
}

unsigned long *alloc_untraced(unsigned long value, unsigned long context_len)
{
  unsigned long *ptr;

  ptr = hAlloc(context_len, 2);

  *ptr = 0;
  *(ptr + 1) = value;

  return ptr;
}

unsigned long *alloc_traced_string(unsigned long traced_size_in_words, unsigned long context_len)
{
  unsigned long size;
  unsigned long *ptr;
  unsigned long i;
  unsigned long *first_char_ptr;

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
      *(ptr + i + 1) = (unsigned long) (first_char_ptr + (i * 2));
    }
  }
  else
  {
    /* set next word to zero.  this is important for 0 length arrays */
    *(ptr + 1) = 0;
  }

  return ptr;
}

unsigned long *alloc_traced_array(unsigned long traced_size_in_words, unsigned long context_len)
{
  unsigned long size;
  unsigned long *ptr;

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

unsigned long *alloc_tagged(unsigned long tag, unsigned long context_len)
{
  unsigned long *ptr;

  ptr = hAlloc(context_len, 2);

  *ptr = (tag & HEAPmask) | HEAPtaggedmask;
  
  return ptr;
}

/* End of runtime system */

