/* Runtime system for the C backend of Humlock */

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#define GMHEAPforwardbiton 0x80000000
#define GMHEAPforwardbitoff 0x7fffffff

#define GMHEAPmask 0x1fffffff 
#define GMHEAPtagmask 0x60000000 
#define GMHEAPuntracedmask 0x00000000 
#define GMHEAPtaggedmask 0x20000000 
#define GMHEAPtracedmask 0x40000000 

uint32_t *label_address;

uint32_t *stackframe[1000];

uint32_t *newtag[1];
uint32_t *exception_handler[1];

uint32_t alloc_untraced(uint32_t value, uint32_t context_len)
{
  uint32_t ptr;

  ptr = (uint32_t) malloc (2 * sizeof(uint32_t));

  *(uint32_t *)ptr = 0;
  *((uint32_t*)ptr + 1) = value;

  return ptr;
}

uint32_t alloc_traced_array(uint32_t traced_size_in_words, uint32_t context_len)
{
  uint32_t size;
  uint32_t ptr;

  /* one extra word for the header */
  if (traced_size_in_words > 0)
    size = traced_size_in_words + 1;
  else
    size = traced_size_in_words + 2;

  ptr = (uint32_t) malloc(size * sizeof(uint32_t));

  /* fill in header word */
  *(uint32_t *)ptr = ( (traced_size_in_words & GMHEAPmask) |
                       GMHEAPtracedmask );
  
  /* set next word to zero.  this is important for 0 length arrays */
  *((uint32_t *)ptr + 1) = 0;

  return ptr;
}

uint32_t alloc_tagged(uint32_t tag, uint32_t context_len)
{
  uint32_t ptr;

  ptr = (uint32_t) malloc (2 * sizeof(uint32_t));

  *(uint32_t *)ptr = (tag & GMHEAPmask) | GMHEAPtaggedmask;
  
  return ptr;
}

/* End of runtime system */


int main(int argc, char **argv)
{
_main:
*(uint32_t *)((stackframe + 0x0)) = alloc_traced_array(0x0, 0x0);
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x2)) = alloc_tagged(0x1, 0x2);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x2)) + 0x4)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x4)) = alloc_tagged(0x0, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = 0x0;
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)(newtag) = (*(uint32_t *)(newtag) + 0x1);
*(uint32_t *)((stackframe + 0x6)) = alloc_untraced(*(uint32_t *)(newtag), 0x6);
*(uint32_t *)(newtag) = (*(uint32_t *)(newtag) + 0x1);
*(uint32_t *)((stackframe + 0x7)) = alloc_untraced(*(uint32_t *)(newtag), 0x7);
*(uint32_t *)((stackframe + 0x8)) = alloc_traced_array(0x2, 0x8);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x8)) + 0x4)) = *(uint32_t *)((stackframe + 0x6));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x8)) + 0x8)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x9)) = alloc_traced_array(0x0, 0x9);
*(uint32_t *)((stackframe + 0xA)) = alloc_untraced((uint32_t) &&_ret, 0xa);
*(uint32_t *)((stackframe + 0xB)) = alloc_traced_array(0x2, 0xb);
*(uint32_t *)((*(uint32_t *)((stackframe + 0xB)) + 0x4)) = *(uint32_t *)((stackframe + 0xA));
*(uint32_t *)((*(uint32_t *)((stackframe + 0xB)) + 0x8)) = *(uint32_t *)((stackframe + 0x9));
*(uint32_t *)((stackframe + 0xC)) = alloc_untraced(0x68, 0xc);
*(uint32_t *)((stackframe + 0xD)) = *(uint32_t *)((stackframe + 0xC));
*(uint32_t *)((stackframe + 0xE)) = *(uint32_t *)((stackframe + 0xB));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0xE));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0xD));
goto _12_fn;
_1_ctor_:
*(uint32_t *)((stackframe + 0x2)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x1));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x3));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x2)) + 0x4));
goto *label_address;
_1_match:
*(uint32_t *)((stackframe + 0x3)) = alloc_traced_array(0x2, 0x3);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x8)) = *(uint32_t *)((stackframe + 0x1));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x6));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x5));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4));
goto *label_address;
_12_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_1_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x65, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _13_fn;
_13_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_1_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_2_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x6c, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _14_fn;
_14_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_2_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_3_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x6c, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _15_fn;
_15_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_3_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_4_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x6f, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _16_fn;
_16_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_4_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_5_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x20, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _17_fn;
_17_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_5_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_6_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x77, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _18_fn;
_18_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_6_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_7_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x6f, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _19_fn;
_19_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_7_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_8_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x72, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _20_fn;
_20_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_8_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_9_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x6c, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _21_fn;
_21_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_9_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_10_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x64, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _22_fn;
_22_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_10_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_untraced((uint32_t) &&_11_ret, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x2, 0x4);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x4)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x4)) + 0x8)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0xa, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _23_fn;
_23_fn:
putc(*(uint32_t *)((*(uint32_t *)((stackframe + 0x1)) + 0x4)), stdout);
*(uint32_t *)((stackframe + 0x2)) = 0x0;
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x0)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_11_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
return (0);
return(0);}
