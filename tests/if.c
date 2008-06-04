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
*(uint32_t *)((stackframe + 0x9)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
switch((*(uint32_t *)(*(uint32_t *)((stackframe + 0x3))) & 0x1fffffff)) { case 0x1: ;
*(uint32_t *)((stackframe + 0xA)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x5)) + 0x4));
switch((*(uint32_t *)(*(uint32_t *)((stackframe + 0x5))) & 0x1fffffff)) { case 0x1: ;
*(uint32_t *)((stackframe + 0xB)) = alloc_traced_array(0x0, 0xb);
*(uint32_t *)((stackframe + 0xC)) = alloc_untraced((uint32_t) &&_2_ret, 0xc);
*(uint32_t *)((stackframe + 0xD)) = alloc_traced_array(0x2, 0xd);
*(uint32_t *)((*(uint32_t *)((stackframe + 0xD)) + 0x4)) = *(uint32_t *)((stackframe + 0xC));
*(uint32_t *)((*(uint32_t *)((stackframe + 0xD)) + 0x8)) = *(uint32_t *)((stackframe + 0xB));
*(uint32_t *)((stackframe + 0xE)) = alloc_untraced(0x6e, 0xe);
*(uint32_t *)((stackframe + 0xF)) = *(uint32_t *)((stackframe + 0xE));
*(uint32_t *)((stackframe + 0x10)) = *(uint32_t *)((stackframe + 0xD));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x10));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0xF));
goto _7_fn;
break;
default: ;
*(uint32_t *)((stackframe + 0xB)) = alloc_traced_array(0x0, 0xb);
*(uint32_t *)((stackframe + 0xC)) = alloc_untraced((uint32_t) &&_4_ret, 0xc);
*(uint32_t *)((stackframe + 0xD)) = alloc_traced_array(0x2, 0xd);
*(uint32_t *)((*(uint32_t *)((stackframe + 0xD)) + 0x4)) = *(uint32_t *)((stackframe + 0xC));
*(uint32_t *)((*(uint32_t *)((stackframe + 0xD)) + 0x8)) = *(uint32_t *)((stackframe + 0xB));
*(uint32_t *)((stackframe + 0xE)) = alloc_untraced(0x6f, 0xe);
*(uint32_t *)((stackframe + 0xF)) = *(uint32_t *)((stackframe + 0xE));
*(uint32_t *)((stackframe + 0x10)) = *(uint32_t *)((stackframe + 0xD));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x10));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0xF));
goto _9_fn;
break;
 }break;
default: ;
*(uint32_t *)((stackframe + 0xA)) = alloc_traced_array(0x0, 0xa);
*(uint32_t *)((stackframe + 0xB)) = alloc_untraced((uint32_t) &&_7_ret, 0xb);
*(uint32_t *)((stackframe + 0xC)) = alloc_traced_array(0x2, 0xc);
*(uint32_t *)((*(uint32_t *)((stackframe + 0xC)) + 0x4)) = *(uint32_t *)((stackframe + 0xB));
*(uint32_t *)((*(uint32_t *)((stackframe + 0xC)) + 0x8)) = *(uint32_t *)((stackframe + 0xA));
*(uint32_t *)((stackframe + 0xD)) = alloc_untraced(0x6e, 0xd);
*(uint32_t *)((stackframe + 0xE)) = *(uint32_t *)((stackframe + 0xD));
*(uint32_t *)((stackframe + 0xF)) = *(uint32_t *)((stackframe + 0xC));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0xF));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0xE));
goto _11_fn;
break;
 }_1_ctor_:
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
_1_1$hoist:
*(uint32_t *)((stackframe + 0x3)) = alloc_traced_array(0x0, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x0, 0x4);
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced((uint32_t) &&_ret, 0x5);
*(uint32_t *)((stackframe + 0x6)) = alloc_traced_array(0x2, 0x6);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x6)) + 0x4)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x6)) + 0x8)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x8)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((stackframe + 0x9)) = *(uint32_t *)((stackframe + 0x6));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x9));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x8));
*(uint32_t *)((stackframe + 0x2)) = *(uint32_t *)((stackframe + 0x7));
goto _1_match;
_ret:
*(uint32_t *)((stackframe + 0x2)) = *(uint32_t *)(exception_handler);
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x2)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_casejoin:
*(uint32_t *)((stackframe + 0x1)) = alloc_traced_array(0x0, 0x1);
*(uint32_t *)((stackframe + 0x2)) = alloc_untraced((uint32_t) &&_6_ret, 0x2);
*(uint32_t *)((stackframe + 0x3)) = alloc_traced_array(0x2, 0x3);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x8)) = *(uint32_t *)((stackframe + 0x1));
*(uint32_t *)((stackframe + 0x4)) = alloc_untraced(0xa, 0x4);
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x6));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x5));
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
_6_ret:
return (0);
_1_3$hoist:
*(uint32_t *)((stackframe + 0x3)) = alloc_traced_array(0x0, 0x3);
*(uint32_t *)((stackframe + 0x4)) = alloc_traced_array(0x0, 0x4);
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced((uint32_t) &&_1_ret, 0x5);
*(uint32_t *)((stackframe + 0x6)) = alloc_traced_array(0x2, 0x6);
*(uint32_t *)((*(uint32_t *)((stackframe + 0x6)) + 0x4)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((*(uint32_t *)((stackframe + 0x6)) + 0x8)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x8)) = *(uint32_t *)((stackframe + 0x3));
*(uint32_t *)((stackframe + 0x9)) = *(uint32_t *)((stackframe + 0x6));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x9));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x8));
*(uint32_t *)((stackframe + 0x2)) = *(uint32_t *)((stackframe + 0x7));
goto _1_match;
_1_ret:
*(uint32_t *)((stackframe + 0x2)) = *(uint32_t *)(exception_handler);
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((*(uint32_t *)((stackframe + 0x2)) + 0x4));
*(uint32_t *)((stackframe + 0x4)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x5)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x4));
label_address = (uint32_t *)*(uint32_t *)((*(uint32_t *)((stackframe + 0x3)) + 0x4));
goto *label_address;
_1_casejoin:
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x0));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x1));
goto _casejoin;
_7_fn:
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
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x6f, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _8_fn;
_8_fn:
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
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x3));
goto _1_casejoin;
_9_fn:
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
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x6b, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _10_fn;
_10_fn:
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
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x3));
goto _1_casejoin;
_11_fn:
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
*(uint32_t *)((stackframe + 0x5)) = alloc_untraced(0x6f, 0x5);
*(uint32_t *)((stackframe + 0x6)) = *(uint32_t *)((stackframe + 0x5));
*(uint32_t *)((stackframe + 0x7)) = *(uint32_t *)((stackframe + 0x4));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x7));
*(uint32_t *)((stackframe + 0x1)) = *(uint32_t *)((stackframe + 0x6));
goto _12_fn;
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
_8_ret:
*(uint32_t *)((stackframe + 0x2)) = alloc_traced_array(0x0, 0x2);
*(uint32_t *)((stackframe + 0x3)) = *(uint32_t *)((stackframe + 0x2));
*(uint32_t *)((stackframe + 0x0)) = *(uint32_t *)((stackframe + 0x3));
goto _casejoin;
return(0);}
