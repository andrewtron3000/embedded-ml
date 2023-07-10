#ifndef __RUNTIME_C_H__
#define __RUNTIME_C_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

#if INTPTR_MAX == INT32_MAX
#define PROCESSOR_BITS 32
#define HEAPforwardbiton  0x80000000
#define HEAPforwardbitoff 0x7fffffff
#define HEAPmask          0x1fffffff 
#define HEAPtagmask       0x60000000 
#define HEAPtaggedmask    0x20000000 
#define HEAPtracedmask    0x40000000 
#elif INTPTR_MAX == INT64_MAX
#define PROCESSOR_BITS 64
#define HEAPforwardbiton  0x8000000000000000
#define HEAPforwardbitoff 0x7fffffffffffffff
#define HEAPmask          0x1fffffffffffffff
#define HEAPtagmask       0x6000000000000000
#define HEAPtaggedmask    0x2000000000000000 
#define HEAPtracedmask    0x4000000000000000
#else
#error "Processor neither 32 nor 64-bit."
#endif

#define HEAPuntracedmask 0x00000000 

#define NUM_STACK_VARS 200
#define NUM_HEAP_WORDS 3192

#define D(x) ( *(x) )

/* Stackvar : int -> hptr */
#define Stackvar(i) ( D(stackframe + (i)) )

/* Intval : hptr -> int */
#define Intval(x) ( D ((x) + 1) )

/* Tupleval : hptr x int -> hptr */
#define Tupleval(x, i) ( D((unsigned long **) ((x) + 1 + i)) )

/* Arrayval : hptr x int -> hptr */
#define Arrayval(x, i) ( Tupleval(x, i) )

/* Arraylen : hptr -> int */
#define Arraylen(x) ( (*x) & HEAPmask ) 

extern unsigned long *temp;
extern unsigned long *stackframe[];
extern unsigned long newtag;
extern unsigned long *exception_handler[];
extern unsigned long storage[];

typedef enum {
  HEAP_NO_SPACE,
  HEAP_GENERAL_ERROR
} Heap_error_t;
  
typedef void (*Heap_error_fn_t)(Heap_error_t);

void efficient_copy(void *d, void *s, unsigned long words);
void efficient_set(void *d, unsigned long target, unsigned long words);

unsigned long hExtractTag ( unsigned long *h );

unsigned long *alloc_untraced(unsigned long value, unsigned long context_len);
unsigned long *alloc_traced_string(unsigned long traced_size_in_words, unsigned long context_len);
unsigned long *alloc_traced_array(unsigned long traced_size_in_words, unsigned long context_len);
unsigned long *alloc_tagged(unsigned long tag, unsigned long context_len);

void initializeHeap(Heap_error_fn_t f);

#ifdef __cplusplus
}
#endif

#endif /* __RUNTIME_C_H__ */
