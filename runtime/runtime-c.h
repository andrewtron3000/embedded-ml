#ifndef __RUNTIME_C_H__
#define __RUNTIME_C_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>
  
#define HEAPforwardbiton 0x80000000
#define HEAPforwardbitoff 0x7fffffff

#define HEAPmask 0x1fffffff 
#define HEAPtagmask 0x60000000 
#define HEAPuntracedmask 0x00000000 
#define HEAPtaggedmask 0x20000000 
#define HEAPtracedmask 0x40000000 

#define NUM_STACK_VARS 200
#define NUM_HEAP_WORDS 3192

#define D(x) ( *(x) )

/* Stackvar : int -> hptr */
#define Stackvar(i) ( D(stackframe + (i)) )

/* Intval : hptr -> int */
#define Intval(x) ( D ((x) + 1) )

/* Tupleval : hptr x int -> hptr */
#define Tupleval(x, i) ( D((uint32_t **) ((x) + 1 + i)) )

/* Arrayval : hptr x int -> hptr */
#define Arrayval(x, i) ( Tupleval(x, i) )

/* Arraylen : hptr -> int */
#define Arraylen(x) ( (*x) & HEAPmask ) 

extern uint32_t *temp;
extern uint32_t *stackframe[];
extern uint32_t newtag;
extern uint32_t *exception_handler[];
extern uint32_t storage[];

typedef enum {
  HEAP_NO_SPACE,
  HEAP_GENERAL_ERROR
} Heap_error_t;
  
typedef void (*Heap_error_fn_t)(Heap_error_t);

void efficient_copy(void *d, void *s, uint32_t words);
void efficient_set(void *d, uint32_t target, uint32_t words);

uint32_t hExtractTag ( uint32_t *h );

uint32_t *alloc_untraced(uint32_t value, uint32_t context_len);
uint32_t *alloc_traced_string(uint32_t traced_size_in_words, uint32_t context_len);
uint32_t *alloc_traced_array(uint32_t traced_size_in_words, uint32_t context_len);
uint32_t *alloc_tagged(uint32_t tag, uint32_t context_len);

void initializeHeap(Heap_error_fn_t f);

#ifdef __cplusplus
}
#endif

#endif /* __RUNTIME_C_H__ */
