
(* C language *)
structure C =
struct
  structure W = Word32

  exception C of string

  datatype inst =

         CONST of W.word 
         | DEBUG of string
         | COMMENT of string
         | LABEL_REF of string
         | GETC
         | PUTC of inst
         | AVAILC
         | ADDRESS_OF of inst
         | ALLOC_TRACED_STRING of inst * inst
         | ALLOC_TRACED_ARRAY of inst * inst
         | ALLOC_TAGGED of inst * inst
         | ALLOC_UNTRACED of inst * inst
         | UPDATE_STACK of W.word * inst
         | STACKVAR of W.word
         | INTVAL of inst
         | TUPLEVAR of inst * W.word
         | ARRAYVAR of inst * inst
         | STORE of inst * inst
         | NATIVE_CALL of string * W.word * inst
         | VARIABLE_REF of W.word
         | DEREFERENCE of inst
         | ADD of inst * inst 
         | SUBTRACT of inst * inst
         | MULTIPLY of inst * inst 
         | DIVIDE of inst * inst
         | SDIVIDE of inst * inst
         | MODULO of inst * inst
         | AND of inst * inst
         | XOR of inst * inst
         | OR of inst * inst
         | NOT of inst
         | LSHIFT of inst * inst
         | RSHIFT of inst * inst
         | CMP_EQ of inst * inst
         | CMP_NEQ of inst * inst
         | CMP_LESSTHAN of inst * inst
         | CMP_LESSTHANEQ of inst * inst
         | CMP_GREATERTHAN of inst * inst
         | CMP_GREATERTHANEQ of inst * inst
         | IF of inst * inst list * inst list
         | SWITCH of inst * (inst * inst list) list * inst list
         | LABEL_AS_VALUE of inst
         | TEMP_VARIABLE
         | GOTO_LABEL of inst
         | GOTO_ADDRESS of inst
         | NEW_TAG_REF
         | EXCEPTION_HANDLER_REF
         | SEPARATOR
         | SET of inst * inst * inst
         | COPY of inst * inst * inst
         | HALT






(*     (\* A <- B  unless C = 0 *\) *)
(*     CMOV of reg * reg * reg *)
(*     (\* replace code with array in A, *)
(*        goto address in B *\) *)
(*   | LOADPROG of reg * reg *)
(*   | LITERAL of reg * W.word (\* limited to 24 bits? *\) *)
(*   | HALT *)
(*   | READ of reg *)
(*   | WRITE of reg *)
(*   (\* A <- id of new array with B words *\) *)
(*   | ALLOC of reg * reg *)
(*   (\* free register *\) *)
(*   | FREE of reg *)
(*   (\* A <- B[C] *\) *)
(*   | ASUB of reg * reg * reg *)
(*   (\* A[B] <- C *\) *)
(*   | UPD of reg * reg * reg *)
(*   | ADD of reg * reg * reg *)
(*   | MUL of reg * reg * reg *)
(*   | DIV of reg * reg * reg *)
(*   | NAND of reg * reg * reg *)

(*   (\* advisory ops here. these don't do anything, *)
(*      but might make it easier to optimize or *)
(*      read the code. *\) *)
(*   (\* ... *\) *)
(*   | COMMENT of string *)

(*   (\* debug instructions; the compiler will output these, but they are not part *)
(*      of the UM spec.  their encodings ensure that they will have no effect in *)
(*      compliant implementations. *\) *)

(*   (\* temporarily stop execution *\) *)
(*   | BREAK *)
(*   (\* print out debugging information (a constant and the contents of a *)
(*    register *\) *)
(*   | INFO of W.word * reg *)
(*   (\* read the time stamp counter into the given register *\) *)
(*   | RDTSC of reg *)

(*   (\* macro-ops here. these don't correspond to *)
(*      individual instructions, but can be expanded *)
(*      in place *\) *)

(*   (\* jump to destination if test reg is not zero *)
(*      (zero, less-than-zero respectively). give  *)
(*      the test register, the destination, and three *)
(*      temporaries *\) *)
(*   | JNZO of reg * Variable.var * reg * reg * reg option *)
(*   | JZO  of reg * Variable.var * reg * reg * reg option *)
(*   | JLZO of reg * Variable.var * reg * reg * reg option *)

(*   (\* CALLWITH (a, v, t1, t2) *)
(*      call to a label, storing return address *)
(*      (= next instruction) in a *\) *)
(*   | CALLWITH of reg * Variable.var * reg * reg *)

(*    (\* If we want to support long jumps (> 300mb), *)
(*       supply SOME tmp for the last register. *\) *)
(*   | LITERAL_ADDRO of reg * Variable.var * reg option *)

(*   (\* load a 32-bit word *\) *)
(*   | LITERAL_ANY of reg * W.word * reg *)
(*     (\* sometimes small enough to use LITERAL. *)
(*        sometimes we can LITERAL and then NOT. *\) *)
(*     (\* at worst: *)
(*        LITERAL reg, word_hi16 *)
(*        LITERAL tmp, 65536 *)
(*        MUL     reg <- tmp * reg *)
(*        LITERAL tmp, word_low16 *)
(*        ADD     reg <- tmp + reg *\) *)

(*   (\* a <- b - c, trashing d *\) *)
(*   | SUB of reg * reg * reg * reg *)
(*     (\* NAND a, c, c   (\* a = ~c *\) *)
(*        ADD a, b, a *)
(*        LITERAL d, 1 *)
(*        ADD a, t, a *)
(*        *\) *)

(*   (\* a <- b | c, trashing d *\) *)
(*   | OR of reg * reg * reg * reg *)
(*     (\* NAND d, b, b *)
(*        NAND a, c, c *)
(*        NAND a, d, a *\) *)

(*    (\* PERF probably a faster way exists? *\) *)
(*   (\* a <- b ^ c, trashing d *\) *)
(*   | XOR of reg * reg * reg * reg *)
(*     (\* OR a, b, c, d *)
(*        NAND d, b, c *)
(*        NAND a, d, a *)
(*        NAND a, a, a *\) *)

(*     (\* a <- ~b *\) *)
(*   | NOT of reg * reg *)

(*   | AND of reg * reg * reg *)
(*     (\* NAND a, b, c, *)
(*        NAND a, a, a *\) *)

(*     (\* a--, trashing b *\) *)
(*   | DEC of reg * reg *)
(*     (\* a = b - 1 *\) *)
(*   | DECTO of reg * reg *)

(*   (\* other stuff *\) *)
(*   (\* encode the instruction given *)
(*      with the wrong endianness. we *)
(*      use this for self-checking *)
(*      purposes. *\) *)
(*   | SWAPEND of inst *)
(*   (\* just give the word directly *\) *)
(*   | DATA of W.word *)

(*   (\* address of this label *\) *)
(*   | DATALAB of Variable.var *)

(*    (\* reserve one word of space,  *)
(*       but it needn't be initialized*\) *)
(*   | BSSDATA *)

(*   (\* convenient when calling functions *)
(*      that return lists of instructions *\) *)
(*   | MANY of inst list *)

(*    (\* no instruction; just marks this label *)
(*       for this spot *\) *)
(*   | LABEL of Variable.var *)

end
