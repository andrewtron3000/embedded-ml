
(* Forth language *)
structure Forth =
struct
  structure W = Word32

  exception Forth of string

  datatype inst =

         CONST of W.word 
         | ADD
         | ONEPLUS
         | SUBTRACT
         | DUP
         | TWODUP
         | SWAP
         | DROP
         | ROT
         | TOR
         | FROMR
         | COPYR
         | EXECUTE
         | STORE
         | STORE16
         | DEBUG of string
         | DEREFERENCE
         | CASE
         | ENDCASE
         | OF
         | ENDOF
         | EXCEPTION_HANDLER_REF
         | NEW_TAG_REF
         | NATIVE_CALL of string
         | VARIABLE_REF of W.word
         | LABEL_REF of string
         | COMMENT of string
         | NEWLINE
         | ALLOC_UNTRACED  (* size -- addr *)
         | ALLOC_TAGGED  (* size -- addr *)
         | ALLOC_TRACED_ARRAY  (* size -- addr *)
         | ALLOC_TRACED_STRING  (* size -- addr *)
         | WRITE_C0
         | READ_C0
         | WRITE_S0
         | READ_S0
         | AVAIL_C0
         | AVAIL_S0
         | CMP_EQ
         | CMP_NEQ
         | CMP_LESSTHAN
         | CMP_LESSTHANEQ
         | CMP_GREATERTHAN
         | CMP_GREATERTHANEQ
         | DO
         | LOOP
         | I
         | IF
         | ELSE
         | THEN
         | MULTIPLY
         | DIVIDE (* signed! *)
         | UDIVMOD (* unsigned mod and div *)
         | MOD 
         | AND
         | XOR
         | OR
         | INVERT
         | LSHIFT
         | RSHIFT
 
         | SECONDS
         | SLEEP

         | COPY
         | SET

end
