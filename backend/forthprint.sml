
structure ForthPrint =
struct

  exception Print of string 
  structure W = Word32

  open Forth
  fun print includemain  out runtime (blocks, lab) =
      let
        val file = TextIO.openOut out

        val MAX_LEN = 72
        val cur_len = ref 0
        fun p s = let in
                    (if String.size s + !cur_len > MAX_LEN then
                      (TextIO.output (file, "\n");
                       cur_len := 0)
                    else
                      ());
                    TextIO.output (file, s);
                    cur_len := !cur_len + String.size s
                  end

        fun printasm (h::asm) = ((case h of CONST i => p (StringUtil.lcase (W.toString i) ^ " ")
                                                  | ADD => p ("+ ")
                                                  | ONEPLUS => p ("1+ ")
                                                  | SUBTRACT => p ("- ")
                                                  | DUP => p ("dup ")
                                                  | TWODUP => p ("2dup ")
                                                  | SWAP => p ("swap ")
                                                  | DROP => p ("drop ")
                                                  | ROT => p ("rot ")
                                                  | TOR => p (">r ")
                                                  | FROMR => p ("r> ")
                                                  | COPYR => p ("r@ ")
                                                  | EXECUTE => p ("execute ")
                                                  | STORE => p ("! ")
                                                  | STORE16 => p ("w! ")
                                                  | DEBUG s => p ("") (* (s ^ " ") *)
                                                  | DEREFERENCE => p ("@ ")
                                                  | CASE => p ("case ")
                                                  | ENDCASE => p ("endcase ")
                                                  | OF => p ("of ")
                                                  | ENDOF => p ("endof ")
                                                  | EXCEPTION_HANDLER_REF => p ("ex ")
                                                  | NEW_TAG_REF => p ("nt ")
                                                  | NATIVE_CALL f => p (" " ^ f ^ " ")
                                                  | VARIABLE_REF i => p (StringUtil.lcase (W.toString i) ^ " v ")
                                                  | LABEL_REF s => p (s ^ "_r ")
                                                  | COMMENT s => p ("") (* ("\n\n s\" " ^ s ^ " \" type cr ") *)
                                                  | NEWLINE => p ("") (* ("\n") *)
                                                  | ALLOC_UNTRACED => p ("au ")
                                                  | ALLOC_TAGGED => p ("at ")
                                                  | ALLOC_TRACED_ARRAY => p ("ay ")
                                                  | ALLOC_TRACED_STRING => p ("as ")
                                                  | WRITE_C0 => p ("emit ")
                                                  | READ_C0 => p ("c0_getc ")
                                                  | WRITE_S0 => p (">co ")
                                                  | READ_S0 => p ("s0_getc ")
                                                  | AVAIL_C0 => p ("c0_avail ")
                                                  | AVAIL_S0 => p ("s0_avail ")
                                                  | CMP_EQ => p ("= ")
                                                  | CMP_NEQ => p ("<> ")
                                                  | CMP_LESSTHAN => p ("< ")
                                                  | CMP_LESSTHANEQ => p ("> invert ")
                                                  | CMP_GREATERTHAN => p ("> ")
                                                  | CMP_GREATERTHANEQ => p ("< invert ")
                                                  | DO => p ("do ")
                                                  | LOOP => p ("loop ")
                                                  | I => p ("i ")
                                                  | IF => p ("if ")
                                                  | ELSE => p ("else ")
                                                  | THEN => p ("then ")
                                                  | MULTIPLY => p ("* ")
                                                  | DIVIDE  => p ("/ ")
                                                  | UDIVMOD  => p ("um/mod ")
                                                  | MOD  => p ("mod ")
                                                  | AND => p ("and ")
                                                  | XOR => p ("xor ")
                                                  | OR => p ("or ")
                                                  | INVERT => p ("invert ")
                                                  | LSHIFT => p ("lshift ")
                                                  | RSHIFT => p ("rshift ")
                                                  | SECONDS => p ("seconds ")
                                                  | SLEEP => p ("sleep ")
                                                  | COPY => p ("ey ")
                                                  | SET => p ("es "));

                                         printasm asm)
                                        
          | printasm nil = (p "\n")

        fun printblock (l, asm) = 
            let 
                val v = Variable.tostring l
                (* define an additional instruction that will *)
                (* drop the value on the return stack, since *)
                (* we'll never be back to the function that called us *)
                val tailcall_rdrop = if Variable.eq(l, lab) then " " else " r> drop "
                val depth_debugging = "" (* " s\" fncall " ^ v ^ " \"  " ^ " type depth . cr " *)
            in
                p (": "  ^ v ^ " " ^ tailcall_rdrop ^ depth_debugging ^ "\n");
                printasm asm;
                p ";\n";
                p (" ' " ^ v ^ "\n" ^ v ^ "_r !\n\n")
            end

        fun generateFunctionVariables (lab, asm) = 
            let 
                val v = Variable.tostring lab
            in
                p ("variable "  ^ v ^ "_r\n")
            end

        val ordered_blocks = rev(blocks)

      in
        TextIO.output (file, runtime);
        app generateFunctionVariables ordered_blocks;
        app printblock ordered_blocks;
        (if includemain then
          p ("\n cr \n" ^ Variable.tostring(lab) ^ "\n cr cr \n")
        else
          ());
        TextIO.flushOut file;
        TextIO.closeOut file
      end

end
