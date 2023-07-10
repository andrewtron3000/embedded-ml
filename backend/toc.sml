(* convert from allocation-converted CPS language to C *)

structure ToC = 
struct

open CPS
open C
open Primop
open ContextMap
structure W = Word32

exception ToC of string

val debugopt = Params.flag false
                           (SOME ("-debugtoforth", 
                                  "Debug the Forth backend")) "debugtoforth"

fun convert (Fix(fns, App(Label main, nil))) = let
    fun debugdo f = if !debugopt then f () else ()
    fun dprint s = if !debugopt then print ("[TOC] " ^ s ^ "\n") else ()
    fun dprintd f = if !debugopt then print ("[TOC] " ^ f() ^ "\n") else ()
    val HEAPtagmask = 0x1fffffff

    fun convertFunction (lab, vars, ce) =
        let
            val blockset = ref nil
            val bytes_per_word = 4
 
            fun dereferenceVariableOntoStack context var = 
                let
                    val ci = ContextMap.findInContext(context, var)
                    val target = W.fromInt(#position(ci))
                in
                    STACKVAR target
                end

            fun convertToHeapAddr c a = case a of Label _ => raise ToC "label?"
                                                | Var vble => dereferenceVariableOntoStack c vble
                                                | Int i => raise ToC "int?"
                                                                 
            fun convertToValue c a = case a of Label _ => raise ToC "label?"
                                             | Var vble => INTVAL ( dereferenceVariableOntoStack c vble )
                                             | Int i => CONST i

            fun extractTagWord c a = case a of Label _ => raise ToC "label? in extract header word?"
                                             | Var vble => (AND (DEREFERENCE (dereferenceVariableOntoStack c vble),
                                                                 CONST (W.fromInt HEAPtagmask)))  (* SUSP: secret knowledge of heap header flags *)
                                             | Int i => raise ToC "integer? in extract header word?"
                                                              
            fun val_to_var v = case v of Var vbl => vbl
                                       | _ => raise ToC "unexpected parameter in val_to_var" 
                                                    
            fun va_as_string v = case v of Var va => " " ^ ((Variable.tostring o val_to_var) v) ^ " "
                                         | Int i => " " ^ W.toString(i) ^ " "
                                         | _ => raise ToC "unexpected parameter in va_as_string" 
                                                      
            fun vas_as_string vas = concat(map va_as_string vas)
                                    
            fun vas_and_posns_as_string context (v::vs) =
                ((va_as_string v) ^ "at posn=" ^ Int.toString(#position(ContextMap.findInContext(context, (val_to_var v))))
                 ^ " " ^ (vas_and_posns_as_string context vs))
              | vas_and_posns_as_string context nil = ""
                                                      
            fun vars_as_string (v::vs) = 
                Variable.tostring(v) ^ " " ^ vars_as_string(vs)
              | vars_as_string (nil) = ""
                                       
            fun handleV c v is =
                let
                    val new_context_info = addToContext(c, v)
                    val target = W.fromInt (#position(new_context_info))
                in
                    (#context(new_context_info),
                     is @ [UPDATE_STACK (target, CONST (W.fromInt 0))])
                end
                
            fun handleVs c (v::vs) is =
                let
                    val (new_ctx, new_instrs) = (handleV c v is);
                in
                    handleVs new_ctx vs new_instrs
                end
              | handleVs c (nil) is = (c, is)
                                      
            fun convertBlock (context, Fix _) = raise ToC "convertBlock: Fix" 
              | convertBlock (context, Deferred _) = raise ToC  "convertBlock: Deferred"
                                                           
            (* 
             might have continuations or returns if it was used as a
             primop and we didn't optimize them out yet...
             *)
              | convertBlock (context, Primop(PHalt, [], _, _)) = 
                let 
                in
                    [ HALT ]
                end

              | convertBlock (context, Primop(PHalt, _, _, _)) = raise ToC "bad PHalt"

              | convertBlock (context, Primop(PSet, [Var re, Var va], vs, [c])) =
                let
                    val (context, is) = handleVs context vs nil
                in
                    COMMENT ( "got to pset" ) ::
                    STORE (ARRAYVAR (dereferenceVariableOntoStack context re, CONST (W.fromInt 0)),
                           convertToHeapAddr context (Var va)) ::
                    is @
                    convertBlock(context, c)
                end

              | convertBlock (context, Primop(PSet, _, _, _)) = raise ToC "bad setref"

              | convertBlock (context, Primop(PGet, [Var va], [v], [c])) =
                let
                    val v_context_info = addToContext(context, v)
                    val target = W.fromInt(#position(v_context_info))
                in
                    COMMENT ( "got to pget" ) ::
                    UPDATE_STACK (target,
                                  CAST("unsigned long *", convertToValue context (Var va))) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PGet, _, _, _)) = raise ToC "bad getref"

              | convertBlock (context, Primop(PRef, [Var va], [v], [c])) =
                let
                    val v_context_info = addToContext(context, v)
                    val target = W.fromInt(#position(v_context_info))
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                in
                    COMMENT ( "got to pref" ) ::
                    UPDATE_STACK (target, 
                                  ALLOC_TRACED_ARRAY (CONST (W.fromInt 1),
                                                      context_len)) ::
                    STORE (ARRAYVAR (STACKVAR target, (CONST (W.fromInt 0))),
                           convertToHeapAddr context (Var va)) ::
                    convertBlock(#context(v_context_info), c)
                end
                
              | convertBlock (context, Primop(PRef, _, _, _)) = raise ToC "bad newref"

              | convertBlock (context, Primop(PNewtag, [], [v], [c])) =
                let
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                    val v_context_info = ContextMap.addToContext(context, v)
                    val target = NEW_TAG_REF
                in
                    COMMENT ( "got to newtag" ) ::
                    STORE ( target,
                            ADD (target, CONST (W.fromInt 1)) ) ::
                    UPDATE_STACK ( W.fromInt(#position(v_context_info)),
                                   ALLOC_UNTRACED (target, context_len) ) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PGethandler, [], [v], [c])) =
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(v_context_info))
                in
                    COMMENT ( "got to gethandler" ) ::
                    UPDATE_STACK (target, CAST ("unsigned long *", DEREFERENCE EXCEPTION_HANDLER_REF)) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PGethandler, _, _, _)) = raise ToC "bad gethandler"
                                                                    
              | convertBlock (context, Primop(PSethandler, [Var va], vs, [c])) =
                let
                    val va_context_info = ContextMap.findInContext(context, va)
                    val (context, is) = (handleVs context vs nil)
                in
                    COMMENT ( "got to sethandler" ) ::
                    STORE ( DEREFERENCE EXCEPTION_HANDLER_REF,
                            STACKVAR (W.fromInt(#position(va_context_info))) ) ::
                    is @
                    convertBlock(context, c)
                end

              | convertBlock (context, Primop(PSethandler, _, _, _)) = raise ToC "bad sethandler"

          (* takes int, initial value *)
              | convertBlock (context, Primop(PArray, [vlen, Var vinit], [v], [c])) =
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                    val target = W.fromInt(#position(v_context_info))
                in
                    COMMENT ( "got to PArray" ) ::
                    UPDATE_STACK (target,
                                  ALLOC_TRACED_ARRAY ( convertToValue context vlen,
                                                       context_len )) ::
                    SET ( ADDRESS_OF (ARRAYVAR (STACKVAR target, CONST (W.fromInt 0))),
                          convertToHeapAddr context (Var vinit),
                          convertToValue context vlen ) :: SEPARATOR ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PArray, _, _, _)) = raise ToC "bad array"

              | convertBlock (context, Primop(PArray0, [], [v], [c])) =
                let 
                     val v_context_info = ContextMap.addToContext(context, v)
                     val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                     val target = W.fromInt(#position(v_context_info))
                in
                    COMMENT ( "got to PArray0" ) ::
                    UPDATE_STACK (target,
                                  ALLOC_TRACED_ARRAY ( CONST (W.fromInt 0),
                                                       context_len )) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PArray0, _, _, _)) = raise ToC "bad array0"

              (* array concat, assuming same type (typically char) *)
              | convertBlock (context, Primop(PJointext, arrs, [v], [c])) =
                let 
                    val new_context_info = addToContext(context, v)
                    val target = W.fromInt(#position(new_context_info))

                    fun find_length a = (extractTagWord context a)

                    fun generate_lengths (x::xs) = ADD (find_length x,
                                                       generate_lengths xs)
                      | generate_lengths nil = CONST (W.fromInt 0)

                    fun generate_copy_code a ds = [ COPY (ADDRESS_OF (ARRAYVAR( STACKVAR target,
                                                                                generate_lengths ds )),
                                                          ADDRESS_OF (ARRAYVAR(convertToHeapAddr context a, CONST (W.fromInt 0))),
                                                          find_length a) ]

                    fun generate_copies (x::xs) acc = (generate_copy_code x acc) @ (generate_copies xs (x::acc))
                      | generate_copies nil acc = nil
                in
                    COMMENT ( "got to pjointext" ) ::
                    UPDATE_STACK(target,
                                 ALLOC_TRACED_ARRAY(generate_lengths arrs,
                                                    CONST (W.fromInt (ContextMap.contextLength context)))) ::
                    (generate_copies arrs nil) @
                    convertBlock(#context(new_context_info), c)
                end

              | convertBlock (context, Primop(PJointext, _, _, _)) = raise ToC "bad jointext"

              (* unsafe sub *)
              (* PERF special case const offset *)
              | convertBlock (context, Primop(PSub, [Var varr, voff], [v], [c])) =
                let 
                     val v_context_info = ContextMap.addToContext(context, v)
                     val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                     val target = W.fromInt(#position(v_context_info))
                     val offset_code = case voff of Int i => CONST i
                                                  | Var v => convertToValue context (Var v)
                                                  | _ => raise ToC "invalid voff in PSub"
                     val source = convertToHeapAddr context (Var varr)
                in
                    COMMENT ( "got to PSub" ) ::
                    UPDATE_STACK ( target,
                                   ARRAYVAR (source, offset_code) )::
                    convertBlock(#context(v_context_info), c)
                end
                
              | convertBlock (context, Primop(PSub, _, _, _)) = raise ToC "bad array sub"

              (* unsafe update with constant offset *)
              | convertBlock (context, Primop(PUpdate, [Var varr, off, Var velem], vs, [c])) =
                let 
                    val offset_code = convertToValue context off
                    val (context, is) = (handleVs context vs nil)
                in
                    COMMENT ( "got to PUpdate" ) ::
                    STORE ( ARRAYVAR (convertToHeapAddr context (Var varr), offset_code),
                            convertToHeapAddr context (Var velem) ) ::
                    is @ convertBlock(context, c)
                end

              | convertBlock (context, Primop(PUpdate, _, _, _)) = raise ToC "bad array upd"

              | convertBlock (context, Primop(PArraylength, [Var varr], [v], [c])) =
                let 
                    val v_context_info = ContextMap.addToContext(context, v)
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                    val target = W.fromInt(#position(v_context_info))
                in 
                    COMMENT ( "got to arraylength" ) ::
                    UPDATE_STACK (target, 
                                  ALLOC_UNTRACED (extractTagWord context (Var varr),
                                                  context_len)) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PArraylength, _, _, _)) = raise ToC "bad array len"

              | convertBlock (context, Primop(PNative f, [args], [v], [c])) = 
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                    val context_len = W.fromInt (ContextMap.contextLength context)
                    val target = W.fromInt(#position(v_context_info))
                in
                    COMMENT ("native: " ^ f) ::
                    UPDATE_STACK ( target,
                                   NATIVE_CALL (f, context_len, convertToHeapAddr context args) ) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, App(f, vas)) =
                let 
                    val f_as_string = case f of Label lb => Variable.tostring(lb)
                                              | Var v => (va_as_string (Var v))
                                              | _ => raise ToC "unexpected crap in APP call to f_as_string" 

                    (* only move variables if they would get stomped on otherwise *)
                    val (move_context, movement_code) = 
                        let 
                            fun moveOneVariable(context, v) = 
                                let 
                                    val context_info = findInContext(context, v)
                                    val new_context_info = findInContext(rebindOneVar(context, v), v)
                                    val () = dprint ("moving " ^ Variable.tostring(v) ^ 
                                                     " from position " ^ Int.toString(#position(context_info)) ^ 
                                                     " to position " ^ Int.toString(#position(new_context_info)) ^ "\n" )
                                    val context_string = ContextMap.toString(#context(context_info))
                                    val new_context_string = ContextMap.toString(#context(new_context_info))
                                    val code = UPDATE_STACK (W.fromInt(#position(new_context_info)),
                                                             STACKVAR (W.fromInt(#position(context_info))))
                                in
                                    (#context(new_context_info), [code])
                                end
                                
                            fun moveVariables(context, (v::vs), code_fragment) = 
                                let
                                    val (new_ctxt, new_code_fragment) = moveOneVariable(context, v)
                                in
                                    moveVariables(new_ctxt, vs, code_fragment @ new_code_fragment)
                                end
                              | moveVariables(context, nil, code_fragment) = (context, code_fragment)

                            fun findUnique accs (v :: vs) =
                                let
                                    fun cf v' = Variable.eq (v,v')
                                in
                                    if (List.exists cf accs) then
                                        findUnique accs vs
                                    else
                                        findUnique (v :: accs) vs
                                end
                              | findUnique accs (nil) = accs
                                                        
                            val vars_to_be_moved  = findUnique nil (map val_to_var vas)

                            fun findLowestPosition (context, (v::vs), lowest) =
                                let
                                    val v_position = #position(findInContext(context, v))
                                in
                                    if v_position < lowest then 
                                        findLowestPosition(context, vs, v_position)
                                    else
                                        findLowestPosition(context, vs, lowest)
                                end
                              | findLowestPosition (context, (nil), lowest) = lowest 

                            val warning_string = "WARNING WILL ROBINSON:"

                            val move_string = (if length(vas) > (ContextMap.contextLength context) 
                                               then warning_string 
                                               else "") ^ 
                                              "Made it to App step for " ^ f_as_string ^ "> " ^ 
                                              ":vars to be moved= " ^ vars_as_string(vars_to_be_moved) ^ 
                                              ":context len= " ^ Int.toString((ContextMap.contextLength context)) ^ 
                                              ":length vas= " ^ Int.toString(length(vas)) ^ 
                                              ":vas=  " ^ vas_as_string(vas)

                        in
                            (* if (findLowestPosition(context, vars_to_be_moved, ContextMap.contextLength(context)) > 
                                List.length(vas)) then
                                (context, nil)
                            else *)
                                let 
                                    val pad_context_by = if List.length(vas) > ContextMap.contextLength(context) then
                                                             List.length(vas) - ContextMap.contextLength(context)
                                                         else
                                                             0
                                    val initial_instructions = [COMMENT move_string]
                                in
                                    moveVariables(ContextMap.padContext(context, pad_context_by),
                                                  vars_to_be_moved, 
                                                  initial_instructions)
                                end
                        end

                    val (assign_context, assign_code) = 
                        let 
                            fun assignVariable(context, v, posn) = 
                                let 
                                    val context_info = findInContext(context, v)
                                    val code = UPDATE_STACK (W.fromInt(posn),
                                                             STACKVAR (W.fromInt(#position(context_info))))
                                in
                                    (#context(context_info), [code], posn+1)
                                end

                            fun assignVariables(context, (v::vs), posn, code_fragment) = 
                                let
                                    val (new_ctxt, new_code_fragment, next_posn) = assignVariable(context, v, posn)
                                in
                                    assignVariables(new_ctxt, vs, next_posn, code_fragment @ new_code_fragment)
                                end
                              | assignVariables(context, nil, _, code_fragment) = (context, code_fragment)

                            val move_context_comment = nil (* [COMMENT (ContextMap.toString move_context)] *)

                        in 
                            assignVariables(move_context,
                                            (map val_to_var vas),
                                            0,
                                            move_context_comment)
                        end

                    val jump_code = case f of Label lb => [GOTO_LABEL (LABEL_REF (Variable.tostring(lb)))]
                                            | Var v => [GOTO_ADDRESS (convertToValue context (Var v))]
                                            | _ => raise ToC "unexpected crap in APP call" 

                in
                    COMMENT ( "got to app" ) ::
                    movement_code @ assign_code @ 
                    jump_code
                end
                
              | convertBlock (context, Primop(PDynamic, [va], [v], [c])) =
                let
                in
		                raise ToC "XXX unimplemented: PDynamic"
                end

              | convertBlock (context, Primop(PDynamic, _, _, _)) = raise ToC "bad dynamic"

              | convertBlock (context, Sumswitch(Var obj, num, v, ics, def)) =
                (* decide if we are going to generate a jump table. *)
                let
                    val new_context_info = ContextMap.addToContext(context, v)
                    val new_context = #context(new_context_info)
                    val target = W.fromInt(#position(new_context_info))
                    val case_code_chunks =
                        let
                            fun transform (i, c) = (CONST (W.fromInt i), convertBlock(new_context, c))
                        in
                            map transform ics
                        end
                in
                    [ COMMENT ( "got to sumswitch" ),
                      UPDATE_STACK (target,
                                    TUPLEVAR (convertToHeapAddr context (Var obj), W.fromInt 0)),
                      SWITCH (extractTagWord context (Var obj),
                              case_code_chunks,
                              convertBlock(new_context, def)) ]
                end

              | convertBlock (context, Primop(PBind, [Var va], [v], [c])) =
                let
                    val va_context_info = ContextMap.findInContext(context, va)
                    val v_context_info = ContextMap.addToContext(context, v)
                in
                    COMMENT ( "got to bind" ) ::
                    UPDATE_STACK (W.fromInt(#position(v_context_info)),
                                  STACKVAR (W.fromInt(#position(va_context_info)))) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PBind, _, _, _)) = raise ToC "bad pbind"

              | convertBlock (context, Primop(PKill, vas, nil, [c])) =
                let 
                in
		                raise ToC "XXX unimplemented: PKill"
                end

              | convertBlock (context, Primop(PKill, _, _, _)) = raise ToC "bad pkill"

              | convertBlock (context, Primop(PGetc p, [], [v], [c])) =
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(v_context_info))
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                in
                    UPDATE_STACK (target,
                                  ALLOC_UNTRACED (GETC, context_len)) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PGetc p, _, _, _)) = raise ToC "bad getc"

              | convertBlock (context, Primop(PPutc p, [va], vs, [c])) =
                let
                    val v = convertToValue context va
                    val (context, is) = (handleVs context vs nil)
                in
                    (* DEBUG "fprintf(\"heap addr for putc is 0x%08x\\n\"," :: a :: DEBUG ")" :: SEPARATOR :: *)
                    PUTC v :: SEPARATOR ::
                    (is @ (convertBlock (context, c)))
                end

              | convertBlock (context, Primop(PPutc p, _, _, _)) = raise ToC "bad putc"

              | convertBlock (context, Primop(PAvail p, [], [v], [c])) =
                let 
                    val v_context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(v_context_info))
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                in
                    UPDATE_STACK (target,
                                  ALLOC_UNTRACED (AVAILC, context_len)) ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PAvail _, _, _, _)) = raise ToC "bad pavail"

              | convertBlock (context, Primop(PCompileWarn s, [], vs, [c])) =
                let 
                in
		                print ("Warning: " ^ s ^ "\n");

		                (* nullstacks' wb vs; *)
		                convertBlock (context, c)
                end

              | convertBlock (context, Primop(PCompileWarn _, _, _, _)) =
                raise ToC "bad compilewarn??"

              | convertBlock (context, Primop(PNull, [Var va], [], [cz, cnz])) =
                let
                    val code_for_zero = convertBlock(context, cz)
                    val code_for_nonzero = convertBlock(context, cnz)
                in
                    [ COMMENT ( "got to pnull" ),
                      IF (CMP_EQ (convertToValue context (Var va), CONST (W.fromInt 0)),
                          code_for_zero,
                          code_for_nonzero) ]
                end

              | convertBlock (context, Primop(PNull, _, _, _)) = raise ToC "bad null"

              | convertBlock (context, e as Primop(B (PCmp c), [vl, vr], [], [ct, cf])) =
                let
                    val vl_instructions = convertToValue context vl
                    val vr_instructions = convertToValue context vr
                    val comparison_instruction = case c of PEq => CMP_EQ
                                                         | PNeq => CMP_NEQ
                                                         | PLess => CMP_LESSTHAN
                                                         | PLesseq => CMP_LESSTHANEQ
                                                         | PGreater => CMP_GREATERTHAN
                                                         | PGreatereq => CMP_GREATERTHANEQ
                                                         (* SUSP should be unsigned? *)
                                                         | PBChk => CMP_GREATERTHANEQ
                in
                    [ COMMENT ( "got to binary comparison" ),
                      IF ( comparison_instruction (vl_instructions, vr_instructions),
                           convertBlock(context, ct),
                           convertBlock(context, cf) ) ]
                end

              | convertBlock (context, Primop(B (PCmp c), _, _, _)) = raise ToC "bad po cmp"
                                                                   
              | convertBlock (context, Primop(PNotb, [va], [v], [c])) =
                let 
                    val context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(context_info))
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                in
                    UPDATE_STACK (target,
                                  ALLOC_UNTRACED (NOT (convertToValue context va),
                                                  context_len)) ::
                    convertBlock(#context(context_info), c)
                end

              | convertBlock (context, Primop(PNotb, _, _, _)) = raise ToC "bad po not"
                                                              
              | convertBlock (context, Primop(B bop, [vl, vr], [v], [c])) =
                let 
                    fun convertOperation oper = case oper of PPlus => ADD
                                                           | PMinus => SUBTRACT
                                                           | PTimes => MULTIPLY
                                                           | PDiv => DIVIDE
                                                           | PSDiv => SDIVIDE
                                                           (* | PMod => MODULO *)
                                                           | PAndb => AND
                                                           | PXorb => XOR
                                                           | POrb => OR
                                                           | PShl => LSHIFT
                                                           | PShr => RSHIFT
                                                           | _ => raise ToC "Unknown primop" 
                    val v_context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(v_context_info))
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                    val r = (convertOperation bop) (convertToValue context vl,
                                                    convertToValue context vr)
                in
                    COMMENT ("got to binary operation") ::
                    UPDATE_STACK (target,
                                  ALLOC_UNTRACED ( r, context_len ) ) ::
                    convertBlock(#context(v_context_info), c)
                end
                
              | convertBlock (context, Primop(B bop, _, _, _)) =
                raise ToC "bad po bop"
                      
              | convertBlock (context, Project(n, Var va, v, c)) =
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(v_context_info))
                in
                    COMMENT ( "got to project" ) ::
                    UPDATE_STACK (target,
                                  TUPLEVAR (convertToHeapAddr context (Var va), W.fromInt n)) ::
                    convertBlock(#context(v_context_info), c)
                end
                
              | convertBlock (context, Alloc(STRING s, [], v, c)) =
                let
                    val chars = explode(s)
                    val string_len = length(chars)
                    val new_context_info = addToContext(context, v)
                    val target = W.fromInt(#position(new_context_info))
                    fun initializeChar posn total_len = [ STORE ( DEREFERENCE (ADD (STACKVAR target,
                                                                                    CONST (W.fromInt (total_len + 1 + (posn*2) + 1)))),
                                                                  CONST (W.fromInt (ord(List.nth(chars,posn)))) ) ]
                    fun initializeString acc limit = if (acc < limit) 
                                                     then (initializeChar acc limit) @ 
                                                          (initializeString (acc+1) limit)
                                                     else nil
                in
                    COMMENT ( "got to alloc STRING" ) ::
                    UPDATE_STACK (target,
                                  ALLOC_TRACED_STRING (CONST (W.fromInt string_len),
                                                       CONST (W.fromInt (ContextMap.contextLength context)))) ::
                    (initializeString 0 string_len) @
                    convertBlock(#context(new_context_info), c)
                end
                
              | convertBlock (context, Alloc(STRING _, _, _, _)) = raise ToC "bad string alloc"
                                                                
              (* allocate constant int *)
              | convertBlock (context, Alloc (INT, [Int i], v, c)) =
                let 
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                    val context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(context_info))
                in
                    COMMENT ( "got to alloc int" ) ::
                    UPDATE_STACK (target,
                                  ALLOC_UNTRACED (CONST i, context_len)) ::
                    convertBlock(#context(context_info), c)
                end
                
              | convertBlock (context, Alloc (INT, _, _, _)) = raise ToC "bad int alloc"
                                                            
              | convertBlock (context, Alloc (TUPLE 0, [], v, c)) =
                (* special case for empty tuples -- store 0 *)
                let
                    val new_context_info = ContextMap.addToContext(context, v)
                    val context_len = W.fromInt (ContextMap.contextLength context)
                    val target = W.fromInt(#position(new_context_info))
                in
                    COMMENT ( "got to alloc tuple0" ) ::
                    UPDATE_STACK (target, 
                                  ALLOC_TRACED_ARRAY (CONST (W.fromInt 0), 
                                                      CONST context_len)) ::
                    convertBlock(#context(new_context_info), c)
                end
                
              | convertBlock (context, Alloc (TUPLE n, vas, v, c)) =
                let 
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                    val new_context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(new_context_info))
                    fun oneVarInstructions(va, posn) = [ STORE (TUPLEVAR (STACKVAR (target), W.fromInt posn),
                                                                convertToHeapAddr context va) ]
                    val allVarInstructions = foldr (op @) [] (ListUtil.mapi oneVarInstructions vas)
                in
                    COMMENT ( "got to alloc tupleN" ) ::
                    UPDATE_STACK (target,
                                  ALLOC_TRACED_ARRAY (CONST (W.fromInt (n)),
                                                      context_len)) ::
                    (allVarInstructions @
                     convertBlock(#context(new_context_info), c))
                end
                
              | convertBlock (context, Alloc (CODE, [Label l], v, c)) =
                let 
                    val context_len = CONST (W.fromInt (ContextMap.contextLength context))
                    val context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(context_info))
                in
                    COMMENT ( "got to alloc code" ) ::
                    UPDATE_STACK (target,
                                  ALLOC_UNTRACED (LABEL_AS_VALUE (LABEL_REF (Variable.tostring(l))),
                                                  context_len)) ::
                    convertBlock(#context(context_info), c)
                end
                
              | convertBlock (context, Alloc (CODE, _, v, c)) = raise ToC "bad code alloc"

              | convertBlock (context, Alloc (INT_T t, [vas], v, c)) =
                let
                    val context_len = W.fromInt (ContextMap.contextLength context)
                    val first_va = val_to_var vas;
                    val context_info = ContextMap.findInContext(context, first_va)
                    val new_context_info = ContextMap.addToContext(context, v)
                    val target = W.fromInt(#position(new_context_info))
                in
                    COMMENT ( "got to alloc int_t vas" ) ::
                    UPDATE_STACK (target,
                                  ALLOC_TAGGED (CONST (W.fromInt t),
                                                CONST context_len)) ::
                    STORE (TUPLEVAR (STACKVAR target, W.fromInt 0),
                           STACKVAR (W.fromInt(#position(context_info)))) ::
                    convertBlock(#context(new_context_info), c)
                end
                    
              | convertBlock (context, Alloc (INT_T t, nil, v, c)) =
                let
                    val new_context_info = ContextMap.addToContext(context, v)
                    val context_len = W.fromInt (ContextMap.contextLength context)
                    val target = W.fromInt(#position(new_context_info))
                in
                    COMMENT ( "got to alloc int_t nil" ) ::
                    UPDATE_STACK (target,
                                  ALLOC_TAGGED (CONST (W.fromInt t),
                                                CONST context_len)) ::
                    STORE (TUPLEVAR (STACKVAR target, W.fromInt 0),
                           CONST (W.fromInt 0)) ::
                    convertBlock(#context(new_context_info), c)
                end

              | convertBlock (context, c) = 
                let in
                    print "Not implemented:\n";
                    CPSPrint.printe c;
                    print "\n";
                    raise ToC "CPS instruction not implemented in toc"
                end

            and addBlock lab block = blockset := (lab, block) :: !blockset
                                     
            (* note the context is a list of variable records. *)
            val initialContext = initializeContext(nil, vars)
            val _ = dprint("initial context is: " ^ ContextMap.toString(initialContext) ^ "\n")
                    
            val firstblock = COMMENT (" initial context is: " ^ ContextMap.toString(initialContext) ^ "") ::
                             convertBlock(initialContext, ce)
        in
            addBlock lab firstblock;
            !blockset
        end
        
    val code = map convertFunction fns
               
in
    ( List.concat code,
	    main )
end

  | convert _ = raise ToC "cps in wrong form"

end


