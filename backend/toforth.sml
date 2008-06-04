(* convert from allocation-converted CPS language to Forth *)

structure ToForth = 
struct

open CPS
open Forth
open Primop
open ContextMap
structure W = Word32
structure V = Variable

exception ToForth of string

val debugopt = Params.flag false
                           (SOME ("-debugtoforth", 
                                  "Debug the Forth backend")) "debugtoforth"

fun convert (Fix(fns, App(Label main, nil))) = 
    let

      fun debugdo f = if !debugopt then f () else ()
      fun dprint s = if !debugopt then print ("[TOFORTH] " ^ s ^ "\n") else ()
      fun dprintd f = if !debugopt then print ("[TOFORTH] " ^ f() ^ "\n") else ()

      val SCIP_STATUS_LOC = 0wx100002
      val SCIP_TESTPOINT_LOC = 0wx10000A
      val SCIP_COUNTER_LOC = 0wx100008
      val HEAPtagmask = 0x1fffffff

      val bytes_per_word = 4

      fun convertFunction (lab, vars, ce) =
          let
              val blockset = ref nil

              (* we basically want to end up with the value of the
                 variable on the stack. so, to start, we'll just look
                 up the variable in the mapping to find out which
                 forthvar it is in.  then, we'll dereference this
                 forthvar once to get the address of the heap node it
                 points to, then we'll dereference the heap node to
                 get the integer.*)

              fun dereferenceVariableOntoStack context var = 
                  let
                      val ci = ContextMap.findInContext(context, var)
                  in
                      [ VARIABLE_REF (W.fromInt(#position(ci))), DEREFERENCE,
                      DEBUG "checkVarSanity"]
                  end

              fun convertToHeapAddr c a = case a of Label _ => raise ToForth "label?"
                                                  | Var vble => dereferenceVariableOntoStack c vble
                                                  | Int i => raise ToForth "int?"

              fun convertToValue c a = case a of Label _ => raise ToForth "label?"
                                               | Var vble => ((dereferenceVariableOntoStack c vble) @ 
                                                              [ CONST (W.fromInt bytes_per_word),
                                                                ADD,
                                                                DEREFERENCE ])
                                               | Int i => [ CONST i ]


              fun extractTagWord c a = case a of Label _ => raise ToForth "label? in extract header word?"
                                               | Var vble => ((dereferenceVariableOntoStack c vble) @ 
                                                              [ DEREFERENCE,
                                                                CONST (W.fromInt HEAPtagmask),  (* SUSP: secret knowledge of heap header flags *)
                                                                AND ])
                                               | Int i => raise ToForth "integer? in extract header word?"

              fun val_to_var v = case v of Var vbl => vbl
                                         | _ => raise ToForth "unexpected parameter in val_to_var" 

              fun va_as_string v = case v of Var va => " " ^ ((V.tostring o val_to_var) v) ^ " "
                                           | Int i => " " ^ W.toString(i) ^ " "
                                           | _ => raise ToForth "unexpected parameter in va_as_string" 

              fun vas_as_string vas = concat(map va_as_string vas)

              fun vas_and_posns_as_string context (v::vs) =
                  ((va_as_string v) ^ "at posn=" ^ Int.toString(#position(ContextMap.findInContext(context, (val_to_var v))))
                   ^ " " ^ (vas_and_posns_as_string context vs))
                | vas_and_posns_as_string context nil = ""

              fun vars_as_string (v::vs) = 
                  V.tostring(v) ^ " " ^ vars_as_string(vs)
                | vars_as_string (nil) = ""

              fun handleV c v is =
                  let
                      val new_context_info = addToContext(c, v);
                  in
                      (#context(new_context_info),
                       (is @ [CONST (W.fromInt 0),
                              VARIABLE_REF (W.fromInt (#position(new_context_info))),
                              STORE]))
                  end

              fun handleVs c (v::vs) is =
                  let
                      val (new_ctx, new_instrs) = (handleV c v is);
                  in
                      handleVs new_ctx vs new_instrs
                  end
                | handleVs c (nil) is = (c, is)

             
              fun convertBlock (context, Fix _) = raise ToForth "convertBlock: Fix" 
                | convertBlock (context, Deferred _) = raise ToForth  "convertBlock: Deferred"
                                                    
                (* might have continuations or returns if it was used
                 as a primop and we didn't optimize them out yet... *)
                | convertBlock (context, Primop(PHalt, [], _, _)) = 
                  let 
                in
                    dprint "Made it to phalt primop step\n";
                    []
                end
                
              | convertBlock (context, Primop(PHalt, _, _, _)) = raise ToForth "bad PHalt"
                                                              
              | convertBlock (context, Primop(PSet, [Var re, Var va], vs, [c])) =
                let
                    val (context, is) = (handleVs context vs nil)
                in
                    dprint "Made it to pset primop step\n";
                    COMMENT ("PSet: updating reference " ^ 
                             V.tostring(re) ^ " to " ^ V.tostring(va) ^ "") ::
                    (convertToHeapAddr context (Var va)) @
                    (dereferenceVariableOntoStack context re) @
                    (CONST (W.fromInt (bytes_per_word)) :: 
                     ADD ::
                     STORE ::
                     is @
                     convertBlock(context, c))
                end
                
              | convertBlock (context, Primop(PSet, _, _, _)) = raise ToForth "bad setref"

              | convertBlock (context, Primop(PGet, [Var va], [v], [c])) =
                let
                    val v_context_info = addToContext(context, v)
                in
                    dprint "Made it to pget primop step\n";
                    COMMENT ("PGet: " ^ V.tostring(v) ^ 
                             " <- reference target of " ^ 
                             V.tostring(va) ^ "") ::
                    (convertToValue context (Var va)) @
                    (VARIABLE_REF (W.fromInt(#position(v_context_info))) ::
                     STORE ::
                     convertBlock(#context(v_context_info), c))
                end

              | convertBlock (context, Primop(PGet, _, _, _)) = raise ToForth "bad getref"

              | convertBlock (context, Primop(PRef, [Var va], [v], [c])) =
                let 
                    val v_context_info = addToContext(context, v)
                in
		              dprint "Made it to pref primop step\n";
                  dprint ("PRef: " ^ V.tostring(v) ^ " <- " ^ V.tostring(va) ^ " reference\n");
                  [ COMMENT ("PRef: " ^ V.tostring(v) ^ " <- " ^ V.tostring(va) ^ " reference"),
                    CONST (W.fromInt (ContextMap.contextLength context)),
                    CONST (W.fromInt 1),
                    ALLOC_TRACED_ARRAY ] @
                  [ DUP, 
                    VARIABLE_REF (W.fromInt(#position(v_context_info))),
                    STORE ] @
                  (convertToHeapAddr context (Var va)) @
                  [ SWAP,
                    CONST (W.fromInt bytes_per_word),
                    ADD,
                    STORE ] @
                  convertBlock(#context(v_context_info), c)
                end
                
              | convertBlock (context, Primop(PRef, _, _, _)) = raise ToForth "bad newref"

              | convertBlock (context, Primop(PNewtag, [], [v], [c])) =
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to pnewtag primop step\n";
                    COMMENT ("newtag: " ^ V.tostring(v) ^ " <- new integer") ::
                    CONST (W.fromInt (ContextMap.contextLength context)) ::
                    NEW_TAG_REF :: DEREFERENCE ::
                    ONEPLUS ::
                    DUP ::
                    NEW_TAG_REF :: STORE ::
                    ALLOC_UNTRACED ::
                    VARIABLE_REF (W.fromInt(#position(v_context_info))) ::
                    STORE ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PGethandler, [], [v], [c])) =
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to pgethandler primop step\n";
                    COMMENT ("gethandler: " ^ V.tostring(v) ^ " <- value of exception handler") ::
                    EXCEPTION_HANDLER_REF :: DEREFERENCE :: 
                    VARIABLE_REF (W.fromInt(#position(v_context_info))) ::
                    STORE ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PGethandler, _, _, _)) = raise ToForth "bad gethandler"

              | convertBlock (context, Primop(PSethandler, [Var va], vs, [c])) =
                let
                    val va_context_info = ContextMap.findInContext(context, va)
                    val (context, is) = (handleVs context vs nil)
                in
                    dprint "Made it to psethandler primop step\n";
                    COMMENT ("sethandler: exception_handler " ^ " <- " ^ V.tostring(va) ^ "" ) ::

                    (* SUSP why not convertToHeapAddr *)
                    VARIABLE_REF (W.fromInt(#position(va_context_info))) ::
                    DEREFERENCE ::
                    EXCEPTION_HANDLER_REF ::
                    STORE ::
                    is @
                    convertBlock(context, c)
                end

              | convertBlock (context, Primop(PSethandler, _, _, _)) = raise ToForth "bad sethandler"

              | convertBlock (context, Primop(PArray, [vlen, Var vinit], [v], [c])) =
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to parray primop step\n";
                    [ COMMENT ("parray: " ^ V.tostring(v) ^ 
                               " <- array containing some elements " ^ 
                               " of " ^ V.tostring(vinit) ^ ""),
                      CONST (W.fromInt (ContextMap.contextLength context)) ] @
                    (convertToValue context vlen) @
                    [ ALLOC_TRACED_ARRAY,
                      DUP,
                      VARIABLE_REF (W.fromInt(#position(v_context_info))),
                      STORE,
                      CONST (W.fromInt bytes_per_word),
                      ADD ] @
                    (dereferenceVariableOntoStack context vinit) @ 
                    [ SWAP ] @
                    (convertToValue context vlen) @
                    [ SET ] @
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PArray, _, _, _)) = raise ToForth "bad array"

              | convertBlock (context, Primop(PArray0, [], [v], [c])) =
                let 
                    val new_context_info = addToContext(context, v)
                in
                    dprint "Made it to parray0 primop step\n";
                    COMMENT ("parray0: " ^ V.tostring(v) ^ " <- zero array") ::
                    CONST (W.fromInt (ContextMap.contextLength context)) ::
                    CONST (W.fromInt 0) :: 
                    ALLOC_TRACED_ARRAY :: 
                    VARIABLE_REF (W.fromInt(#position(new_context_info))) :: 
                    STORE ::
                    convertBlock(#context(new_context_info), c)
                end

              | convertBlock (context, Primop(PArray0, _, _, _)) = raise ToForth "bad array0"

              (* array concat, assuming same type (typically char) *)
              | convertBlock (context, Primop(PJointext, arrs, [v], [c])) =
                let 
                    (* 
                     1) put out a zero
                     2) put out the length of the first array and then add
                     3) put out the length of the next array and add
                     4) ...
                     5)  call allocate \ new_length d
                     6) swap drop dup \ d d 
                     7) do loop stuff
                     *)

                    fun find_length a = (extractTagWord context a)

                    fun generate_length_code a = (find_length a) @
                                                 [ADD]
                                                 
                    fun generate_copy_code a = DUP :: (* addr+4 addr+4 *)
                                               (find_length a) @ (* addr+4 addr+4 length *)
                                               (CONST (W.fromInt bytes_per_word) :: (* addr+4 addr+4 length 4 *)
                                                MULTIPLY :: (* addr+4 addr+4 length_in_bytes *)
                                                ADD :: (* addr+4 addr+4+length_in_bytes *)
                                                [SWAP]) @ (* addr+4+length_in_bytes addr+4 *)
                                               (convertToHeapAddr context a) @ (* addr+4+length_in_bytes addr+4 source_addr *)
                                               (CONST (W.fromInt bytes_per_word) :: [ADD]) @ (* addr+4+length_in_bytes addr+4 source_addr+4 *)
                                               [SWAP] @ (* addr+4+length_in_bytes source_addr+4 addr+4 *)
                                               (find_length a) @ 
                                               [COPY]

                    val generate_lengths = List.concat (map generate_length_code arrs)
                    val generate_copies = List.concat (map generate_copy_code arrs)
                    val new_context_info = addToContext(context, v)
                in
                    dprint "Made it to pjointext primop step\n";
                    COMMENT ("Pjointext: " ^ V.tostring(v) ^ " <- " ^ " multiple arrays") ::
                    CONST (W.fromInt (ContextMap.contextLength context)) ::
                    CONST (W.fromInt 0) ::
                    generate_lengths @
                    [ ALLOC_TRACED_ARRAY, 
                      DUP,
                      VARIABLE_REF (W.fromInt(#position(new_context_info))),
                      STORE,
                      CONST (W.fromInt bytes_per_word),
                      ADD ] @
                    generate_copies @
                    ( DROP :: 
                      convertBlock(#context(new_context_info), c) )
                end

              | convertBlock (context, Primop(PJointext, _, _, _)) = raise ToForth "bad jointext"

              (* unsafe sub *)
              (* PERF special case const offset *)
              | convertBlock (context, Primop(PSub, [Var varr, voff], [v], [c])) =
                let 
                    val new_context_info = addToContext(context, v)
                    val offset_code = case voff of Int i => [CONST (W.fromInt((W.toInt(i)+1)*bytes_per_word))] 
                                                 | Var v => (convertToValue context (Var v)) @ 
                                                            [ ONEPLUS,
                                                              CONST (W.fromInt bytes_per_word),
                                                              MULTIPLY ]
                                                 | _ => raise ToForth "invalid voff in PSub"
                in
                    dprint "Made it to psub primop step\n";
                    [ COMMENT ("psub: " ^ V.tostring(v) ^ " <- " ^ V.tostring(varr) ^ "[" ^ va_as_string(voff) ^ "]") ] @
                    (convertToHeapAddr context (Var varr)) @
                    offset_code @
                    (ADD ::
                     DEREFERENCE ::
                     VARIABLE_REF (W.fromInt(#position(new_context_info))) ::
                     STORE :: 
                     convertBlock(#context(new_context_info), c))
                end

              | convertBlock (context, Primop(PSub, _, _, _)) = raise ToForth "bad array sub"

              (* unsafe update with constant offset *)
              | convertBlock (context, Primop(PUpdate, [Var varr, Int off, Var velem], vs, [c])) =
                let 
                    val offset = W.toInt(off)
                    val (context, is) = (handleVs context vs nil)
                in
                    dprint "Made it to pupdate (const) primop step\n";
                    [ COMMENT ("PUpdate: " ^ V.tostring(varr) ^ "[" ^ Int.toString(offset) ^ "]" ^ " <- " ^  V.tostring(velem) ^ "") ] @
                    (convertToHeapAddr context (Var velem)) @
                    (convertToHeapAddr context (Var varr)) @
                    (CONST (W.fromInt((offset+1)*bytes_per_word)) ::
                     ADD ::
                     STORE ::
                     is @
                     convertBlock(context, c))
                end

              | convertBlock (context, Primop(PUpdate, [Var varr, voff, Var velem], vs, [c])) =
                let 
                    val (context, is) = (handleVs context vs nil)
                in
                    dprint "Made it to pupdate (non-const) primop step\n";
                    [ COMMENT ("PUpdate: " ^ V.tostring(varr) ^ "[" ^ va_as_string(voff) ^ "]" ^ " <- " ^  V.tostring(velem) ^ "") ] @
                    (convertToHeapAddr context (Var velem)) @
                    (convertToHeapAddr context (Var varr)) @
                    (convertToValue context voff) @
                    ( ONEPLUS ::
                      CONST (W.fromInt bytes_per_word) ::
                      MULTIPLY ::
                      ADD ::
                      STORE ::
                      is @
                      convertBlock(context, c) )
                end

              | convertBlock (context, Primop(PUpdate, _, _, _)) = raise ToForth "bad array upd"

              | convertBlock (context, Primop(PArraylength, [Var varr], [v], [c])) =
                let 
                    val new_context_info = ContextMap.addToContext(context, v)
                in 
                    dprint "Made it to parraylength primop step\n";
                    [ COMMENT ("PArraylength: " ^ V.tostring(v) ^ " <- length of " ^ V.tostring(varr) ^ " array"),
                      CONST (W.fromInt (ContextMap.contextLength context)) ] @
                    (extractTagWord context (Var varr)) @
                    ( ALLOC_UNTRACED ::
                      VARIABLE_REF (W.fromInt(#position(new_context_info))) ::
                      STORE ::
                      convertBlock(#context(new_context_info), c) )
                end

              | convertBlock (context, Primop(PArraylength, _, _, _)) = raise ToForth "bad array len"

              | convertBlock (context, Primop(PNative f, [args], [v], [c])) = 
                let
                    val v_context_info = ContextMap.addToContext(context, v)
                in
                    dprint ("toforth: Made it to pnative primop step\n");
                    [ COMMENT ("native: " ^ f),
                      CONST (W.fromInt (ContextMap.contextLength context)) ] @
                      (convertToHeapAddr context args) @
                    ( (NATIVE_CALL f) :: 
                      VARIABLE_REF (W.fromInt(#position(v_context_info))) ::
                      STORE ::
                      convertBlock(#context(v_context_info), c) )
                end

              | convertBlock (context, App(f, vas)) =
                let
                    val f_as_string = case f of Label lb => V.tostring(lb)
                                              | Var v => (va_as_string (Var v))
                                              | _ => raise ToForth "unexpected crap in APP call to f_as_string" 

                    (* only move variables if they would get stomped on otherwise *)
                    val (move_context, movement_code) = 
                        let 
                            fun moveOneVariable(context, v) = 
                                let 
                                    val context_info = findInContext(context, v)
                                    val source = [ VARIABLE_REF (W.fromInt(#position(context_info))), 
                                                   DEREFERENCE ]
                                    val new_context_info = findInContext(rebindOneVar(context, v), v)
                                    val () = dprint ("moving " ^ V.tostring(v) ^ 
                                                     " from position " ^ Int.toString(#position(context_info)) ^ 
                                                     " to position " ^ Int.toString(#position(new_context_info)) ^ "\n" )
                                    val context_string = ContextMap.toString(#context(context_info))
                                    val new_context_string = ContextMap.toString(#context(new_context_info))
                                    val destination = [ VARIABLE_REF (W.fromInt(#position(new_context_info))),
                                                        STORE,
                                                        (* COMMENT "before:",
                                                        COMMENT context_string,
                                                        COMMENT "after:",
                                                        COMMENT new_context_string, *)
                                                        NEWLINE ]
                                in
                                    (#context(new_context_info), source @ destination)
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
                                    fun cf v' = V.eq (v,v')
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
                            if (findLowestPosition(context, vars_to_be_moved, ContextMap.contextLength(context)) > 
                                List.length(vas)) 
                            then
                                (* no need to move anything if all of the variables to move are above the required spots *)
                                (context, nil) 
                            else
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
                                    val source = [ VARIABLE_REF (W.fromInt(#position(context_info))), 
                                                   DEREFERENCE ]
                                    val destination = [ VARIABLE_REF (W.fromInt(posn)),
                                                        STORE, 
                                                        COMMENT ("{" ^ V.tostring(v) ^ " ~> " ^ Int.toString(posn) ^ "}") ]
                                in
                                    (#context(context_info), source @ destination, posn+1)
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

                    val jump_setup_code = case f of Label lb => [(LABEL_REF (V.tostring(lb))), DEREFERENCE]
                                                  | Var v => (convertToValue context (Var v))
                                                  | _ => raise ToForth "unexpected crap in APP call" 

                in
                    dprint "Made it to App step\n";
                    COMMENT ("APP: " ^ "")  :: 
                    movement_code @ assign_code @ 
                    jump_setup_code @ [EXECUTE]
                end

              | convertBlock (context, Primop(PDynamic, _, _, _)) =
                let
                in
                  raise ToForth "PDynamic unsupported in Forth backend"
                end

              | convertBlock (context, Sumswitch(Var obj, num, v, ics, def)) =
                (* ENH: decide if we are going to generate a jump table. *)
                let
                    val new_context_info = ContextMap.addToContext(context, v)
                    val new_context = #context(new_context_info)
                    fun alternative(i, c) = 
                        let
                          val br_lab = V.namedvar ("SW" ^ (V.tostring lab))
                          val br_code = convertBlock(new_context, c)
                          val () = addBlock br_lab br_code
                        in
                          [CONST (W.fromInt i),
                           OF,
                           LABEL_REF (V.tostring br_lab),
                           DEREFERENCE,
                           EXECUTE,
                           ENDOF]
                        end
                    val alternatives = foldr (op @) [] (map alternative ics) 
                    val code_for_default_case = convertBlock(new_context, def)
                    val warning_string = if List.length(ics) > 100 then
                                             " GOT A LARGET SUMSWITCH "
                                         else
                                             ""    
                in
                    dprint "Made it to sumswitch step\n";
                    [ COMMENT ("sumswitch: " ^ warning_string ^ V.tostring(v) ^ " , " ^ V.tostring(obj) ^ " , " ^ Int.toString(num) ^ "") ] @
                    (convertToHeapAddr context (Var obj)) @
                    (* get "value" out of obj and assign new variable to it (don't alloc) *)
                    [ DEBUG " dup @ e0000000 and 20000000 <> if dup u. s\" whoah nelly \" type then",
                      CONST (W.fromInt(bytes_per_word)),
                      ADD,
                      DEREFERENCE,
                      VARIABLE_REF (W.fromInt(#position(new_context_info))),
                      STORE ] @
                    (* get "tag" out of obj *)
                    (extractTagWord context (Var obj)) @
                    CASE :: 
                    (* case on this tag *)
                    alternatives @ 
                    (* i1 of *)
                    (* cs1 *)
                    (* endof *)
                    (* i2 of *)
                    (* cs2 *)
                    (* endof *)
                    (* ... *)
                    (* default code here *)
                    (* SUSP Add a drop because the default case seems to leave
                     an extra value on the data stack *)
                    [ DROP ] @
                    code_for_default_case @
                    (* end case *)
                    [ENDCASE]
                end

              | convertBlock (context, Primop(PBind, [Var va], [v], [c])) =
                let
                    val va_context_info = ContextMap.findInContext(context, va)
                    val v_context_info = ContextMap.addToContext(context, v)
                in
                    dprint ("toforth: Made it to pbind primop step for " ^ 
                           V.tostring(v) ^ 
                           " <- " ^ 
                           V.tostring(va) ^
                           "\n");
                    COMMENT ("bind: " ^ V.tostring(v) ^ " <- " ^ V.tostring(va) ^ "")  :: 
                    VARIABLE_REF (W.fromInt(#position(va_context_info))) :: 
                    DEREFERENCE :: 
                    VARIABLE_REF (W.fromInt(#position(v_context_info))) ::
                    STORE ::
                    convertBlock(#context(v_context_info), c)
                end

              | convertBlock (context, Primop(PBind, _, _, _)) = raise ToForth "bad pbind"

              | convertBlock (context, Primop(PKill, vas, nil, [c])) =
                let 
                in
                  (* SUSP if we ever use PKill we should implement it, but it
                    is safe to treat it as a no-op. *)
                  convertBlock (context, c)
                end

              | convertBlock (context, Primop(PKill, _, _, _)) = raise ToForth "bad pkill"

              | convertBlock (context, Primop(PGetc p, [], [v], [c])) =
                let
                    val read = case p of Console => [ READ_C0,
                                                      DUP, WRITE_C0,
                                                      DUP, CONST (W.fromInt 13),
                                                      CMP_EQ,
                                                      IF,
                                                      DROP, CONST (W.fromInt 10),
                                                      THEN ]
                                       | Serial0 => [ READ_S0 ]
                    val v_context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to pgetc primop step\n";
                    [ COMMENT ("pgetc: " ^ V.tostring(v) ^ " <-"),
                      CONST (W.fromInt (ContextMap.contextLength context)) ] @
                    read @
                    [ ALLOC_UNTRACED,
                      VARIABLE_REF (W.fromInt(#position(v_context_info))),
                      STORE ] @
                    convertBlock(#context(v_context_info), c)
                end
              | convertBlock (context, Primop(PGetc _, _, _, _)) = raise ToForth "bad getc"

              | convertBlock (context, Primop(PAvail p, [], [v], [c])) =
                let 
                  val v_context_info = ContextMap.addToContext(context, v)
                  val instr = case p of 
                                  Serial0 => AVAIL_S0
                                | Console => AVAIL_C0
                in
                  [ COMMENT ("pavail: " ^ V.tostring v ^ " <-"),
                    CONST (W.fromInt (ContextMap.contextLength context)) ] @
                  [ instr,
                    ALLOC_UNTRACED,
                    VARIABLE_REF (W.fromInt(#position(v_context_info))),
                    STORE ]
                  @ convertBlock(#context(v_context_info), c)
                end
              | convertBlock (context, Primop(PAvail _, _, _, _)) = raise ToForth "bad pavail"

              | convertBlock (context, Primop(PPutc p, [va], vs, [c])) =
                let
                    val cmt = case va of Label _ => raise ToForth "label in putc?"
                                       | Var vble => COMMENT ("putc: " ^ V.tostring(vble) ^ " ->")
                                       | Int i => COMMENT ("putc: " ^ W.toString(i) ^ " ->")
                    val write = case p of Console => WRITE_C0
                                        | Serial0 => WRITE_S0
                    val (context, is) = (handleVs context vs nil)
                in
                    dprint "Made it to pputc primop step\n";
                    cmt ::
                    (convertToValue context va) @
                    (write ::
                     is @
                    convertBlock(context, c))
                end

              | convertBlock (context, Primop(PPutc _, _, _, _)) = raise ToForth "bad putc"

              | convertBlock (context, Primop(PCompileWarn s, [], vs, [c])) =
                let
                  val (context, is) = (handleVs context vs nil)
                in
                  dprint ("Warning: " ^ s ^ "\n");
                  is @
                  convertBlock(context, c)
                end

              | convertBlock (context, Primop(PCompileWarn _, _, _, _)) =
                raise ToForth "bad compilewarn??"

              | convertBlock (context, Primop(PNull, [Var va], [], [cz, cnz])) =
                let
                    val code_for_zero = convertBlock(context, cz)
                    val zero_lab = V.namedvar ("ZE" ^ (V.tostring lab))
                    val () = addBlock zero_lab code_for_zero
                    val code_for_nonzero = convertBlock(context, cnz)
                    val nonzero_lab = V.namedvar ("NZ" ^ (V.tostring lab))
                    val () = addBlock nonzero_lab code_for_nonzero
                in
                    dprint "Made it to pnull primop step\n";
                    [COMMENT ("pnull: " ^ V.tostring(va) ^ "")] @
                    (convertToValue context (Var va)) @
                    [ CONST (W.fromInt 0),
                      (* SUSP: is there a stack problem here when an execute branches completely out of the IF? *)
                      CMP_EQ, 
                      IF,
                      LABEL_REF (V.tostring zero_lab),
                      DEREFERENCE,
                      EXECUTE,
                      ELSE,
                      LABEL_REF (V.tostring nonzero_lab),
                      DEREFERENCE,
                      EXECUTE,
                      THEN]
                end

              | convertBlock (context, Primop(PNull, _, _, _)) = raise ToForth "bad null"

              | convertBlock (context, e as Primop(B (PCmp c), [vl, vr], [], [ct, cf])) =
                let
                    (* PERF "compare to 0" can be special cased *)
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
                    val code_for_true = convertBlock(context, ct)
                    val true_lab = V.namedvar ("TR" ^ (V.tostring lab))
                    val () = addBlock true_lab code_for_true

                    val code_for_false = convertBlock(context, cf)
                    val false_lab = V.namedvar ("FL" ^ (V.tostring lab))
                    val () = addBlock false_lab code_for_false
                in
                    dprint "Made it to binary compare primop step\n";
                    COMMENT ("pcmp: " ^ (va_as_string vl) ^ " @ " ^ (va_as_string vr) ^ "") ::
                    vl_instructions @
                    vr_instructions @
                    (* SUSP: is there a stack problem here when an execute branches completely out of the IF? *)
                    [comparison_instruction, 
                     IF,
                     LABEL_REF (V.tostring true_lab),
                     DEREFERENCE,
                     EXECUTE,
                     ELSE,
                     LABEL_REF (V.tostring false_lab),
                     DEREFERENCE,
                     EXECUTE,
                     THEN]
                end

              | convertBlock (context, Primop(B (PCmp c), _, _, _)) = raise ToForth "bad po cmp"

              | convertBlock (context, Primop(PNotb, [va], [v], [c])) =
                let 
                    val context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to pnotb primop step\n";
                    COMMENT ("pnotb operator: ") :: 
                    [CONST (W.fromInt (ContextMap.contextLength context))] @
                    (convertToValue context va) @
                    (INVERT ::
                     ALLOC_UNTRACED ::
                     VARIABLE_REF (W.fromInt(#position(context_info))) ::
                     STORE ::
                     convertBlock(#context(context_info), c))
                end

              | convertBlock (context, Primop(PNotb, _, _, _)) = raise ToForth "bad po not"

              | convertBlock (context, Primop(B bop, [vl, vr], [v], [c])) =
                let 
                    val context_info = ContextMap.addToContext(context, v)

                    fun convertOperation oper = case oper of PPlus => [ ADD ]
                                                           | PMinus => [ SUBTRACT ]
                                                           | PTimes => [ MULTIPLY ]
                                                           | PDiv => [ SWAP,
                                                                       (* extend to double *)
                                                                       CONST (W.fromInt 0),
                                                                       ROT,
                                                                       UDIVMOD,
                                                                       SWAP,
                                                                       DROP ]
                                                           | PSDiv => [ DIVIDE ]
                                                           (* PERF: | PMod => [ MOD ] *)
                                                           | PAndb => [ AND ]
                                                           | PXorb => [ XOR ]
                                                           | POrb => [ OR ]
                                                           | PShl => [ LSHIFT ]
                                                           | PShr => [ RSHIFT ]
                                                           | _ => raise ToForth "Unknown primop" 
                in
                    dprint "Made it to binary operation primop step\n";
                    COMMENT ("binary op: ") :: 
                    [ CONST (W.fromInt (ContextMap.contextLength context)) ] @
                    (convertToValue context vl) @
                    (convertToValue context vr) @ 
                    (convertOperation bop) @

                    ( ALLOC_UNTRACED ::
                      VARIABLE_REF (W.fromInt(#position(context_info))) ::
                      STORE ::
                      convertBlock(#context(context_info), c) )
                end

              | convertBlock (context, Primop(B bop, _, _, _)) =
                raise ToForth "bad po bop"

              | convertBlock (context, Primop(PSetStatus, [v], vs, [c])) =
                let
                    val (context, is) = (handleVs context vs nil)
                in
                  (convertToValue context v) @
                  [CONST SCIP_STATUS_LOC,
                   STORE16]
                  @ is
                  @ convertBlock (context, c)
                end

              | convertBlock (context, Primop(PSetStatus, _, _, _)) =
                raise ToForth "bad po setstatus"

              | convertBlock (context, Primop(PSetTestpoint, [v], vs, [c])) =
                let
                    val (context, is) = (handleVs context vs nil)
                in
                  (convertToValue context v) @
                  [CONST SCIP_TESTPOINT_LOC,
                   STORE16]
                  @ is
                  @ convertBlock (context, c)
                end

              | convertBlock (context, Primop(PSetTestpoint, _, _, _)) =
                raise ToForth "bad po testpoint"

              | convertBlock (context, Primop(PSetCounter, [v], vs, [c])) =
                let
                    val (context, is) = (handleVs context vs nil)
                in
                  (convertToValue context v) @
                  [CONST SCIP_COUNTER_LOC,
                   STORE16]
                  @ is
                  @ convertBlock (context, c)
                end

              | convertBlock (context, Primop(PSetCounter, _, _, _)) =
                raise ToForth "bad po setcounter"

              | convertBlock (context, Primop(PFromSeconds, [v], [va], [c])) =
                let
                  val context_info = ContextMap.addToContext(context, va)
                in
                  [CONST (W.fromInt (ContextMap.contextLength context))] @
                  (convertToValue context v) @
                  [ SECONDS,
                    ALLOC_UNTRACED,
                    VARIABLE_REF (W.fromInt(#position(context_info))),
                    STORE ]
                  @ convertBlock (#context(context_info), c)
                end

              | convertBlock (context, Primop(PFromSeconds, _, _, _)) =
                raise ToForth "bad po fromseconds"

              | convertBlock (context, Primop(PSleep, [v], vs, [c])) =
                let
                    val (context, is) = (handleVs context vs nil)
                in
                  (convertToValue context v) @
                  [SLEEP]
                  @ is
                  @ convertBlock (context, c)
                end

              | convertBlock (context, Primop(PSleep, _, _, _)) =
                raise ToForth "bad po sleep"

              | convertBlock (context, Project(n, Var va, v, c)) =
                let
                    val new_context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to project step\n";
                    [ COMMENT ("project: " ^ 
                               V.tostring(v) ^ " <-" ^ " element " ^ Int.toString(n) ^ " of " ^ V.tostring(va) ^ 
                               "(pos=" ^ Int.toString(#position(ContextMap.findInContext(context, va))) ^ ")") ] @
                    (convertToHeapAddr context (Var va)) @ 
                    [ CONST (W.fromInt((n+1) * bytes_per_word)),
                      ADD,
                      DEREFERENCE,
                      VARIABLE_REF (W.fromInt(#position(new_context_info))),
                      STORE ] @
                    convertBlock(#context(new_context_info), c)
                end

              | convertBlock (context, Alloc(STRING s, [], v, c)) =
                let
                    val chars = explode(s)
                    val string_len = length(chars)
                    val new_context_info = addToContext(context, v)

                    fun initializeChar posn total_len = [ NEWLINE,
                                                          DUP,
                                                          CONST (W.fromInt ((total_len + 1 + (posn*2) + 1) * 
                                                                            bytes_per_word)),
                                                          ADD,
                                                          CONST (W.fromInt (ord(List.nth(chars,posn)))),
                                                          SWAP, 
                                                          STORE,
                                                          NEWLINE ] 
                    fun initializeString acc limit = if (acc < limit) 
                                                     then (initializeChar acc limit) @ 
                                                          (initializeString (acc+1) limit)
                                                     else nil
                in
                    dprint "Made it to alloc (string) step\n";
                    [ COMMENT ("alloc string: " ^ V.tostring(v) ^ " <- string "),
                      CONST (W.fromInt (ContextMap.contextLength context)),
                      CONST (W.fromInt string_len),
                      ALLOC_TRACED_STRING, (* addr *)
                      DUP, (* addr addr *)
                      VARIABLE_REF (W.fromInt(#position(new_context_info))),
                      STORE ] @ (* addr *)
                    (initializeString 0 string_len) @
                    [ DROP ] @
                    convertBlock(#context(new_context_info), c)
                end

              | convertBlock (context, Alloc(STRING _, _, _, _)) = raise ToForth "bad string alloc"

              (* allocate constant int *)
              | convertBlock (context, Alloc (INT, [Int i], v, c)) =
                let 
                    val context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to alloc (int) step\n";
                    COMMENT ("alloc_int: " ^ W.toString(i) ^ " -> " ^ V.tostring(v) ^ "") ::
                    CONST (W.fromInt (ContextMap.contextLength context)) ::
                    CONST i ::
                    ALLOC_UNTRACED ::
                    VARIABLE_REF (W.fromInt(#position(context_info))) ::
                    STORE ::
                    convertBlock(#context(context_info), c)
                end

              | convertBlock (context, Alloc (INT, _, _, _)) = raise ToForth "bad int alloc"

              | convertBlock (context, Alloc (TUPLE 0, [], v, c)) =
                (* special case for empty tuples -- store 0 *)
                let 
                    val new_context_info = ContextMap.addToContext(context, v)
                in
                  dprint "Made it to alloc (tuple 0) step\n";
                  COMMENT ("alloc zero tuple: " ^ Int.toString(0) ^ " -> " ^ V.tostring(v) ^ "") ::
                  CONST (W.fromInt (ContextMap.contextLength context)) ::
                  CONST (W.fromInt 0) :: 
                  ALLOC_TRACED_ARRAY :: 
                  VARIABLE_REF (W.fromInt(#position(new_context_info))) :: (* 0 addr addr var *)
                  STORE ::
                  convertBlock(#context(new_context_info), c)
                end

              | convertBlock (context, Alloc (TUPLE n, vas, v, c)) =
                let 
                    val new_context_info = ContextMap.addToContext(context, v)
                    fun getPosition va = #position(ContextMap.findInContext(context,va))
                    fun oneVarInstructions(va, posn) = (NEWLINE :: 
                                                        DUP :: 
                                                        (convertToHeapAddr context va) @
                                                        [SWAP, 
                                                         CONST (W.fromInt((posn+1) * bytes_per_word)), 
                                                         ADD, 
                                                         STORE])
                    val allVarInstructions = (foldr (op @) [] (ListUtil.mapi oneVarInstructions vas)) @ [DROP];
                in
                    dprint "Made it to tuple (n) step\n";
                    COMMENT ("alloc tuple " ^ Int.toString(n) ^ ": " ^ V.tostring(v) ^ " <- [" ^ (vas_and_posns_as_string context vas) ^ "]") :: 
                    NEWLINE ::
                    CONST (W.fromInt (ContextMap.contextLength context)) ::
                    CONST (W.fromInt (n)) ::
                    ALLOC_TRACED_ARRAY :: (* PERF: tuples get zeroed and then refilled here ... *)
                    DUP ::
                    VARIABLE_REF (W.fromInt(#position(new_context_info))) ::
                    STORE ::
                    (allVarInstructions @ 
                     convertBlock(#context(new_context_info), c))
                end

              | convertBlock (context, Alloc (CODE, [Label l], v, c)) =
                let 
                    val context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to alloc code step\n";
                    COMMENT ("alloc code: " ^ V.tostring(v) ^ 
                             "(pos=" ^ Int.toString(#position(context_info)) ^ ")" ^ 
                             " <- " ^ V.tostring(l) ^ "") :: 
                    CONST (W.fromInt (ContextMap.contextLength context)) ::
                    LABEL_REF (V.tostring(l)) ::
                    DEREFERENCE ::
                    ALLOC_UNTRACED ::
                    VARIABLE_REF (W.fromInt(#position(context_info))) ::
                    STORE ::
                    convertBlock(#context(context_info), c)
                end

              | convertBlock (context, Alloc (CODE, _, v, c)) = raise ToForth "bad code alloc"

              | convertBlock (context, Alloc (INT_T t, [vas], v, c)) =
                let
                    val first_va = val_to_var vas;
                    val context_info = ContextMap.findInContext(context, first_va)
                    val new_context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to alloc tagged step\n";
                    COMMENT ("alloc tagged int: " ^ V.tostring(v) ^ " <- [" ^ V.tostring(first_va) ^ "] with tag " ^ Int.toString(t) ^ "") ::
                    CONST (W.fromInt (ContextMap.contextLength context)) ::
                    CONST (W.fromInt t) ::
                    ALLOC_TAGGED ::
                    DUP ::
                    VARIABLE_REF (W.fromInt(#position(new_context_info))) ::
                    STORE ::
                    VARIABLE_REF (W.fromInt(#position(context_info))) ::
                    DEREFERENCE ::
                    SWAP ::
                    CONST (W.fromInt bytes_per_word) ::
                    ADD ::
                    STORE ::
                    convertBlock(#context(new_context_info), c)
                end
              | convertBlock (context, Alloc (INT_T t, nil, v, c)) =
                let
                    val new_context_info = ContextMap.addToContext(context, v)
                in
                    dprint "Made it to alloc tagged step\n";
                    COMMENT ("alloc tagged int: " ^ V.tostring(v) ^ " <- " ^ " [] with tag " ^ Int.toString(t) ^ "") ::
                    CONST (W.fromInt 0) ::
                    CONST (W.fromInt (ContextMap.contextLength context)) ::
                    CONST (W.fromInt t) ::
                    ALLOC_TAGGED ::
                    DUP ::
                    VARIABLE_REF (W.fromInt(#position(new_context_info))) ::
                    STORE ::
                    CONST (W.fromInt bytes_per_word) ::
                    ADD ::
                    STORE ::
                    convertBlock(#context(new_context_info), c)
                end

              | convertBlock (context, c) = 
                let in
                    print "Not implemented:\n";
                    CPSPrint.printe c;
                    print "\n";
                    raise ToForth "CPS instruction not implemented in toforth"
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

  | convert _ = raise ToForth "cps in wrong form"

end


