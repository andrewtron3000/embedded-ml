
structure Limits =
struct
  
  (* layout of data
     0:                cr0 code (jumps to main)
                       ...
     globals:          global data (exception handler..)
     stack_start:      stack area
     code_start:       all code here
     *)

  (* measurements always in words *)
  (* size reserved cr0 launching pad. *)
  val MAX_CR0 = 128
  val GLOBALS_START = MAX_CR0

(* PERF make this smaller now that adventure is optimized? *)
  (* SUSP must agree with the number of variables in variables.fr *)
  val STACK_SIZE = 2048

  (* allocate some globals *)
  local
    val global_ctr = ref 0
    fun global () = (GLOBALS_START + (!global_ctr)
                     before
                     global_ctr := !global_ctr + 1)
  in
    (* XXX should probably be 64-bit? *)
    val LAST_TAG = global ()

    val UNTRACED_GLOBALS = !global_ctr

    (* traced globals here *)
    val EXCEPTION_HANDLER = global ()

    val GLOBALS_SIZE = !global_ctr
    val TRACED_GLOBALS = GLOBALS_SIZE - UNTRACED_GLOBALS
  end

  val TRACING_START = GLOBALS_START + UNTRACED_GLOBALS
  val STACK_START = GLOBALS_START + GLOBALS_SIZE

  val CODE_START = STACK_START + STACK_SIZE

end
