
signature REGHELPER =
sig

  (* type of imperative block of instructions, that can be
     appended to *)
  type workingblock

  exception RegHelper of string * workingblock

  (* Creates new working block *)
  val empty : (int * bool) CPS.VVM.map -> Variable.var -> workingblock

  (* returns the finalized working block *)
  val finalize : workingblock -> UMA.inst list

  (* sometimes a basic block will diverge into
     more than one control path. This creates a new
     empty block with the same register status. *)
  val split : workingblock -> workingblock

  (* Causes an instruction to be added to the working
     block. Checks to see that the appropriate
     registers are locked and updates the register
     state according to the result of the
     instruction. *)
  val emit : workingblock -> UMA.inst -> unit
  (* Emit code specialized for some insts *)
  (* For branching instructions, second argument
   indicates whether or not all registers should be
   forcibly spilled *)
  val emit_JZ : workingblock -> bool -> (UMA.reg * Variable.var) -> unit
  val emit_JNZ : workingblock -> bool -> (UMA.reg * Variable.var) -> unit
  val emit_JMP : workingblock -> bool -> Variable.var -> unit
  val emit_DEC : workingblock -> UMA.reg -> unit
  (* Emit a comment giving the current status of the register file *)
  val emit_status : workingblock -> unit

  (* Give me (and lock for read/write) a register *)
  val getreg : workingblock -> UMA.reg
  (* Give me (and lock for read/write) a register
     with the literal in it. *)
  val getlit : workingblock -> CPS.intconst -> UMA.reg
  (* Give me a register (for reading only) that
     has this literal in it. *)
  val getlitro : workingblock -> CPS.intconst -> UMA.reg
  (* Like the literal variants immediately above *)
  val getaddr : workingblock -> Variable.var -> UMA.reg
  val getaddrro : workingblock -> Variable.var -> UMA.reg
  (* Give me a read-only register that holds the value
     of this variable.  Use a register if the variable
     is already present, otherwise reload it from the
     stack.  *) 
  val getvar : workingblock -> Variable.var -> UMA.reg

  (* Return a register to the file for reuse *)
  val unlock : workingblock -> UMA.reg -> unit

  (* Clear out unlocked registers including the
   contents and any variables, spilling if necessary *)
  val clearunlocked : workingblock -> unit

  (* Bind a variable to the value in the given
     register.  Must be called on a locked register,
     and will change the lock state to make this
     register read-only.  Dead variables should be
     killed using 'killvar' after the corresponding
     register has been unlocked for the last time to
     avoid spilling dead varibles. *)
  val bindvar : workingblock -> Variable.var -> UMA.reg -> unit
  (* As above, but bind the variable to a constant
     value.  No unlock is necessary after calling this
     function.  Use getvar if you need a register that
     holds this value. *)
  val bindvarlit : workingblock -> Variable.var -> CPS.intconst -> unit

  (* Indicates that a variable is now dead (and
    therefore avoid generating spill code for any
    register that might contain it.) *)
  val killvar : workingblock -> Variable.var -> unit

  (* debugging *)
  val dump : workingblock -> unit
  val checkstate : workingblock -> workingblock

end
