signature GC =
sig

  (* used for compile-time configuration errors and assertions *)
  exception GC of string
  
  datatype shape =
           ARRAY_TRACED   (* length field n, then n traced pointers *)
         | UNTRACED       (* raw data *)
         | TAGGED_POINTER (* untraced word then traced pointer *)

  (* how many words at the beginning of each object are reserved 
    for the GC? *) 
  val gc_header_size : int

  (* called at safe point to maybe do a GC *)
  val gc_check : (* number of roots *)
                 int ->
                 (* returns the sequence of instructions *)
                 UMA.inst list

  (* allocate a dynamic amount of memory *)
  val gc_alloc : (* two temporary registers *)
                 UMA.reg -> UMA.reg ->
                 (* result register *)
                 UMA.reg ->
                 (* allocated size; might be the 
                    same as one of the temporary regs *)
                 UMA.reg ->
                 (* shape of allocated object *)
                 shape ->
                 UMA.inst list

  (* allocate a dynamic amount of memory *)
  val gc_alloc' : RegHelper.workingblock ->
                 (* size of allocation; will be unlocked *)
                 UMA.reg ->
                 (* shape of allocated object *)
                 shape ->
                 (* continuation *)
                 (UMA.reg -> 'a) ->
                 'a

  (* allocate a statically known amount of memory *)
  val gc_alloc_static : 
                 (* two temporary registers *)
                 UMA.reg -> UMA.reg -> 
                 (* result register *)
                 UMA.reg ->
                 (* number of words needed *)
                 int ->
                 (* shape of allocated object *)
                 shape ->
                 UMA.inst list

  val gc_alloc_static' : 
                 RegHelper.workingblock ->
                 (* number of words needed *)
                 int ->
                 (* shape of allocated object *)
                 shape ->
                 (* continuation *)
                 (UMA.reg -> 'a) ->
                 'a

  (* called once at the beginning *)
  val gc_init : Variable.var

  (* called at end of program (debugging) *)
  val gc_end : Variable.var

  (* list of gc "basic blocks."  must include at least gc_init and gc_end as
     well as any code called by gc_check, gc_alloc or gc_alloc_static. *)
  val gc_code : (* beginning of roots in text segment *)
                 int ->
                (* main label; this is jumped to after gc_init *)
                 Variable.var ->
                 (Variable.var * UMA.inst list) list

end
