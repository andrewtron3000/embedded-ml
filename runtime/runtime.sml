(* Humlock runtime. *)
structure Runtime =
struct

  val lab_pow2_table = Variable.namedvar "pow2_table"
  val pow2tab =
    (lab_pow2_table,
     List.tabulate
     (32,
      fn i => UMA.DATA (Word32.<<(0w1, Word.fromInt i))))

  val lab_shifttable = Variable.namedvar "shift_table"
  val shifttable =
    (lab_shifttable,
     [UMA.DATA 0w1,
      UMA.DATA 0w256,
      UMA.DATA (0w256 * 0w256),
      UMA.DATA (0w256 * 0w256 * 0w256)])

  val signbit = (Conventions.lab_signbit, [UMA.DATA Conventions.SIGN_BIT])

  val mantissamask = (Conventions.lab_mantissamask, [UMA.DATA Conventions.MANTISSA_MASK])

  val DYNAMIC_WORDS = 3
    
  val lab_dynamic = Variable.namedvar "dynamicregion"
  val dynamic =
(*    (lab_dynamic, List.tabulate (DYNAMIC_WORDS, 
                                 fn _ => UMA.DATA 0w0))
   *)
    (lab_dynamic,
     [UMA.DATA 0wx756e696e,
      UMA.DATA 0wx69746961,
      UMA.DATA 0wx6c697a65,
      UMA.DATA 0wx3c2d2d2d])

  (* wraps a program with the Humlock runtime. *)
  fun wrap (blocks, main) =
    let
    in
      (GC.gc_code Limits.TRACING_START main @ 
       [pow2tab, shifttable, signbit, mantissamask, dynamic] @ blocks,
       GC.gc_init)
    end
  
  (* create an assemblable program from a prog/entrypoint pair *)
  fun cr0 (blocks, main) =
    let
    in
      ((* Conventions.errormsg "\nhi\n" @  *)
       [UMA.LITERAL_ADDR(Conventions.aa, main, Conventions.hh),
        UMA.LOADPROG(Conventions.zz, Conventions.aa)],
       blocks)
    end

end
