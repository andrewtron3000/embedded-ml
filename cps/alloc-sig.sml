
signature CPSALLOC =
sig

    (* raised on error *)
    exception CPSAlloc of string

    (* Remove constants from expressions so that they are
       explicitly allocated. Some exceptions are made:
       
       Constant labels in Apps are kept. (We can generate
       more efficient 'jmp' instructions this way.)

       Up to one constant is kept in an arithmetic/comparison primop.
       (If both are constant, we allocate one -- some other
       pass should have optimized this by now!) For div, this can
       only be the divisor; for sub, only the subtrahend.

       And, of course, arguments to small Allocs are retained in
       constant form. (However, tuple components are allocated.)

       In general, any primop that actually consumes one of
       these constant values (like a print_string primop or something)
       should save the constant so that better code can be generated.

       *)

    val convert : CPS.cexp -> CPS.cexp

end