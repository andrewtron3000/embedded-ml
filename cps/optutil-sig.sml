
signature CPSOPTUTIL =
sig

    exception CPSOptUtil of string

    (* eval the comparison of a binary operator on two ints *)
    val evalcmp : Primop.compare -> CPS.intconst -> CPS.intconst -> bool

    (* evaluate a binary operator on two ints, if defined *)
    val evalints : Primop.binop -> CPS.intconst -> CPS.intconst -> CPS.value option

    (* is the primop effect-free? *)
    val noeffect : Primop.primop -> bool

end