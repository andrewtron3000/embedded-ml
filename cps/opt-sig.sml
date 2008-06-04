
signature CPSOPT =
sig
    (* raised on encountering an error *)
    exception CPSOpt of string

    (* optimize an expression.
       Run only on pre closure-converted code! *)

    val optimize : CPS.cexp -> CPS.cexp

end