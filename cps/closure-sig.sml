
signature CLOSURE =
sig

    (* raised on error *)
    exception Closure of string

    (* probably move elsewhere *)
    val MAX_ARGS : int

    (* closure convert an expression and hoist all the closed
       functions out to the outer level. *)
    val convert : CPS.cexp -> CPS.cexp

end