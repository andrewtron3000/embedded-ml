
signature TOCPS =
sig

    (* length of the maximum record generated. Guaranteed to be at least 2. *)
    val MAXRECORD : int

    (* convert k e

       convert an IL expression e to a CPS expression.
       The continuation k is called to generate the "tail"
       of the expression from its final value. Typically
       this is something like (fn v => Finish v).
       *)
    val convert : (CPS.value -> CPS.cexp) -> IL.exp -> CPS.cexp

    (* clear some debugging stuff; call between converted programs *)
    val clear : unit -> unit

    exception CPS of string

end
