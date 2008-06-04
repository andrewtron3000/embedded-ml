
signature ILOPT =
sig
    
    exception ILOpt of string

    (* XXX add initial context? *)
    val optimize : IL.exp -> IL.exp

end
