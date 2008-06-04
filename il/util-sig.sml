
signature ILUTIL =
sig

    exception ILUtil of string

    (* pointwise f e
       applies f to every immediate subexpression of e
       and then rebuilds e. 

       ie. pointwise f (Pair(e1, e2)) = Pair(f e1, f e2)

       nb. This preserves totality/recursiveness annotations,
       so be sure that f preserves these, or else don't
       call on fix expressions.
       *)
    val pointwise : (IL.exp -> IL.exp) -> IL.exp -> IL.exp
    (* copy a term *)
    val duplicate : IL.exp -> IL.exp
 
    val mappoly : ('a -> 'b) -> 'a IL.poly -> 'b IL.poly

    val tsubste : IL.typ Variable.Map.map -> IL.exp -> IL.exp

    val unevar : IL.typ -> IL.typ

end