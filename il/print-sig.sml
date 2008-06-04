
signature ILPRINT =
sig

    (* attempts to use type abbreviations to print
       datatypes if in scope. *)
    val ttolex : Context.context -> IL.typ -> Layout.layout
    (* take a pair of types and produce a pair of layouts 
      that shows only their differences *)
    val ttolexdif : Context.context -> (IL.typ * IL.typ) -> (Layout.layout * Layout.layout)

    (* type, expression, and declaration  to layout. *)
    val ttol : IL.typ -> Layout.layout
    val etol : IL.exp -> Layout.layout
    val dtol : IL.dec -> Layout.layout

end
