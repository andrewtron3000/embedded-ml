
signature ELABORATE =
sig

    exception Elaborate of string

    val elab : Context.context -> EL.exp -> IL.exp * IL.typ

end