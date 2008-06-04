
signature UNIFY =
sig
    exception Unify of string

    val new_ebind : unit -> IL.ebind ref

    val unify : Context.context -> IL.typ -> IL.typ -> unit
end
