
signature NULLARY =
sig

    exception Nullary of string

    (* rewrites datatype declarations and constructor uses
       to support nullary constructors (no "of") and
       non-polymorphic datatypes.

       datatype t =
          A of int
        | B

        - becomes -

       datatype () t =
          A of int
        | B

       and patterns

       case e of
          B => e'

        - become - 

       case e of
          B() => e'

       where B is an App pattern applied to nuthin' (not
       writable in the concrete syntax.)

       occurrences of the type t become (()t) (not writable in the
       concrete syntax -- that's TApp(nil, TVar "t"). 

       The rest of the compiler assumes this translation has
       been done. *)
    val nullary : EL.exp -> EL.exp

end