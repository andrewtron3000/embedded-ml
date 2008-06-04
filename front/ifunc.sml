
(* XXX this is like not needed *)
(* These are the internal functions that aphasia provides.
   XXX TODO: hide these and provide an interface to them
   at better types (ie, finds should return an option, not -1) *)

structure IFunc =
struct
    (* toplevel signature *)

    open EL
    open Primop

    infixr -->
    fun d --> c = TArrow (d, c)

    fun tuple l =
        TRec (ListUtil.mapi (fn (t, n) => (Int.toString (n+1), t)) l)

    val s = TVar "string"
    val i = TVar "int"
    val u = tuple nil
    val b = TApp(nil, NONE, "bool")
    val c = TVar "char"
    fun list t = TApp([t], NONE, "list")

    val toplevel =
        Signature
        (NONE, nil)

end