
signature ELABUTIL =
sig

    exception Elaborate of string

    val ltos : Pos.pos -> string

    val error : Pos.pos -> string -> 'b

    val new_evar : unit -> IL.typ

    (* unify context location message actual expected *)
    val unify : Context.context -> Pos.pos -> string -> 
                    IL.typ -> IL.typ -> unit

    (* int to string *)
    val itos : int -> string

    (* generate a new string with arg as base *)
    val newstr : string -> string

    (* XXX only one of the following two should surivive, probably *)

    (* if t has unset evars, replace those with new type
       variables. return the list of new type variables
       and the substituted type. Pass in a "surrounding
       context" to determine which variables are (in)eligible
       for generalization. *)
    val polygen : Context.context -> IL.typ -> IL.typ * Variable.var list

    (* XXX update spec/type -- see comment in sml *)
    val generalize : Context.context -> IL.exp -> IL.typ -> 
                           IL.exp * Variable.var list * IL.typ

    val evarize : IL.typ IL.poly -> IL.typ

    val unroll : Pos.pos -> IL.typ -> IL.typ

end