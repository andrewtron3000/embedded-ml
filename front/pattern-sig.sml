
signature PATTERN =
sig

    exception Pattern of string

    (* elaborate user elab elabt ctx (ob,arms,def) 
       ob: must be variables

       returns elaborated pattern match and its type
       *)
    val elaborate : bool -> (Context.context -> EL.exp -> IL.exp * IL.typ) ->
        (Context.context -> Pos.pos -> EL.typ -> IL.typ) ->
        Context.context -> Pos.pos ->
                           string list * 
                           (EL.pat list * EL.exp) list * 
                           (unit -> EL.exp) -> 
        IL.exp * IL.typ

end