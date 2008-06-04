
structure Variable :> VARIABLE =
struct

    exception Variable of string

    datatype var =
        Special of string option * string
      | Regular of int * string * (unit -> string)

    val arena = StringOnce.arenaex "_"

    fun namedvar s =
        let val (i, f) = StringOnce.symbol arena s
        in Regular(i, s, f)
        end

    fun special so s = 
        let in
            (* if unqualified,
               prevent others from printing with this
               name by incrementing its reference count *)
            if isSome so
            then ()
            else ignore (StringOnce.symbol arena s);

            Special(so, s)
        end

    fun getspecial (Special(so, s)) = SOME (so, s)
      | getspecial _ = NONE

    fun newvar () = namedvar "vv"

    fun basename (Regular(_, s, _)) = s
      | basename (Special(_, s)) = s

    fun alphavary (v as Regular _) = namedvar (basename v)
      | alphavary _ = raise Variable "can't alphavary special"

    fun eq (Regular(n1, _, _), Regular(n2, _, _)) = n1 = n2
      | eq (Special _, Regular _) = false
      | eq (Regular _, Special _) = false
      | eq (Special (so, s), Special (sso, ss)) = so = sso andalso s = ss
        
    fun compare (Regular(n1, _, _), Regular(n2, _, _)) = Int.compare (n1, n2)
      | compare (Regular _, Special _) = LESS
      | compare (Special _, Regular _) = GREATER
      | compare (Special (so, s), Special (sso, ss)) =
        HumlockUtil.pathcompare ((so, s), (sso, ss))
        
    fun tostring (Regular(_, _, f)) = f ()
      | tostring (Special _) = raise Variable "can't tostring special"

    fun show (Regular(_, _, f)) = f()
      | show (Special (NONE, s)) = "." ^ s
      | show (Special (SOME modu, s)) = modu ^ "." ^ s
        
    structure Map = SplayMapFn (struct
                                    type ord_key = var
                                    val compare = compare
                                end)
end
