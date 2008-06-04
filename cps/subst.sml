
structure CPSSubst =
struct
    exception Subst of string
    open CPS

    (* non-capture-avoiding substitution *)
    fun subst s c =
        let val self = subst s
            fun dv (Var v) =
                (case Variable.Map.find (s, v) of
                     NONE => (Var v)
                   | SOME va => va)
              | dv va = va
        in
            case c of
                Alloc (t, vas, v, ce) =>
                    Alloc(t, map dv vas, v, self ce)
              | Project (i, va, v, ce) =>
                    Project (i, dv va, v, self ce)
              | App (va, vl) =>
                    App (dv va, map dv vl)
              | Fix (vael, ce) =>
                    Fix (map (fn (v, a, e) =>
                              (v, a, self e)) vael, self ce)
              | Intswitch (va, iel, def) => 
                    Intswitch (dv va, 
                               ListUtil.mapsecond self iel,
                               self def)
              | Sumswitch (va, num, v, iel, def) => 
                    Sumswitch (dv va, num, v,
                               ListUtil.mapsecond self iel,
                               self def)
              | Primop (po, vas, vrs, ces) =>
                    Primop (po, map dv vas, vrs, map self ces)
              | Deferred osd =>
                    Deferred
                     (fn () =>
                      let val old = osd ()
                      in
                      case Util.Oneshot.deref old of
                          NONE =>  old
(*                              raise Subst 
                                      ("can't do delayed subst because " ^
                                       "oneshot was never set!") *)
                        | SOME ce => Util.Oneshot.init (self ce)
                      end)
        end

    (* substitution and simultaneous alpha-vary
       (avoids capture) *)
    fun alphavary s c =
        let val self = subst s
            fun dv (Var v) =
                (case Variable.Map.find (s, v) of
                     NONE => (Var v)
                   | SOME va => va)
              | dv va = va

            fun nv s v =
                let 
                    val vv = Variable.alphavary v
                in
                    (Variable.Map.insert(s, v, Var vv),
                     vv)
                end

            fun ren s v k =
                let
                    val (ns, vv) = nv s v
                in
                    k (ns, vv)
                end

            fun renl s vs k =
                let
                    fun folder (v, (cs, vl)) =
                        let val (ns, vv) = nv cs v
                        in (ns, vv :: vl)
                        end
                in
                    k (foldr folder (s, nil) vs)
                end

        in
            case c of
                Alloc (t, vas, v, ce) =>
                    ren s v
                    (fn (s, v) =>
                     Alloc(t, map dv vas, v, alphavary s ce))
              | Project (i, va, v, ce) =>
                    ren s v
                    (fn (s, v) =>
                     Project (i, dv va, v, alphavary s ce))
              | App (va, vl) =>
                    App (dv va, map dv vl)
              | Fix (vael, ce) =>
                    renl s (map #1 vael)
                    (fn (s, vs) =>
                     let val nvael =
                         ListPair.map (fn (vv, (_, a, e)) =>
                                       (vv, a, e)) (vs, vael)
                     in
                         Fix (map (fn (v, a, e) =>
                                   (* XXX to avoid capture here,
                                      also need to rename args
                                      (but it should suit our
                                      purpose in cps opt ok) *)
                                   (v, a, alphavary s e)) nvael,
                              alphavary s ce)
                     end)
              | Intswitch (va, iel, def) => 
                    Intswitch (dv va, 
                               ListUtil.mapsecond self iel,
                               self def)
              | Sumswitch (va, num, v, iel, def) => 
                    ren s v
                    (fn (s, v) =>
                     Sumswitch (dv va, num, v,
                                ListUtil.mapsecond (alphavary s) iel,
                                alphavary s def))
              | Primop (po, vas, vrs, ces) =>
                    renl s vrs
                    (fn (s, vrs) =>
                     Primop (po, map dv vas, vrs, map (alphavary s) ces))
              | Deferred osd =>
                    Deferred
                     (fn () =>
                      let val old = osd ()
                      in
                      case Util.Oneshot.deref old of
                          NONE => old
(*                              raise Subst 
                                  ("can't do delayed subst because " ^
                                   "oneshot was never set!") *)
                        | SOME ce => Util.Oneshot.init (self ce)
                      end)
        end



end
