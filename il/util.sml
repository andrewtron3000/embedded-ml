structure ILUtil :> ILUTIL =
struct
    open IL
    exception ILUtil of string

    fun mappoly f (Mono x) = Mono (f x)
      | mappoly f (Quant(v, x)) = Quant (v, mappoly f x)

    (* run f on all immediate subexpressions of exp,
       then rebuild it *)
    fun pointwise f exp = 
        (case exp of
             Polyvar (tl, v) => exp
           | Deferred os =>
                 (case Util.Oneshot.deref os of
                      SOME e => pointwise f e
                    | NONE => raise ILUtil "unset oneshot in pointwise")
           | Int _ => exp
           | String _ => exp
           (* | Char _ => exp *)
           | Jointext el => Jointext (map f el)
           | Record lel => Record (ListUtil.mapsecond f lel)
           | Proj (l, t, e) => Proj(l, t, f e)
           | Raise (t, e) => Raise(t, f e)
           | Handle (e, v, handler) => Handle(f e, v, f handler)
           | Seq (e1, e2) => Seq (f e1, f e2)
           | Roll (t, e) => Roll (t, f e)
           | Unroll e => Unroll (f e)
           | Tag (e1, e2) => Tag (f e1, f e2)
           | Inject(t, l, e) => Inject (t, l, Option.map f e)
           | Throw (e1, e2) => Throw(f e1, f e2)
           | Primapp(po, el, tl) => Primapp (po, map f el, tl)
           | App (ff, el) => App (f ff, map f el)
           | Sumcase (t, e, v, lel, def) => Sumcase (t, f e, v,
                                                     ListUtil.mapsecond f lel,
                                                     f def)
           | Tagcase (t, e, v, vel, def) => Tagcase (t, f e, v,
                                                     ListUtil.mapsecond f vel,
                                                     f def)
           | Letcc (v, t, e) => Letcc (v, t, f e)
           | Let(Do e1, e2) => pointwise f (Seq(e1, e2))
           | Let(Tagtype v, e) => Let(Tagtype v, f e)
           | Let(Newtag(v, t, vv), e) => Let(Newtag (v, t, vv), f e)
           | Let(Val vtep, rest) => Let(Val
                                        (mappoly (fn (v, t, e) =>
                                                  (v, t, f e)) vtep), 
                                        f rest)
           | Let(Fix flp, rest) => Let(Fix
                                       (mappoly (map (fn {name, arg, dom,
                                                          cod, body, inline,
                                                          recu, total} =>
                                                      {name=name,
                                                       inline=inline,
                                                       recu=recu,
                                                       total=total,
                                                       arg=arg,
                                                       dom=dom,
                                                       cod=cod,
                                                       body=f body})) flp),
                                       f rest))

    (* nb. does not handle capture/shadowing.
       also, be careful because oneshot.wrap only works when
       there is exactly one copy of each oneshot. *)
    fun tsubste s exp =
        let 
            val self = tsubste s
            fun sub t = Subst.tsubst s t
        in
            case exp of
                (* anything that has a t in it *)
                Polyvar(tl, v) => Polyvar(map sub tl, v)
                (* allow delayed *)
              | Deferred os =>
                    (Util.Oneshot.wrap self os; exp)
              | Raise(t, e) => Raise(sub t, self e)
              | Proj(l, t, e) => Proj(l, sub t, self e)
              | Roll(t, e) => Roll(sub t, self e)
              | Tagcase(t, obj, v, vel, def) =>
                    Tagcase(sub t, self obj, v,
                            ListUtil.mapsecond self vel,
                            self def)
              | Primapp(po, el, tl) =>
                    Primapp(po, map self el, map sub tl)
              | Sumcase(t, obj, v, vel, def) =>
                    Sumcase(sub t, self obj, v,
                            ListUtil.mapsecond self vel,
                            self def)
              | Letcc(v, t, e) => Letcc(v, sub t, self e)
              | Let(Newtag(v,t,vv), e) =>
                    Let(Newtag(v, sub t, vv), self e)
              | Let(Val vtep, e) =>
                    Let(Val (mappoly (fn (v,t,e) => (v, sub t, self e)) vtep),
                        self e)
              | Let(Fix flp, e) =>
                    Let(Fix (mappoly (map (fn {name, arg, dom,
                                               cod, body, inline,
                                               recu, total} =>
                                           {name=name,
                                            arg=arg,
                                            dom=map sub dom,
                                            cod=sub cod,
                                            recu=recu,
                                            body=self body,
                                            inline=inline,
                                            total=total})) flp),
                        self e)
              (* otherwise recurse *)
              | _ => pointwise self exp
        end

    fun duplicate e = pointwise duplicate e

    fun unevar (Evar (ref (Bound t))) = unevar t
      | unevar (Evar _) = raise ILUtil "didn't expect unset evar!"
      | unevar t = t

end