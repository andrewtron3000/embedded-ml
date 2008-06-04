
structure Nullary :> NULLARY =
struct

    exception Nullary of string

    structure SM = StringMap
    open EL

    infixr 9 `
    fun a ` b = a b

    datatype c = CON | EXN

    fun ismono (GT, _) s = isSome (SM.find(GT, s))
    fun nullexp (_, GC) s = 
      case (SM.find(GC, s)) of
        NONE => false
      | SOME EXN => true
      | SOME CON => false

    fun nullpat (_, GC) s = SM.find (GC, s)

    fun nul G (exp, loc) =
        let fun % x = (x, loc)
            fun self e = nul G e
        in
          %(case exp of
                Constant _ => exp
              | Var s => 
                    if nullexp G s
                    then App(%exp, % `Record nil)
                    else exp
              | Modvar _ => exp
              | Float _ => exp
              | App (a, b) => App(self a, self b)
              | Throw (a, b) => Throw(self a, self b)
              (* XXX v might shadow constructor *)
              | Letcc (v, b) => Letcc (v, self b)
              | Record sel => Record ` ListUtil.mapsecond self sel
              | Vector el => Vector ` map self el
              | Proj (s, t, e) => Proj (s, tul G t, self e)
              | Andalso (a, b) => Andalso(self a, self b)
              | Orelse (a, b) => Orelse(self a, self b)
              | Andthen (a, b) => Andthen (self a, self b)
              | Otherwise (a, b) => Otherwise (self a, self b)
              | If (a, b, c) => If(self a, self b, self c)
     
              | Seq (a, b) => Seq(self a, self b)
              | Constrain (e, t) => Constrain(self e, tul G t)
              | Jointext el => Jointext ` map self el
              | Raise e => Raise ` self e
              | CompileWarn s => CompileWarn s
              | Handle (e, pel) =>
                 Handle(self e, map (fn (p, ee) =>
                                     (pul G p, self ee)) pel)
              | Case (el, plel, NONE) =>
                 Case(map self el,
                      map (fn (pl, ee) =>
                           (map (pul G) pl, self ee)) plel, NONE)
              | Case _ => raise Nullary "case SOME"
              | Let (d, e) => 
                 let val (GG, dd) = dul G d
                 in Let (dd, nul GG e)
                 end)
        end

    and tul G typ = 
        (case typ of
             TVar s =>
                 if ismono G s
                 then TApp(nil, NONE, s)
                 else typ
           | TNum _ => typ
           | TModvar _ => typ
           | TApp (tl, so, s) => TApp (map (tul G) tl, so, s)
           | TRec stl => TRec ` ListUtil.mapsecond (tul G) stl
           | TArrow (a,b) => TArrow (tul G a, tul G b))

    and pul G pat =
        (case pat of
             PVar s =>
               (case nullpat G s of
                   NONE => pat
                 | SOME EXN => PApp (s, SOME ` PRecord nil)
                 | SOME CON => PApp (s, NONE))

           | PWild => PWild
           | PAs (s, p) => PAs(s, pul G p)
           | PRecord spl => PRecord ` ListUtil.mapsecond (pul G) spl
           | PConstrain (p, t) => PConstrain (pul G p, tul G t)
           | PConstant c => pat
           | PWhen (e, p) => PWhen (nul G e, pul G p)
           | PApp (s, p) => PApp (s, Option.map (pul G) p))

    and dul G (dec, loc) =
        let fun % (G, x) = (G, (x, loc))
        in
        %(case dec of
             Do e => (G, Do ` nul G e)
           | Type (sl, s, t) => (G, Type (sl, s, tul G t))
           (* this is where things are added *)
           | Datatype (sl, dats : (string * (string * typ option) list) list) =>
                 let
                     (* inside the body, references should never be type applications. 
                        XXX should filter out in case we're shadowing a type here *)
                     val GTarms = #1 G

                     (* produce the datatype decl with the new
                        nullary constructor map. *)
                     fun mapdt acc GC nil = (Datatype(sl, rev acc), GC)
                       | mapdt acc GC ((t, stol)::rest) =
                         let
                             fun doarm aa GC nil = (rev aa, GC)
                               | doarm aa GC ((s, SOME t)::more) =
                                 doarm ((s, SOME (tul (GTarms, GC) t))::aa) GC more
                               | doarm aa GC ((s, NONE)::more) =
                                 doarm ((s, NONE)::aa)
                                 (SM.insert (GC, s, CON)) more

                             val (arm, GC) = doarm nil GC stol
                         in
                             mapdt ((t, arm) :: acc) GC rest
                         end

                     val (newdt, GC) = mapdt nil (#2 G) dats

                       
                     (* for the body of the let, we should rewrite these types
                        to type applications if the type var list is empty. *)
                     val types = 
                         if List.null sl
                         then map #1 dats
                         else nil

                     val GT = foldl SM.insert' (#1 G) ` map (fn t => (t, true)) types

                 in
                   ((GT, GC), newdt)
                 end
           | Tagtype _ => (G, dec)

           (* actually, rewrite exception decs too *)
           | Newtag (a, SOME t, b) => (G, Newtag (a, SOME ` tul G t, b))
           | Newtag (a, NONE, b) => ((#1 G, SM.insert (#2 G, a, EXN)),
                                     Newtag  (a, SOME ` TRec nil, b))

           | Exception (a, SOME t) => (G, Exception (a, SOME ` tul G t))
           | Exception (a, NONE) => ((#1 G, SM.insert (#2 G, a, EXN)),
                                     Exception (a, SOME ` TRec nil))

           | Signature (so, decs) =>
                 (G, Signature (so, map
                                (fn SVal (sl, s, t) => SVal(sl, s, tul G t)
                                 |  SType (sl, t) => SType (sl, t)
                                 |  SPrim (sl, s, t, p) => SPrim(sl, s, tul G t, p)) decs))
           | Val (sl, p, e) => (G, Val (sl, pul G p, nul G e))

           | Native (f, s, t) => (G, Native (f, s, tul G t))

           | Fun fl =>
                 (G,
                  Fun ` map (fn (sl, f, clauses) =>
                             (sl, f, map
                              (fn (pl, to, e) =>
                               (map (pul G) pl, Option.map (tul G) to,
                                nul G e)) clauses)) fl))
        end

    fun nullary exp = nul (SM.empty, SM.empty) exp


end
