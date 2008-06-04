
structure Unify :> UNIFY =
struct

    open IL
        
    structure V = Variable.Map
        
    local 
        val last_tag = ref 0
    in
        fun new_ebind () =
            let in
                last_tag := !last_tag + 1;
                ref (Free (!last_tag))
            end
    end

    fun same_evar r x =
        (case x of
             (Evar (ref (Bound t))) => same_evar r t
           | (Evar (r' as ref (Free _))) => r = r'
           | _ => false)


    exception Unify of string

    fun occurs r x =
        (case x of
             TVar _ => false
           | TRec stl => ListUtil.existsecond (occurs r) stl
           | Arrow (_, t1, t2) => List.exists (occurs r) t1 
                                  orelse occurs r t2
           | Sum (lcl) => 
                 ListUtil.existsecond (fn (Carrier {carried=t, ...}) => occurs r t | _ => false) lcl
           | Mu (_, vcl) => ListUtil.existsecond (occurs r) vcl
           | TRef c => occurs r c
           | TVec c => occurs r c
           | TCont c => occurs r c
           | TTag (t, _) => occurs r t
           | (Evar (ref (Bound t))) => occurs r t
           | (Evar (r' as ref (Free _))) => r = r')

    (* r := Bound t  with path compression. *)
    fun set r (Evar (ref (Bound t))) = set r t
      | set r t = r := Bound t

    fun mapif m v =
        case Variable.Map.find (m, v) of
            NONE => v
          | SOME vv => vv

    fun mapplus m (v, vv) =
        Variable.Map.insert (m, v, vv)

    fun unifyex ctx eqmap t1 t2 =
        (case (t1, t2) of
             (TVar v1, TVar v2) =>
                 ignore (Variable.eq (mapif eqmap v1, v2) 
                            orelse raise Unify "Var")

           | (TTag (t1, v1), TTag (t2, v2)) => 
                 let in
                     Variable.eq (mapif eqmap v1, v2) 
                      orelse raise Unify "tag var";
                     unifyex ctx eqmap t1 t2
                 end

           | (TVec t1, TVec t2) => unifyex ctx eqmap t1 t2
           | (TCont t1, TCont t2) => unifyex ctx eqmap t1 t2
           | (TRec lcl1, TRec lcl2) =>
                 let
                     val l = ListUtil.sort 
                              (ListUtil.byfirst String.compare) lcl1
                     val r = ListUtil.sort 
                              (ListUtil.byfirst String.compare) lcl2
                 in
                     ignore
                     (ListUtil.all2 (fn ((l1,t1),(l2,t2)) =>
                                     let in
                                         unifyex ctx eqmap t1 t2;
                                         l1 = l2
                                     end) l r
                      orelse raise Unify "Record")
                 end
           | (Arrow (_, dom1, cod1), Arrow (_, dom2, cod2)) => 
                 let in
                     ListUtil.all2 (fn (a, b) => (unifyex ctx eqmap a b; 
                                                  true)) 
                                        dom1 dom2
                        orelse raise Unify "Arrows have different arity";
                     unifyex ctx eqmap cod1 cod2
                 end
           | (Evar (ref (Bound t1)), t2) => unifyex ctx eqmap t1 t2
           | (t1, Evar (ref (Bound t2))) => unifyex ctx eqmap t1 t2
           | (Evar (r as ref (Free _)), t2) =>
                 if same_evar r t2 then ()
                 else if occurs r t2 then
                         raise Unify "circularity"
                      else set r t2
           | (t1, Evar (r as ref (Free _))) =>
                 if same_evar r t1 then ()
                 else if occurs r t1 then
                         raise Unify "circularity"
                      else set r t1 
           | (TRef c1, TRef c2) => unifyex ctx eqmap c1 c2
           | (Mu (i1, m1), Mu (i2, m2)) => 
                 ignore
                 ((i1 = i2 andalso
                   ListUtil.all2 (fn ((v1, t1),
                                     (v2, t2)) =>
                                 (unifyex ctx (mapplus eqmap (v1, v2)) t1 t2; 
                                  true)) m1 m2)

                  orelse raise Unify "mu")
           | (Sum ltl1, Sum ltl2) =>
                 ignore
                 ((ListUtil.all2 (fn ((l1, t1),
                                      (l2, t2)) =>
                                  ((case (t1, t2) of
                                      (NonCarrier, NonCarrier) => ()
                                    | (Carrier { definitely_allocated = aa1, carried = tt1}, 
                                       Carrier { definitely_allocated = aa2, carried = tt2}) => 
                                        let in
                                          if aa1 = aa2 then ()
                                          else raise Unify "sum:always_allocated(bug!)";
                                          unifyex ctx eqmap tt1 tt2
                                        end
                                    | _ => raise Unify "sum:carrier");
                                      l1 = l2))
                  (ListUtil.sort (ListUtil.byfirst String.compare) ltl1)
                  (ListUtil.sort (ListUtil.byfirst String.compare) ltl2))
                  orelse raise Unify "sum")
           | _ => raise Unify "tycon mismatch")

    fun unify ctx t1 t2 = unifyex ctx Variable.Map.empty t1 t2
end
