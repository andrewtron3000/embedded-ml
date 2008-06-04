
structure Subst =
struct

  open IL

  structure E = EL

  exception Subst of string

  type 'a subst = 'a Variable.Map.map

  fun fromlist l =
      foldl Variable.Map.insert' Variable.Map.empty l

  fun tsubst s (x as (TVar v)) =
      (case Variable.Map.find (s, v) of
           SOME tt => tt
         | NONE => x)
    | tsubst s (TRec ltl) = TRec (ListUtil.mapsecond (tsubst s) ltl)
    | tsubst s (Arrow (b, dom, cod)) = Arrow (b, map (tsubst s) dom, 
                                              tsubst s cod)
    | tsubst s (Sum ltl) = Sum (ListUtil.mapsecond (arminfo_map (tsubst s)) ltl)
    | tsubst s (Mu (i, vtl)) =
           let (* remove bindings for each variable *)
               val ns = foldl (fn ((v,_), s) => 
                               Variable.Map.insert (s, v, TVar v)) s vtl
           in  Mu (i, ListUtil.mapsecond (tsubst ns) vtl)
           end
    | tsubst s (Evar(ref (Bound t))) = tsubst s t
    | tsubst s (x as (Evar _)) = x

    | tsubst s (TRef t) = TRef (tsubst s t)

    | tsubst s (TVec t) = TVec (tsubst s t)
    | tsubst s (TCont t) = TCont (tsubst s t)

    | tsubst s (TTag (t, v)) = TTag (tsubst s t, v)

  fun etsubst s t =
      (case t of
           (x as E.TVar v) =>
               (case StringMap.find (s, v) of
                    SOME tt => tt
                  | NONE => x)
         | E.TModvar _ => t
         | E.TNum n => t
         | E.TApp (tl, so, str) => E.TApp (map (etsubst s) tl, so, str)
         | E.TRec stl => E.TRec (ListUtil.mapsecond (etsubst s) stl)
         | E.TArrow (dom, cod) => E.TArrow(etsubst s dom, etsubst s cod))


  fun pbinds  E.PWild _ = false
    | pbinds (E.PAs (i, p)) v = i = v orelse pbinds p v
    | pbinds (E.PConstrain (p, t)) v = pbinds p v
    | pbinds (E.PConstant _) _ = false
    | pbinds (E.PVar i) v = i = v
    | pbinds (E.PRecord lpl) v = 
      ListUtil.existsecond (fn p => pbinds p v) lpl
    | pbinds (E.PApp  (_, SOME p)) v = pbinds p v
    | pbinds (E.PApp  (_, NONE)) v = false
    | pbinds (E.PWhen (_, p)) v = pbinds p v

  (* exp/var in exp'. assumes exp closed. *)
     
  fun esubst (s as (vv : string, ee : EL.exp_)) (e,loc) =
      (fn x => (x, loc))
      (case e of
           E.Var v => if v = vv then ee else e
         | E.Modvar _ => e
         | E.Constant _ => e
         | E.Float _ => e
         | E.Proj (l,t,e) => E.Proj (l, t, esubst s e)
         | E.Record lel => E.Record (ListUtil.mapsecond (esubst s) lel)
         | E.Seq (a,b) => E.Seq (esubst s a, esubst s b)
         | E.Let (d,e) =>
               let val (d, shadowed) = dsubst s d
               in E.Let (d, if shadowed then e else esubst s e)
               end

         | E.Throw (a, b) => E.Throw(esubst s a, esubst s b)
         | E.Letcc (v, e') =>
               if v = vv 
               then e
               else E.Letcc(v, esubst s e')
         | E.App (a,b) => E.App (esubst s a, esubst s b)

         | E.Andalso (a,b) => E.Andalso (esubst s a, esubst s b)
         | E.Orelse (a,b) => E.Orelse (esubst s a, esubst s b)
         | E.Andthen (a,b) => E.Andthen (esubst s a, esubst s b)
         | E.Otherwise (a,b) => E.Otherwise (esubst s a, esubst s b)
         | E.If (a,b,c) => E.If (esubst s a, esubst s b, esubst s c)

         | E.Constrain (e,t) => E.Constrain (esubst s e, t)
         | E.Case (el, pel, NONE) => E.Case (map (esubst s) el, 
                                             map (mlsubst s) pel, NONE)
         | E.Case _ => raise Subst "case SOME"
         | E.Raise e => E.Raise (esubst s e)
         | E.Handle (e, pel) => E.Handle (esubst s e, map (msubst s) pel)

         | E.Jointext el => E.Jointext (map (esubst s) el)

         | E.Vector el => E.Vector (map (esubst s) el)
         | E.CompileWarn s => E.CompileWarn s
     )


  (* pattern lists as in fn; 
     just pretend it's a tuple for the sake of bindings *)
  (* XXX is this accurate? are earlier args available in the 'when' 
     clauses for pats in later args? *)
  and fsubst (s as (vv, ee)) (pl,e) =
      if pbinds (E.PRecord (map (fn p => ("", p)) pl)) vv then (pl, e)
      else (map (psubst s) pl, esubst s e)

  and mlsubst (s as (vv, ee)) (pl, e) =
      if List.exists (fn p => pbinds p vv) pl
      then (map (psubst s) pl, e)
      else (map (psubst s) pl, esubst s e)

  and psubst s (E.PWild) = E.PWild
    | psubst s (E.PAs (i, p)) = E.PAs (i, psubst s p)
    | psubst s (E.PConstrain (p, t)) = E.PConstrain(psubst s p, t)
    | psubst s (E.PConstant c) = E.PConstant c
    | psubst s (E.PVar i) = E.PVar i
    | psubst s (E.PRecord lpl) = 
    E.PRecord (ListUtil.mapsecond (psubst s) lpl)
    | psubst s (E.PApp  (t, p)) = E.PApp (t, Option.map (psubst s) p)
    | psubst s (E.PWhen (e, p)) = E.PWhen (esubst s e, psubst s p)

  (* substitute in a match (pattern * expression), but only if the
     pattern doesn't shadow the variable in question. *)
  and msubst (s as (vv, ee)) (p, e) =
      if pbinds p vv then (psubst s p, e)
      else (psubst s p, esubst s e)

  (* substitute in a declaration, and return a boolean indicating if the
     (term) variable was shadowed in the decl. *)

  and dsubst (s as (vv,ee)) ((d,loc) : E.dec) =
      (fn (x,b) => ((x, loc), b))
      (case d of
           E.Val (tyvars, p, e) => (E.Val (tyvars, psubst s p, esubst s e), 
                                    pbinds p vv)
         | E.Do e => (E.Do (esubst s e), false)
         | E.Fun l => 
               (* if any function is named the same as this variable,
                 do no substitution and return shadowed. *)
               if List.exists (fn (_, f, _) => f = vv) l then (d, true)
               else let
                        fun dfs (pl, to, e) =
                            if pbinds (E.PRecord (map (fn x => 
                                                       ("", x)) pl)) vv 
                            then (map (psubst s) pl, to, e)
                            else (map (psubst s) pl, to, esubst s e)
                    in (E.Fun (map (fn (tyvars, f, branches) => 
                                    (tyvars, f, map dfs branches)) l), false)
                    end
         | (d as E.Native (f, _, _)) => (d, f = vv) (* no bound vars or subexps *)
         | E.Datatype (_, dl) =>
               (* datatypes have no expressions, 
                  but bind some constructors *)
               let fun one (_, ctors) = 
                   List.exists (fn (i,_) => i = vv) ctors
               in (d, List.exists one dl)
               end
         (* these are simple *)
         | E.Exception (i, _) => (d, i = vv)
         | E.Tagtype t => (d, false)
         | E.Newtag (i, _, _) => (d, i = vv)
         | E.Type _ => (d, false)
         | E.Signature (SOME _, _) => (d, false)
         | E.Signature (NONE, strdecs) => 
               let fun one (E.SVal(_, id, _)) = vv = id
                     | one (E.SPrim(_, id, _, _)) = vv = id
                     | one (E.SType _) = false
               in
                   (d, List.exists one strdecs)
               end)

end
