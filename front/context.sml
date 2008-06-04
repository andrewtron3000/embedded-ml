
structure Context :> CONTEXT =
struct
    open Variable
        
    structure S = StringMap
    structure SU = StringMapUtil

    structure MM = ModuleMap
    structure MMU = ModuleMapUtil

    datatype context = 
        C of { vars : (IL.typ IL.poly * Variable.var * IL.idstatus) MM.map,
               cons : (IL.kind * IL.con * IL.tystatus) MM.map,
               dbs  : unit S.map }

    (* XXX absent should include class of identifier (con/db/tab/col/id) *)
    exception Absent of string

    fun absent s =
        let in 
(*
              print "(Unbound in context: ";
              print s;
              print ")\n";
*)
            raise Absent s
        end

    (* this only checks vars, not cons.
       But evars shouldn't ever appear bound to type variables, right?
       (I can't look inside lambdas, anyway.) *)
    fun has_evar (C{vars, ...}) n =
        MMU.exists
        (fn (pt, _, _) =>
         let
             open IL
             fun up (Quant (_, p)) = up p
               | up (Mono t) =
                 let
                     fun has tt =
                         (case tt of
                              TVar _ => false
                            | TRec ltl => List.exists (fn (_, t) => 
                                                       has t) ltl
                            | Arrow (_, tl, t) =>
                                  has t orelse
                                  List.exists has tl
                            | Sum ltl => List.exists 
                                  (fn (_, Carrier { carried, ... }) => has carried
                                     | _ => false) ltl
                            | Mu (_, vtl) => List.exists (fn (_, t) =>
                                                          has t) vtl
                            | Evar (ref (Free m)) => n = m
                            | Evar (ref (Bound t)) => has t
                            | TVec t => has t
                            | TCont t => has t
                            | TTag (t, _) => has t
                            | TRef t => has t)
                 in
                     has t
                 end
         in
             up pt
         end) vars

        

    fun varex (C {vars, ...}) module sym =
        (case MM.find (vars, (module, sym)) of
             SOME x => x
           | NONE => absent sym)

    fun var ctx sym = varex ctx NONE sym

    fun conex (C {cons, ...}) module sym =
        (case MM.find (cons, (module, sym)) of
             SOME x => x
           | NONE => absent sym)

    fun con ctx sym = conex ctx NONE sym

    fun bindex (C {vars, cons, dbs}) module sym typ var stat =
        C { vars = MM.insert (vars, (module, sym), (typ, var, stat)),
            cons = cons,
            dbs = dbs }

    fun bindv a b c d = bindex a NONE b c d IL.Normal

    fun bindcex (C { cons, vars, dbs }) module sym con kind status =
        C { vars = vars,
            cons = MM.insert (cons, (module, sym), (kind, con, status)),
            dbs = dbs }

    fun bindc c sym con kind status = bindcex c NONE sym con kind status

    val empty = C { vars = MM.empty, cons = MM.empty, dbs = S.empty }

end
