
structure ILPrint :> ILPRINT =
struct

    val iltypes = Params.flag true
        (SOME ("-iltypes",
               "show types and coercions in IL")) "iltypes"

    open IL
    structure L = Layout
    structure V = Variable

    val $ = L.str
    val % = L.mayAlign
    val itos = Int.toString

    fun dolist s f i l = StringUtil.delimit s (List.map (fn m => f i m) l)

    fun recordortuple layout sep l r osep vals =
        let 
            val sorted = 
                ListUtil.sort (ListUtil.byfirst HumlockUtil.labelcompare) vals
        in
            if
               (* can't be length 1 *)
               length sorted <> 1 andalso
               (* must be consecutive numbers *)
               ListUtil.alladjacent (fn ((x,_), (sx,_)) => 
                                     (case (Int.fromString x, 
                                            Int.fromString sx) of
                                          (SOME xx, SOME ssxx) => xx = ssxx - 1
                                        | _ => false)) sorted andalso
               (* must be empty or start at 1 *)
               (List.null sorted orelse
                #1 (hd sorted) = "1")
            then L.listex l r osep (map (layout o #2) sorted)
            else L.recordex sep (ListUtil.mapsecond layout sorted)
        end

    fun recordortuple2 layout sep l r osep vals =
        let 
            val sorted = 
                ListUtil.sort (ListUtil.byfirst HumlockUtil.labelcompare) vals
        in
            if
               (* can't be length 1 *)
               length sorted <> 1 andalso
               (* must be consecutive numbers *)
               ListUtil.alladjacent (fn ((x,_), (sx,_)) => 
                                     (case (Int.fromString x, 
                                            Int.fromString sx) of
                                          (SOME xx, SOME ssxx) => xx = ssxx - 1
                                        | _ => false)) sorted andalso
               (* must be empty or start at 1 *)
               (List.null sorted orelse
                #1 (hd sorted) = "1")
            then 
              let val (ls1, ls2) = ListPair.unzip (map (layout o #2) sorted)
              in
                (L.listex l r osep ls1,
                 L.listex l r osep ls2)
              end
            else 
              let val (ll, lsl) = ListPair.unzip (ListUtil.mapsecond layout sorted)
                  val (ls1, ls2) = ListPair.unzip lsl
              in
                (L.recordex sep (ListPair.zipEq (ll, ls1)),
                 L.recordex sep (ListPair.zipEq (ll, ls2)))
              end
        end

           
    exception NoMu
    fun ttol t = ttolex Context.empty t
    and ttolex ctx t =
      if not (!iltypes)
      then $"-"
      else
      let
        val self = ttolex ctx
      in
        (case t of
           (* should print these right-associatively *)
             Arrow (b, [dom], cod) =>
                 L.paren (%[self dom,
                            $(if b then "=>" else "->"),
                            self cod])
           | Arrow (b, dom, cod) => 
                 L.paren (%[L.list (map self dom),
                            $(if b then "=>" else "->"),
                            self cod])
           | TVec t => L.paren (L.seq[self t, $" array"])
           | TCont t => L.paren (L.seq[self t, $" cont"])
           | TRef t => L.paren (L.seq[self t, $" ref"])
           | TVar v => L.str (V.show v)
           | Sum ltl => L.listex "[" "]" "," (map (fn (l, Carrier { carried = t,
                                                                    definitely_allocated = b}) =>
                                                   L.seq[$l, $" : ", 
                                                         self t]
                                                    | (l, NonCarrier) =>
                                                   L.seq[$l, $"(-none-)"]) ltl)
           | Mu (i, m) => 
                 (* try to figure out the datatype's name
                    and write that instead. this is rather
                    a hack... *)
                 (let
                    val thisc =
                      (case List.nth(m, i) of
                         (v, _) => Variable.basename v)
                         handle _ => raise NoMu

                    val t = 
                      (case Context.conex ctx NONE thisc of
                         (kind, Lambda f, _) => 
                           (case kind of
                              0 => $ thisc
                           (* XXX otherwise do anti-unification 
                              or whatever to figure out what
                              the datatype "type constructor"
                              is applied to. (but we should be
                              careful that unification doesn't
                              cause any harmful side-effects.) *)
                            | _ => raise NoMu)

                       (* doesn't look like a datatype! *)
                       | _ => raise NoMu)


                         (* totally normal for this to be out of
                            scope right now *)
                         handle Context.Absent _ => raise NoMu
                           
                  in
                    $ thisc (* raise NoMu *)
                  end
                    handle NoMu =>
                      L.paren (%[$("#" ^ itos i),
                                 $"mu",
                                 L.alignPrefix
                                 (ListUtil.mapi 
                                  (fn ((v,t),n) =>
                                   %[%[$(itos n), $"as",
                                       $(V.tostring v),
                                       $"."], self t]) m,
                                  "and ")]))
           | TRec nil => $"unit"
           | TRec ltl => recordortuple self ":" "(" ")" " *" ltl

           | TTag (t, v) => %[$"tag", self t, $"=>", $(V.tostring v)]
           | Evar (ref (Bound t)) => self t
           | Evar (ref (Free n)) => $("'a" ^ itos n))
      end

    exception Merge
        
    fun merge ((l1 : label, t1)::ltl1, (l2, t2)::ltl2) = 
        if l1 = l2
        then (l1, (t1, t2))::(merge (ltl1, ltl2))
        else raise Merge
      | merge (nil, nil) = nil
      | merge (_, _) = raise Merge

    fun ttolexdif ctx (t1, t2) =
        let 
          exception Diff
        in
          (ignore (Unify.unify ctx t1 t2); ($"_", $"_"))
          handle Unify.Unify _ =>
                 (let 
                    val self = ttolexdif ctx
                  in
                    case (t1, t2) of
                      (Arrow (b1, [dom1], cod1), Arrow (b2, [dom2], cod2)) =>
                      if b1 = b2 then
                        let val (doml1, doml2) = self (dom1, dom2)
                            val (codl1, codl2) = self (cod1, cod2)
                        in
                          (L.paren (%[doml1,
                                      $(if b1 then "=>" else "->"),
                                      codl1]),
                           L.paren (%[doml2,
                                      $(if b2 then "=>" else "->"),
                                      codl2]))
                        end
                       else raise Diff
                      | (Arrow (b1, dom1, cod1), Arrow (b2, dom2, cod2)) => 
                           if b1 = b2 then
                             let val doms = ListPair.zipEq (dom1, dom2)
                                 val (doml1, doml2) = ListPair.unzip (map self doms)
                                 val (codl1, codl2) = self (cod1, cod2)
                             in
                               (L.paren (%[L.list doml1,
                                           $(if b1 then "=>" else "->"),
                                           codl1]),
                                L.paren (%[L.list doml2,
                                           $(if b2 then "=>" else "->"),
                                           codl2]))
                             end
                               handle ListPair.UnequalLengths => raise Diff
                           else
                             raise Diff
                        
                      | (TVec t1, TVec t2) => 
                           let val (l1, l2) = self (t1, t2) in
                             (L.paren (L.seq[l1, $" array"]),
                              L.paren (L.seq[l2, $" array"]))
                           end
                         | (TCont t1, TCont t2) => 
                           let val (l1, l2) = self (t1, t2) in
                             (L.paren (L.seq[l1, $" cont"]),
                              L.paren (L.seq[l2, $" cont"]))
                           end
                         | (TRef t1, TRef t2) => 
                           let val (l1, l2) = self (t1, t2) in
                             (L.paren (L.seq[l1, $" ref"]),
                              L.paren (L.seq[l2, $" ref"]))
                           end

                         (* XXX handle mu? handle sum? *)
                             
                         | (TRec ltl1, TRec ltl2) =>
                           (let
                              val ltl = merge (ltl1, ltl2)
                            in
                              recordortuple2 self ":" "(" ")" " *" ltl
                            end
                              handle Merge => raise Diff)

                         (* lookup bound evars and continue *)
                         | (Evar (ref (Bound t)), _) => self (t, t2)
                         | (_, Evar (ref (Bound t))) => self (t1, t)

                         (* otherwise, there is no shared structure so just
                           print them out normally *)
                         | _ => raise Diff
                  end
                    handle Diff => (ttolex ctx t1, ttolex ctx t2))
        end
          

    (* <t> *)
    fun bttol t = if !iltypes then L.seq[$"<", ttol t, $">"]
                  else $""

    fun etol e =
        (case e of
             Int i => $("0x" ^ Word32.toString i)
           | String s => $("\"" ^ String.toString s ^ "\"")
           (* | Char c => $("?" ^ implode [c]) *)
           | App (e1, [e2]) => L.paren(%[etol e1, etol e2])
           | App (e1, e2) => L.paren(%[etol e1, L.list (map etol e2)])

           | Polyvar (nil, v) => $(V.show v)
           | Polyvar (tl, v) => %[$(V.show v),
                                  if !iltypes 
                                  then 
                                    L.listex "<" ">" "," (map ttol tl)
                                  else $""]

           (* print as if n-ary *)
           | Seq _ => 
                 let 
                     fun allseqs acc (Seq (ee, er)) = allseqs (ee::acc) er
                       | allseqs acc e = (acc, e)

                     val (front, last) = allseqs nil e
                 in
                     L.listex "(" ")" ";" (rev (map etol (last::front)))
                 end
           (* also fake n-ary like above *)
           | Let (dd, ee) =>
                 let
                     fun alldecs acc (Let (dd, er)) = alldecs (dd::acc) er
                       | alldecs acc e = (acc, e)

                     val (decs', body) = alldecs nil e
                     val decs = rev (map dtol decs')
                 in
                     L.align
                     [$"let",
                      L.indent 4 (L.align decs),
                      $"in",
                      L.indent 4 (etol body),
                      $"end"]
                 end
           | Proj (l, t, e) => 
                 if !iltypes
                 then 
                   %[L.seq[$("#" ^ l), $"/", L.paren(ttol t)],
                     etol e]
                 else %[$("#" ^ l), etol e]
           | Record sel => recordortuple etol "=" "(" ")" "," sel
           | Primapp (po, el, ts) =>
                 %( [$"[PRIM", $(Primop.tostring po),
                     L.listex "(" ")" "," (map etol el)]
                   @ (case (!iltypes, ts) of 
                          (_, nil) => nil
                        | (false, _) => nil
                        | _ => [L.listex "<" ">" "," (map ttol ts)])
                   @ [$"]"])
           | Inject (t, l, NONE) => L.paren (%[$("inj_" ^ l),
                                               bttol t,
                                                $"(-NONE-)"])
           | Inject (t, l, SOME e) => L.paren(%[$("inj_" ^ l),
                                                bttol t,
                                                etol e])
           | Unroll e => 
                 if !iltypes
                 then L.paren(%[$"unroll", etol e])
                 else etol e
           | Roll (t,e) => 
                 if !iltypes
                 then L.paren(%[$"roll", 
                                bttol t,
                                etol e])
                 else etol e
           | Sumcase (t, e, v, lel, def) =>
                 L.align
                 (%[$"sumcase", etol e, %[$":", ttol t], 
                    %[$"as", $ (V.tostring v)]] ::
                  map (fn (l, e) => %[%[$"  |", $l, $"=>"], L.indent 4 (etol e)]) lel @
                  [%[%[$"  |", $"_", $"=>"], L.indent 4 (etol def)]])
           | Tagcase (t, e, v, vel, def) =>
                 L.align
                 (%[$"tagcase", etol e, $":", ttol t, 
                    $"as", $ (V.tostring v)] ::
                  map (fn (vv, e) => %[%[$"  |", $(V.tostring vv), 
                                         $"=>"], etol e]) 
                         vel @
                  [%[$"  |", $"_", $"=>", etol def]])

           | Throw (e1, e2) => L.paren(%[$"throw",
                                         etol e1,
                                         $"to",
                                         etol e2])
           | Letcc (v, t, e) => %[$"letcc", $(V.tostring v),
                                  $":", %[ttol t, $"cont"],
                                  $"in",
                                  etol e]


           | Raise (t, e) => L.paren(%[$"raise", 
                                       bttol t, etol e])
           | Tag (e1, e2) => L.paren(%[$"tag", etol e1, $"with", etol e2])


           | Deferred os =>
                 (case Util.Oneshot.deref os of
                      NONE => $"XXX-UNSET-ONESHOT-XXX"
                    | SOME e => etol e)

           | Handle (e, v, h) => %[L.paren(etol e),
                                   $"handle",
                                   %[%[$(V.tostring v), $"=>"], etol h]]

           | Jointext el =>
                 %[$"jointext",
                   L.listex "[" "]" "," (map etol el)]

           (* | _ => $"???XXX???" *))

    and dtol d =
        (case d of
             Do e => %[$"do", etol e]
           | Tagtype v => %[$"tagtype", $(V.tostring v)]
           | Newtag (new, t, ext) => %[$"newtag", $(V.tostring new), 
                                       $"tags", ttol t, $"in", 
                                       $(V.tostring ext)]
           | Val vtep =>
                 let
                     fun up (Quant (v, p)) =
                         let val (quants, var, t, e) = up p
                         in (v :: quants, var, t, e)
                         end
                       | up (Mono (v, t, e)) = (nil, v, t, e)

                     val (quants, var, t, e) = up vtep
                 in
                     %[%[%([$"val"]
                           @ (case quants of
                                  nil => nil
                                | _ => [L.listex "(" ")" "," (map ($ o V.tostring) quants)])
                           @ [$(V.tostring var)]),
                         L.indent 4 (%[$":", ttol t, $"="])],
                       L.indent 4 (etol e)]
                 end
           | Fix pfs =>
                 let
                     fun up (Quant (v, p)) =
                       if !iltypes then %[$(V.tostring v), up p]
                       else up p
                       | up (Mono fl) =
                         L.alignPrefix
                         (map (fn {name, arg, dom, cod, body, inline, recu, total} =>
                               %[
                               %[if length arg <> length dom 
                                then $"XXX arg/dom mismatch!!"
                                else $"",
                                $(V.tostring name),
                                if !iltypes 
                                then L.seq[$(if inline then "INLINE " else ""),
                                           $(if recu then "RECU " else "NRECU "),
                                           $(if total then "TOTAL" else "PART")]
                                else $"",
                                L.listex "(" ")" "," 
                                    (ListPair.map 
                                     (fn (a, t) =>
                                      %[$(V.tostring a),
                                        if !iltypes 
                                        then L.seq[$":", ttol t]
                                        else $""]) (arg, dom)),
                                %[$":",
                                  ttol cod,
                                  $"="]],
                                L.indent 4 (etol body)]) fl,
                          "and ")
                 in
                     %[$"fun", up pfs]
                 end)

end
