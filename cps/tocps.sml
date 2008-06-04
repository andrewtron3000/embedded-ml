
(* Convert to Continuation Passing Style.
   The algorithm implemented is more-or-less the
   same as in "Compiling With Continuations" (Appel),
   though I have made minor changes along with changing
   the datatype. (I also pass continuations as the
   first argument to a function rather than the last.)

   Everything happens in "convert".
*)

(* PERF get rid of mutually-recursive fixes where they are not
   needed. It would be good if cpsopt could do this for us,
   but it doesn't yet *)

structure ToCPS :> TOCPS =
struct

  (* There were getting to be too many parens... *)
  infixr 9 `
  fun a`b = a b
  fun list x = [x]

  structure V = Variable
  structure I = IL
  structure LU = ListUtil
  open CPS
  open Primop

  val itos = Int.toString

  exception CPS of string

  local
    structure SM = StringMap
    val db = ref SM.empty : int SM.map ref
    val ctr = ref 0
    fun ++ c = (c := !c + 1; !c)

    (* can aid in debugging assembly code *)
    val uniquesum = Params.flag false
      (SOME ("-uniquesum", 
             "Generate unique tags for each sum label " ^ 
             "(turns off representation optimizations too)")) "uniquesum"

  in

    fun clear () = (ctr := 0; db := SM.empty)

      
    datatype labrep = LABREP_NORMAL of int | LABREP_NULL | LABREP_NOTAG
    datatype sumrep = SUMREP_NORMAL | SUMREP_LIST | SUMREP_NONE
                        (* ... enum, etc. *)

    fun max_tag ltl = 
      if !uniquesum then (case Int.maxInt of NONE => 536870912 * 4 | SOME i => i)
      else length ltl

    fun genlabel l ltl =
      case LU.position (fn (s,_) => s = l) ltl of
        NONE => NONE
      | SOME n =>
          if !uniquesum
          then SOME(SUMREP_NORMAL,
                    case SM.find (!db, l) of
                      NONE => let val this = ++ctr 
                              in 
                                db := SM.insert(!db, l, this);
                                SymbolDB.push "sumlabel" this l;
                                LABREP_NORMAL this
                              end
                    | SOME i => LABREP_NORMAL i)
          else 
            let
              fun islist [(_, I.NonCarrier), (_, I.Carrier {definitely_allocated = true, ...})] = true
                | islist [(_, I.Carrier {definitely_allocated = true, ...}), (_, I.NonCarrier)] = true
                | islist _ = false

              val rep =
                (* best rep *)
                if length ltl = 1 then SUMREP_NONE
                (* pretty cheap *)
                else if islist ltl then SUMREP_LIST
                     else (* PERF test ENUM *)
                       (* no special rep available... *)
                       SUMREP_NORMAL
            in
              case rep of
                SUMREP_NORMAL => SOME(SUMREP_NORMAL, LABREP_NORMAL n)
              | SUMREP_NONE => SOME(SUMREP_NONE, LABREP_NOTAG)
              | SUMREP_LIST => SOME(SUMREP_LIST,
                                      case ListUtil.Alist.find op= ltl l of
                                        NONE => raise CPS "impossible/notfound"
                                      | SOME I.NonCarrier => LABREP_NULL
                                      | SOME (I.Carrier _) => LABREP_NOTAG)
            end
            
  end


  (* PERF -- keep low now to detect bugs.
     with current um backend there is no real limit,
     so it might as well be quite high! (Unless we
     think that tuples are rebuilt frequently, in which
     case being more like lists makes them more efficient
     because we are able to reuse common tails.) *)
  val MAXRECORD = 64

  fun error s = (print s; print "\n"; raise CPS s)

  (* strip off quantifiers, not needed here... *)
  fun losepoly (I.Mono a) = a
    | losepoly (I.Quant (_, a)) = losepoly a

  fun unevar (IL.Evar (ref (IL.Bound tt))) = unevar tt
    | unevar t = t

  fun resolvetype G (IL.TVar v) = error "unimplemented: type vars"
    | resolvetype _ t = unevar t

  (* XXX subst these *)
  val iltrue  = Initial.trueexpil
  val ilfalse = Initial.falseexpil

  (* convert does all of the work.
     It's passed an IL context G (used to look up types to do
     "type directed translation", for instance), a continuation
     c, and an IL expression. The IL expression is translated
     into a CPS value (usually a variable), and that is passed
     to c. *)

  (* nb: redefined below to pass initial context empty *)
  fun convert G c (I.Polyvar (_, v)) = c ` Var v
    | convert G c (I.Int i) = c ` Int i
    (* | convert G c (I.Char ch) = c ` Int ` Word32.fromInt ` ord ch *)
    | convert G c (I.String s) = 
    let val v = V.namedvar "string"
    in Alloc(STRING s, [], v, c ` Var v)
    end
    | convert G c (I.Record lel) =
      let
          (* first, sort the fields alphabetically. *)
          val lel = LU.sort (LU.byfirst String.compare) lel
          (* now, maybe chain based on the value of MAXRECORD. In
             the process, strip off labels, which we won't use now.
             Note that we only process the first MAXRECORD (or MR-1)
             fields, leaving the rest to recursion...
             *)
          fun split l =
              if length l > MAXRECORD then
                  map (fn(_,e)=>e) (List.take (l, MAXRECORD - 1)) @
                  [I.Record ` List.drop (l, MAXRECORD - 1)]
              else map (fn(_,e)=>e) l

          val lel = split lel

          val v = V.namedvar "record"

          (* continuation after cpsing arguments. Just allocate away... *)
          fun k values = Alloc(TUPLE ` length lel, values, v, c ` Var v)
      in
          convertl G k lel
      end
    | convert G c (I.Proj (l, t, e)) =
      let
       val v = V.namedvar "proj"

       (* this is only tricky because we may need to chain projections. *)
       fun k value =
           case resolvetype G t of
             IL.TRec lel =>
                let
                    val lel = LU.sort (LU.byfirst String.compare) lel
                in 
                  case LU.position (fn (s,_) => s = l) lel of
                    NONE => 
                     error 
                     "Label in projection is not in the type!!"
                  | SOME i =>
                     let
                         (* generate a series of projections to 
                            reach the tail. invt n < sz *)
                         fun gproj (value, sz) n =
                             let in
(*
                                 print ("gproj from " ^ 
                                        CPSPrint.vtos value ^ 
                                        " #" ^ itos n ^ " size = " ^
                                        itos sz ^ "\n");
*)
                             if sz <= MAXRECORD
                             then (* standard representation *)
                                 Project(n, value, v, c ` Var v)
                             else if n < MAXRECORD - 1
                                  then (* has chain, but don't need it *)
                                      Project(n, value, v, c ` Var v)
                                  else (* get chain, recurse *)
                                      let val vt = V.namedvar "chain"
                                          val skip = MAXRECORD - 1
                                      in
                                          Project(skip,
                                                  value, vt,
                                                  gproj (Var vt,
                                                         sz - skip)
                                                    (n - skip))
                                      end
                             end
                     in
                         gproj (value, length lel) i
                     end
                end
         | _ => error "Type in projection is not record, or is unresolved!"
      in
          convert G k e
      end
    | convert G c (I.Seq (e1, e2)) = convert G c (I.Let(I.Do e1, e2))
    | convert G c (I.Let(I.Do e1, e2)) = 
      let fun k _ = convert G c e2
      in convert G k e1
      end
    | convert G c (I.Let(I.Val vtep, e2)) =
      let 
          val (v, _, e) = losepoly vtep

          fun k value = Primop(PBind, list value, list v, 
                               list ` convert G c e2)
      in
          convert G k e
      end
    | convert G c (I.Deferred os) = 
                 (case Util.Oneshot.deref os of
                      NONE => error "Unset oneshot in CPS conversion"
                    | SOME e => convert G c e)

    | convert G c (I.Jointext el) =
                    convert G c (I.Primapp(PJointext, el, nil))

    (* Target language is untyped, so ... *)
    | convert G c (I.Unroll e) = convert G c e
    | convert G c (I.Roll (_, e)) = convert G c e
    | convert G c (I.Inject (t, l, eo)) =
      (case resolvetype G t of
           I.Sum ltl =>
               let val ltl = LU.sort (LU.byfirst String.compare) ltl
                   val vn = V.namedvar "nonebody"
                   val v = V.namedvar "inject"
               in case (genlabel l ltl, eo) of
                   (NONE, _) => error "Label in inject not part of type!!"
                 | (SOME (_, LABREP_NORMAL n), NONE) =>
                     (* Alloc(TUPLE 0, [], vn, *)
                        Alloc(INT_T n, [], v, c ` Var v)
                 | (SOME (_, LABREP_NORMAL n), SOME e) => 
                       let
                           fun k value = Alloc(INT_T n, list value, v, 
                                               c ` Var v)
                       in
                           convert G k e
                       end
                 | (SOME (_, LABREP_NULL), SOME e) =>
                       (* xxx maybe a primop to alloc null... *)
                       (* convert G (fn _ => Alloc(TUPLE 0, [], v, c ` Var v)) e *)
                       error "labrep_null for carrier??"
                 | (SOME (_, LABREP_NULL), NONE) =>
                       Alloc(TUPLE 0, [], v, c ` Var v)

                 | (SOME (_, LABREP_NOTAG), SOME e) => convert G c e
                 (* can happen(?), like "datatype t = A". representation is ()? *)

                 | (SOME (_, LABREP_NOTAG), NONE) =>
                       error ("unimplemented notag/non-carrier : " ^ l)
(*                        Alloc(TUPLE 0, [], v, c ` Var v) *)

                 (* | SOME _ => error "oops, sum representation not implemented" *)
               end
         | _ => error "Type in inject is not a sum, or is unresolved!")

(* no good--v must be bound in def, but we don't know a priori how to
   unwrap the object
    | convert G c (I.Sumcase (t, ob, v, nil, def)) =
    (* no arms; just use default *)
         (error "no arms";
          convert G c (I.Seq(ob, def)))
*)

    | convert G c (I.Sumcase (t, ob, v, lel, def)) =
      (* straightforward translation to CPS sumcase,
         but need to reify the continuation because it
         will be referenced in each arm and the default. *)
      (case resolvetype G t of
         I.Sum nil => error "XXX unimp: case on void"
       | I.Sum (ltl as ((l, _) :: _)) =>
             (case genlabel l ltl of
                SOME (SUMREP_NONE, _) =>
                  (case lel of
                     nil => convert G
                              (fn va =>
                               Primop(PBind, [va], [v],
                                      [convert G c def])) ob
                     (* def unreachable *)
                   | [(_, e)] => convert G (fn va =>
                                            Primop(PBind, [va], [v], 
                                                   [convert G c e])) ob
                   | _ => error "bug: sumrep_none with cases!")

              | SOME (SUMREP_LIST, _) =>
                     (case lel of
                        (* var is bound in the default, but will
                           only occur if the default is the nonzero
                           case. *)
                        nil => convert G
                                (fn va =>
                                 Primop(PBind, [va], [v],
                                        [convert G c def])) ob
                      | _ =>
                          (* two arms. one is represented as NULL.
                             the other is just whatever it is. *)
                          let
                            fun findem (NONE, SOME nz) nil = (def, nz)
                              | findem (SOME z, NONE) nil = (z, def)
                              (* throw away def.. *)
                              | findem (SOME z, SOME nz) nil = (z, nz)
                              | findem (NONE, NONE) nil = 
                              error "sumrep/option impossible (covered above)"
                              | findem (z, nz) ((l, e) :: rest) =
                              (case (z, nz, genlabel l ltl) of
                                 (NONE, nz, SOME(_, LABREP_NULL)) => findem (SOME e, nz) rest
                               | (SOME _, _, SOME(_, LABREP_NULL)) => error "already found labrep-null"
                               | (z, NONE, SOME(_, LABREP_NOTAG)) => findem (z, SOME e) rest
                               | (_, SOME _, SOME(_, LABREP_NOTAG)) => error "already found labrep-notag"
                               | _ => error ("unexpected rep in findem : " ^ l))

                            val (z, nz) = findem (NONE, NONE) lel

                            fun k value =
                              let 
                                val x = V.namedvar "caseresult_optrep"
                                val f = V.namedvar "casejoin_optrep"
                                fun jumpback onearm = App(Var f, [onearm])

                                val z = convert G jumpback z
                                val nz = convert G jumpback nz
                              in
                                Fix([(f, list x, c `Var x)],
                                    Primop(PBind, [value], [v],
                                           [Primop(PNull, [value], [],
                                                   [z, nz])]))
                              end
                          in
                            convert G k ob
                          end)

              | SOME (SUMREP_NORMAL, _) => 
               let
                   val ltl = LU.sort (LU.byfirst String.compare) ltl

                   val lel = LU.sort (LU.byfirst String.compare) lel
                   val _ =
                       LU.alladjacent (LU.byfirst op <>) lel
                       orelse 
                       error "bug: duplicate case labels in sumcase"

                   fun tagn l =
                       case genlabel l ltl of
                           NONE => 
                               error 
                               "label in sum arm is not in the type!!"
                         | (SOME (_, LABREP_NORMAL n)) => n
                         | _ => error "bad labrep for sumrep_normal!"

                   fun k value =
                       let val x = V.namedvar "caseresult"
                           val f = V.namedvar "casejoin"
                           fun jumpback onearm = App(Var f, [onearm])
                           val arms = map (fn (l, e) => 
                                           (tagn l, 
                                            convert G jumpback e)) lel
                       in
                           Fix([(f, list x, c `Var x)],
                               Sumswitch(value, 
                                         max_tag ltl, v, arms, 
                                         convert G jumpback def))
                       end
               in
                   convert G k ob
               end
              | NONE => error "no rep for sumtype??")
         | _ => error "type in sumcase is not sum or is unresolved!")

    | convert G c (I.App (e, el)) =
           let
               fun k function =
                   let
                       val r = V.namedvar "ret"
                       val x = V.namedvar "retarg"
                       fun kk args =
                           Fix([(r, list x, c ` Var x)],
                               App(function, Var r :: args))
                   in
                       convertl G kk el
                   end
           in
               convert G k e
           end

    | convert G c (I.Throw (e1, e2)) =
           let
             fun k arg =
               let
                 fun kk cont = App(cont, [arg])
               in
                 convert G kk e2
               end
           in
             convert G k e1
           end

    | convert G c (I.Letcc (v, _, e)) =
           let
             val ka = V.namedvar "cc_arg"

             fun k va = App(Var v, [va])
           in
             (* reify continuation as a function
                (call it 'v', as the cont is) *)
             Fix([(v, [ka], c (Var ka))],
                 (* and proceed, calling that
                    function when done *)
                 convert G k e)
           end

    | convert G c (I.Let(I.Fix polyfns, e)) =
           let
               val fns = losepoly polyfns
               fun onefn {name, arg, dom=_, cod=_, body, 
                          inline=_,
                          recu=_, total=_} = 
                   let val w = V.namedvar "fret"
                       fun k r = App(Var w, list r)
                   in (name, w :: arg, convert G k body)
                   end
           in
               Fix(map onefn fns, convert G c e)
           end

    (* no type checking, so we don't care *)
    | convert G c (I.Let(I.Tagtype _, e)) = convert G c e

    (* we generate these randomly, so make a pair. 
       1 in 2^32 odds are pretty poor ... *)
    | convert G c (I.Let(I.Newtag (tag, _, _), e)) =
           let
               val tagl = V.namedvar "tagl"
               val tagr = V.namedvar "tagr"
           in
               Primop
               (PNewtag, nil, [tagl],
                list `
                Primop
                (PNewtag, nil, [tagr],
                 list `
                 Alloc(TUPLE 2,
                       [Var tagl, Var tagr],
                       tag,
                       convert G c e)))
           end

    (* unlike sumcase, we can't ever hope to do a clever job with
       tagcase. So just unroll the comparisons right here. *)
    | convert G c (I.Tagcase (t, obj, var, (v,e)::rest, def)) =
           (* v is bound to a pair of integers.
              obj is (tag, data). if v = tag, then
              run e with var = data. else, continue ... *)
           let
               val vl1 = V.namedvar "tagcasel1"
               val vl2 = V.namedvar "tagcasel2"

               val vr1 = V.namedvar "tagcaser1"
               val vr2 = V.namedvar "tagcaser2"

               val tag = V.namedvar "tag"

               val fail = V.namedvar "tagfail"

               val join = V.namedvar "tcasejoin"
               val joina = V.namedvar "joinarg"

               fun joink va = App(Var join, list va)

               fun k ov =
                Fix(list (join, [joina], c ` Var joina),
                Fix(list (fail, nil,
                     convert G joink
                      (I.Tagcase (t, obj, var, rest, def))),
                 Project
                 (0, ov, tag,
                  Project
                  (0, Var tag, vl1,
                   Project
                   (0, Var v, vl2,
                    Primop
                    (B ` PCmp PEq, [Var vl1, Var vl2],
                     nil,
                     [Project
                      (1, Var tag, vr1,
                       Project
                       (1, Var v, vr2,
                        Primop
                        (B ` PCmp PEq, [Var vr1, Var vr2],
                         nil,
                         [Project
                          (1, ov, var,
                           convert G joink e),
                          App(Var fail, nil)]))),
                      App(Var fail, nil)]))))
                    ))
           in
               convert G k obj
           end

    | convert G c (I.Tagcase (_, obj, var, nil, def)) =
           convert G c def

    | convert G c (I.Raise (_, e)) = 
           let val h = V.namedvar "currenthandler"
               fun k exnval = 
                   Primop(PGethandler, nil, list h, 
                          list ` App(Var h, list exnval))
           in convert G k e
           end

    (* note switched order of tag, value *)
    | convert G c (I.Tag (va, tg)) =
           let
               val v = V.namedvar "tagged"

               fun k vava = convert G (kk vava) tg
               and kk vava tgva =
                   Alloc(TUPLE 2, [tgva, vava], v, c ` Var v)
           in
               convert G k va
           end

    | convert G c (I.Handle (e, v, eh)) =
           (* this code is a bit complicated, but 
              conceptually it's simple.

              We make a new function "newhandler" that will serve as
              the exception handler, and install this, then continue
              evaluating e. Both the new handler and a wrapper around
              the expression e must reset the handler to the old handler
              before they exit. Since there are two possible return
              paths, we abstract the join point into one, called 
              "leavehandler". 
              *)
           (* XXX don't make an mrec fix, because the handler escapes,
              but calls to the join point are usually direct. *)
           let
               val h = V.namedvar "oldhandler"
               val n = V.namedvar "newhandler"
               val f = V.namedvar "leavehandler"
               val x = V.namedvar "leavehandlerarg"

               fun jumpback result = App(Var f, list result)
           in
               Primop(PGethandler, nil, list h,
                      list `
                      Fix([(f, list x, c ` Var x),
                           (n, list v, Primop(PSethandler, list ` Var h, nil,
                                              list ` convert G jumpback eh))],
                          Primop(PSethandler, list ` Var n, nil,
                                 list ` convert G 
                                   (fn v => Primop(PSethandler, 
                                                   list ` Var h, nil, 
                                                   list ` jumpback v)) e)))
           end

    (* in the CPS language, comparisons are control ops, not expressions *)
    | convert G c (I.Primapp(po as B (PCmp co), [i1, i2], nil)) =
           let
               fun k va1 va2 =
                   let val x = V.namedvar "cmpresult"
                       val f = V.namedvar "cmpjoin"
                   in
                       Fix([(f, list x, c ` Var x)],
                           Primop(po,
                                  [va1, va2],
                                  nil,
                                  [convert G (fn tt => 
                                              App(Var f, [tt])) iltrue,
                                   convert G (fn ff => 
                                              App(Var f, [ff])) ilfalse]))
                   end
           in
               convert G (fn v1 => convert G (k v1) i2) i1
           end


    | convert G c (I.Primapp(B (PCmp _ ), _, _)) =
           raise CPS "should have two ints and no types to binary comparison"

(*
    | convert G c (I.Primapp(PRef, [e], _)) =
           let val v = V.namedvar "newref"
               fun k value = Alloc(REF, list value, v, c ` Var v)
           in
               convert G k e
           end

    (* could do a similar thing for 'Write' and other ops that 
       don't have any real return value. these allocations will
       be wiped out in the opt phase *)
    | convert G c (I.Primapp(PSet, [r, v], _)) =
           let val vr = V.namedvar "setcell"
               val vv = V.namedvar "setval"
               val vu = V.namedvar "setunit"
               fun k valr = convert G (kk valr) v
               and kk valr valv =
                   Primop(PSet, [valr, valv], [],
                          list `
                          Alloc(TUPLE 0, nil, vu,
                                c ` Var vu))
           in
               convert G k r
           end
*)

    (* other primops remain atomic *)

    (* types ignored here. *)
    | convert G c (I.Primapp(po, el, _)) =
           let val v = V.namedvar (Primop.tostring po ^ "_res")
               fun k values = Primop(po, values, list v, list ` c ` Var v)
           in
               convertl G k el
           end


  (* CPS-convert a list, passing the list of values to the continuation *)
  and convertl G c l =
      let fun g (e :: rest) acc = convert G (fn v => g rest (v::acc)) e
            | g nil acc = c (rev acc)
      in g l nil
      end

  (* unless we have top-level definitions of record or sum types (option?),
     we don't need anything in the initial context.

     XXX since we don't have a way in the IL to bind a type abbreviation,
     the context is useless right now.
     *)
  (* nb, requires global uniqueness--so we alpha vary first. *)
  val convert = fn c => fn il => convert Context.empty c (ILAlpha.alphavary il)

end