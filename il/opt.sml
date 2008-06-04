
structure ILOpt :> ILOPT =
struct
    open IL
    structure V = Variable
    structure P = Primop

    val debugopt = Params.flag false
        (SOME ("-debugoptil", 
               "Show lots of optimization info for IL")) "debugoptil"

    val showopt = Params.flag false
        (SOME ("-showopt", 
               "Show each pass of optimization")) "showopt"

    val info = Params.flag false
        (SOME ("-info",
               "Show statistics while compiling")) "info"

    val inline_ctors = Params.flag true
        (SOME ("-inline-ctors",
               "Always inline constructors")) "inlinectors"

    val iloptthresh = Params.param "0"
        (SOME ("-iloptstop",
               "Stop when IL optimization makes this much progress or less")) "iloptthresh"

    exception ILOpt of string

    structure VM = V.Map
    structure OS = Util.Oneshot

    (* things that are no bigger than vars *)
    fun istiny (Int _) = true
      (* | istiny (Char _) = true *)
      | istiny (Polyvar _) = true
    (* strings are tiny because they either get fused to other
       strings (which is good, it saves us expensive concatenation
       at runtime) or are represented by the same pointer ultimately,
       since we coalesce them in the data *)

      (* nullary injections should definitely be inlined; they're
         just little values *)

      | istiny (Inject(_, _, NONE)) = true

      (* string constants should not be inlined; they
         have identity *)
      (* | istiny (String _) = true *)
      | istiny _ = false

    fun constant (Int _) = true
      (* | constant (Char _) = true *)

      (* no! *)
(*      | constant (String _) = true *)
      | constant _ = false

    (* for debugging *)
    fun printfvs m = 
        let in
            print "fvs:\n";
            VM.appi (fn (v, tol) =>
                     print ("  " ^ V.show v ^ ": " ^ 
                            Int.toString (length tol) ^ " entries\n")) m
        end

    fun pushuse info v oneshot =
        (oneshot, VM.insert(info, v, oneshot))

    val progress = ref 0
    fun prog s n = 
        let in
            if !info orelse !debugopt
            then print ("progress: " ^ s ^ "\n")
            else ();
            progress := !progress + n
        end

    fun valuable e = (e, VM.empty, true)

    fun union' s1 s2 = 
        (if !debugopt
         then
             let in
                 print "union (s1):\n";
                 printfvs s1;
                 print "union (s2):\n";
                 printfvs s2;
                 print "result:\n"
             end
         else ();
         let val u = union s1 s2
         in
             if !debugopt
             then
                 let in
                     print "after:\n";
                     printfvs u
                 end
             else ();
                 
             u
         end)

    and union s1 s2 = VM.unionWith (op @) (s1, s2)

    (* re-wrap anything to make it polymorphic *)
    fun repoly nil x = Mono x
      | repoly (h::t) x = Quant (h, repoly t x)

    val warned = ref 10
    fun warnfew s =
        if (!warned > 0)
        then (print s; warned := !warned - 1)
        else ()

    (* remove a variable from the map. because the
       variable is now bound, replace all uses of it
       with the variable itself. 

       (I made this work for poly as well as mono
        variables, since I didn't see any need to
        limit it to mono variables.) *)
    fun remove v m =
        let val (mm, osl) = VM.remove (m, v)
            fun restore (tl, os) = OS.set (os, Polyvar (tl, v))
        in
            app restore osl;
            mm
        end handle LibBase.NotFound => m

    (* hoisted because of dumbass lack of poly-recursion. *)
    (* as deadcodel below, but preserves a #1 of arbitrary type *)
    fun deadcodlel dc lel =
        let
            fun folder ((l,e),
                        (done, fvs, va)) =
                let
                    val (ee, fvv, vaa) = dc e
                in
                    ((l,ee)::done, union fvs fvv, va andalso vaa)
                end
        in
           foldr folder (nil, VM.empty, true) lel
        end

    infix ++
    fun G ++ (v, b) = VM.insert(G, v, b)

    (* partitions a list of functions into a list of
       lists, which we'll call bundles.

       in each bundle, the function definitions may
       only make reference to functions that appear
       AFTER them (or within the same bundle). 
       So the first bundle of functions is the
       *last* one bound in the code. *)
    (* PERF conservative *)
    fun partition_mrec fl = [ fl ]
    (* To do this, find cycles, collapse them into
       equivalence classes, and then do topological
       sort. *)

    (* (deadcode is not a good name for this,
       since it does much more than dead code
       elimination)

       deadcode G e
       G is info for each variable (right now,
             it just marks if a function is total,
             for propagation of totality)
       e is an il expression
       returns (ee, freevars, valuable)

       valuable is true if the expression is
       a value or can be evaluated to a value
       without any effect

       side-effects the progress counter prog
       to reflect the amount of reduction done

       Caller must fill each oneshot in the
       returned free-variable set with the
       variable itself. *)
    fun deadcode G exp =
        (case exp of
             Polyvar (tl, v) =>
                 (* we only ever set oneshots with
                    valuable expressions *)
                 let val os = OS.oneshot ()
                 in (Deferred os, VM.insert(VM.empty, v, [(tl, os)]), true)
                 end
           | Deferred os =>
                 (case OS.deref os of
                      SOME e => deadcode G e
                    | NONE => raise ILOpt "unset oneshot in deadcode")
           | Int _ => valuable exp
           | String _ => valuable exp
           (* | Char _ => valuable exp *)

           | Throw(e1, e2) =>
                    let
                      val (ee1, fv1, _) = deadcode G e1
                      val (ee2, fv2, _) = deadcode G e2
                    in
                      (* never valuable *)
                      (Throw(ee1, ee2), union fv1 fv2, false)
                    end

           | Letcc(v, t, e) =>
                    let
                      val (ee, fv, va) = deadcode (G ++ (v, false)) e
                      val fvh = remove v fv
                    in
                      (* has an effect of binding the current continuation,
                         so shouldn't propagate valuability (??), consider

                         letcc u
                         in (fn x => someref := u)
                         end 

                         anyway letcc is tricky, don't try to be clever
                         *)
                         
                      (Letcc(v, t, ee), fvh, false)
                    end

           | Jointext nil => (prog "jt nil" 5; valuable (String ""))
           | Jointext [e] => (prog "jt single" 5; deadcode G e)
           | Jointext el =>
                 let
                     val (ees, fv, va) = deadcodel G el

                     (* val ees = joinflatten ees *)
                 in
                     (Jointext ees, fv, va)
                 end

           | Record lel =>
                 let
                     val (lell, fv, va) = deadcodlel (deadcode G) lel
                 in
                     (Record lell, fv, va)
                 end

           | Proj (l, t, e) =>
                 let
                     val (ee, fv, va) = deadcode G e
                 in
                     (Proj(l, t, ee), fv, va)
                 end

           | Raise (t, e) => 
                 let
                     val (ee, fv, _) = deadcode G e
                 in
                     (Raise(t, ee), fv, false)
                 end

           | Handle (e, v, handler) =>
                 let
                     val (ee, fv, va) = deadcode G e
                 in
                     (* if the body is valuable, then it can't
                        raise an exception, so simplify *)
                     if va then (prog "handle" 100; (ee, fv, true))
                     else 
                         let
                             val (eh, fvh, _) = 
                                 deadcode (G ++ (v, false)) handler

                             val fvh = remove v fvh
                         in
                             (Handle(ee, v, eh), union fv fvh,
                              false)
                         end
                 end

          | Seq (e1, e2) =>
                 let
                     val (ee, fv, va) = deadcode G e1
                 in
                     (* if the first is valuable, ignore it *)
                     if va then (prog "seq" 100; deadcode G e2)
                     else
                         let
                             val (ee2, fv2, _) = deadcode G e2
                         in
                             makeseq ee ee2 (union fv fv2)
                         end
                 end

          | Roll (t, e) => 
                 let
                     val (ee, fv, va) = deadcode G e
                 in
                     (Roll(t, ee), fv, va)
                 end

          | Unroll e =>
                 let
                     val (ee, fv, va) = deadcode G e
                 in
                     (Unroll ee, fv, va)
                 end

          | Tag (e1, e2) =>
                 let
                     val (ee1, fv1, va1) = deadcode G e1
                     val (ee2, fv2, va2) = deadcode G e2
                 in
                     (Tag (ee1, ee2), union fv1 fv2, va1 andalso va2)
                 end

          | Inject(t, l, NONE) => (Inject(t, l, NONE), VM.empty, true)

          | Inject(t, l, SOME e) =>
                 let
                     val (ee, fv, va) = deadcode G e
                 in
                     (Inject(t, l, SOME ee), fv, va)
                 end

          | Primapp(P.PJointext, el, _) => deadcode G (Jointext el)
          | Primapp(po, el, tl) =>
                 let
                     val (eel, fvs, va) = deadcodel G el

                     (* no reductions *)
                     val plain = (Primapp(po, eel, tl), fvs, false)
                     (* maintains valuability *)
                     val vplain = (Primapp(po, eel, tl), fvs, va) 
                 in
                   (* FIXME these operations should be 32-bit unsigned,
                      which (especially for DIV) differs from what Int
                      does by default *)
                      
                     (* PERF: want to actually do reductions
                        here if possible. *)
                     (* XXX we should agree with some "semantics" about
                        overflow when we actually carry out these
                        applications *)
                     (case po of
                          P.B P.PPlus =>
                              (case eel of
                                   [Int 0w0, e] => (prog "plus0" 5;
                                                  (e, fvs, va))
                                 | [e, Int 0w0] => (prog "plus0" 5;
                                                  (e, fvs, va))
                                 | [Int n, Int m] =>
                                       (prog "plus" 5;
                                        valuable (Int (n + m)))
                                 | _ => plain)

                        | P.PNotb => 
                              (case eel of
                                 [Int w] => (prog "notb" 5;
                                              valuable (Int (Word32.notb w)))
                               | _ => plain)

                        | P.B P.PShr => 
                              (case eel of
                                 [Int w, Int f] => (prog "shr" 5;
                                                    valuable (Int 
                                                              (Word32.>>
                                                               (w,
                                                                Word.fromInt
                                                                (Word32.toInt f)))))
                               | _ => plain)

                        | P.B P.PMinus => 
                              (case eel of
                                   [e, Int 0w0] => (prog "minus0" 5;
                                                  (e, fvs, va))
                                 | [Int n, Int m] => 
                                       (prog "minus" 5;
                                        valuable (Int (n - m)))
                                 | _ => plain)

                                 (*
                                 (* PERF off because semantics
                                    don't match udiv instruction *)
                        | P.B P.PDiv => 
                              (case eel of
                                   [_, Int 0w0] => plain
                                 | [e, Int 1w0] => (prog "div1" 5;
                                                    (e, fvs, va))
                                 | [Int n, Int m] =>
                                       (* does SML have the same
                                          semantics for div as aaph? *)
                                       (prog "div" 5;
                                        valuable (Int (n div m)))
                                 | _ => plain)
                                 *)

                        | P.B P.PTimes => 
                              (case eel of
                                   [Int 0w1, e] => (prog "times1" 5;
                                                    (e, fvs, va))
                                 | [e, Int 0w1] => (prog "times1" 5;
                                                    (e, fvs, va))
                                 | [Int 0w0, e] => (prog "times0" 5;
                                                    (Seq(e, Int 0w0),
                                                   fvs, va))
                                 | [e, Int 0w0] => (prog "times0" 5;
                                                    (Seq(e, Int 0w0),
                                                   fvs, va))
                                 | [Int n, Int m] =>
                                       (prog "times" 5;
                                        valuable (Int (n * m)))
                                 | _ => plain)

                        | P.B (P.PCmp _) =>
                              (case eel of
                                   [Int i, Int j] => 
                                       (* PERF need context to create bools *)
                                       vplain
                                 | _ => vplain)

                        | P.PEqs => vplain

                        | P.PSet => plain
                        (* PERF can be ignored, but not substituted *)
                        | P.PRef => plain
                        (* PERF can be ignored, but not substituted *)
                        | P.PGet => plain

                        (* I/O can't be optimized *)
                        | P.PPutc _ => plain
                        | P.PGetc _ => plain
                        | P.PAvail _ => plain
                        | P.PSetStatus => plain
                        | P.PSetTestpoint => plain
                        | P.PSetCounter => plain
                        | P.PSleep => plain
                                     
                        | P.PFromSeconds => vplain

                        (* can treat this like a value since it
                           is not actually possible to update it, and
                           we don't have equality on arrays *)
                        | P.PArray0 => vplain
                        (* PERF can be ignored, but not substituted *)
                        | P.PArray => plain

                        | P.PNative _ => plain

                        (* PERF could remove its arguments. *)
                        | P.PHalt => plain

                        (* another phase substitutes these out *)
                        | P.PBind => vplain

                        (* shouldn't ever optimize these away *)
                        | P.PCompileWarn _ => plain

                        (* can't reduce during compilation, but is total
                           at runtime *)
                        | P.PDynamic => vplain

                        | _ =>
                              let in
                                  if List.all constant eel
                                  then 
                                      warnfew ("Stupid: Not optimizing " ^ 
                                               "constant " ^
                                               P.tostring po ^ "\n")
                                  else ();
                                  plain
                              end)
                 end

          | App (f, el) =>
                 let

                     val (ff, fvs1, _) = deadcode G f
                     val (eel, fvs2, vbl) = deadcodel G el

                     val nope =
                         (App(ff, eel), union fvs1 fvs2, false)
                 in
                     (case f of
                          Polyvar(_, v) =>
                              (* check if it's total *)
                              (case VM.find(G, v) of
                                   SOME true =>
                                       (App(ff, eel),
                                        union fvs1 fvs2,
                                        (* app is valuable if arg
                                           is valuable *)
                                        vbl)
                                 (* XX bug if not found? *)
                                 | _ => nope)
                        | _ => nope)
                 end

          | Sumcase (t, e, v, lel, def) =>
                 let
                     val (ee, fvs, va) = deadcode G e

                     val armG = G ++ (v, false)
                     val (llel, fvs2, _) = deadcodlel (deadcode armG) lel

                     val fvs2 = remove v fvs2

                     (* XXX could simplify, thought it was optional *)
                     val (fv, dd) =
                         let val (dee, fv, _) = deadcode armG def
                             val fv = remove v fv
                         in (union fv (union fvs fvs2), dee)
                         end
                 in
                     (* XXX reduce: if va and e is a roll(inject(...)) *)
                     (* could expand definition of valuability: if
                        everything is valuable, then I guess this is
                        valuable too *)
                     (Sumcase(t, ee, v, llel, dd), fv, false)
                 end

          | Tagcase (t, e, v, vel, def) =>
                 let
                     (* vars in arms are expression variables,
                        not bindings. (But they cannot be substituted
                        because we have nowhere to substitute..) *)
                     val (ee, fv, _) = deadcode G e
                     val armG = G ++ (v, false)
                     val (vvel, fv2, _) = deadcodlel (deadcode armG) vel

                     val fv2 = remove v fv2

                     (* XXX is var bound in default as above??? *)
                     val (de, fv3, _) = deadcode armG def
                     val fv3 = remove v fv3

                     val vars = map #1 vel

                     val varset =
                       foldl (fn (v, b) => 
                              let
                                (* shouldn't ever set these, since
                                   we can't substitute. *)
                                val os = Util.Oneshot.oneshot ()
                              in
                                Util.Oneshot.wrap
                                (fn x =>
                                 if (case x of
                                       Polyvar (nil, v') => V.eq (v, v')
                                     | _ => false)
                                 then x
                                 else 
                                   raise ILOpt 
                                     ("can't substitute for exn tag " ^
                                      V.tostring v)) os;
                                VM.insert(b, v, [(nil, os)])
                              end) VM.empty vars
                 in
                     (Tagcase(t, ee, v, vvel, de),
                      union (union fv (union fv2 fv3)) varset,
                      false)
                 end

          | Let(Do e1, e2) => deadcode G (Seq(e1, e2))
          | Let(Tagtype v, e) =>
                 let
                     val (ee, fv, va) = deadcode G e
                 in
                     (Let(Tagtype v, ee), fv, va)
                 end
          | Let(Newtag(v, t, vv), e) =>
                 let
                     val (ee, fv, va) = deadcode G e
                 in
                     (* if the tag is never used, erase it *)
                     (* XXX could substitute let newtag... end around
                        the use of the tag var, if there is exactly one. *)
                     (case VM.find(fv, v) of
                          NONE => (prog "newtag" 10; (ee, fv, va))
                        | SOME _ =>
                              (Let(Newtag(v, t, vv), ee),
                               remove v fv,
                               false))
                 end
          (* just hoisted because of its size *)
          | Let(Val vtep, rest) => deadletval G vtep rest

          | Let(Fix flp, rest) => deadletfix G flp rest)

    and deadletfix G flp rest =
      let
          fun unpoly acc (Mono fl) = (rev acc, fl)
            | unpoly acc (Quant (tv, p)) = unpoly (tv :: acc) p

          val (tvs, fl) = unpoly nil flp
          val ntvs = length tvs

          (* can assume these are total when processing the
             body, if they are marked total and not recursive *)
          val G = foldl (fn ({name, recu, total, ...},
                             G) =>
                         G ++ (name, not recu andalso total))
                        G fl

          val (eer, fvr, varest) = deadcode G rest

          fun onebod {name, arg, dom, cod, body, inline=_, recu=_, total=_} =
            let
              val _ = length arg = length dom orelse
                raise ILOpt "|function args| <> |dom typ| (mutual)"
                  
              (* add arguments to context; they aren't total *)
              val G = foldl (fn (a, G) => G ++ (a, false)) G arg
              val (ebody, fvb, vabod) = deadcode G body
                
              (* this is the binding position for the function and
                 its arguments. We can't inline in the body, so just
                 replace them. *)
              val fv = foldr (Util.uncurry remove) fvb (name :: arg)
            in
              (fvb, fv, (* total if body is valuable *) vabod, ebody)
            end
            
      in
          (* only try to optimize a single function *)
          (case fl of
             (* lonely tropical vacation! *)
               nil => raise ILOpt "empty fun island?!"
             | ({...} :: {...} :: _) =>
                 (* mutual case. this is hard, so just try to
                    break up functions that aren't actually
                    mutually recursive, and then let the single
                    function case take care of the rest. If you
                    want the optimizer to clean up your code,
                    don't write mutually recursive functions! *)
             let
               (* G already has the bindings for the functions
                  themselves. *)
               fun translateone (f as {name, arg, dom, cod, body, inline, 
                                       recu=_, total=_}) =
                 let
                   val (_, fv, total, ebody) = onebod f
                   val recu = true (* XXX conservative; should maybe
                                      patch this later *)
                 in
                   (* just return new function decl and its free variables;
                      valuability is meaningless for a decl *)
                   (name, {name=name, arg=arg, dom=dom, cod=cod, inline=inline,
                           body=ebody, recu=recu, total=total}, fv)
                 end

               (* translate each body; simple... *)
               val fl = map translateone fl

               (* partition the list of functions into a list of lists;
                  these will be the resulting islands. [[all]] is a valid
                  conservative answer. *)
               val parts = partition_mrec fl

               (* definitely want to run again if we successfully broke
                  up the mutually-recursive island. *)
               val _ = 
                 if length parts > 1
                 then prog "broke-mutual" 1000
                 else ()

               val () = 
                 if !debugopt
                 then
                   let in
                     print "Partitions:\n";
                     app (fn l => 
                          let in
                            app (fn (name, _, _) =>
                                 print (Variable.tostring name ^ " ")) l;
                            print "\n"
                          end) parts
                   end
                 else ()

               (* need to remove recursive calls. *)

               (* now build up the nested series of mutually recursive
                  Fixes. the final argument is the partition list, which
                  must run from lastly defined to firstly defined. *)
               fun partit fv_rest e_rest nil = (e_rest, fv_rest, varest)
                 | partit fv_rest e_rest (nil :: _) = 
                 raise ILOpt "empty island after partitioning"
                 | partit fv_rest e_rest ( (funs as ({1=_, 2=_, 3=_} :: _)) :: more ) =
                 let

                   (* PERF could check if none of these names
                      appear in the rest at all; if so, drop the
                      whole island *)

                   (* rebuild a fix. Each one will use the original
                      set of type variables. *)
                   val fix = (repoly tvs (map #2 funs))

                   val fv = foldl (Util.uncurry union) fv_rest (map #3 funs)

                   (* at this point, fv covers the entire scope of each
                      function being defined, including the rest. remove
                      them. *)
                   val fv = foldl (Util.uncurry remove) fv (map #1 funs)
                 in
                   partit fv (Let(Fix fix, e_rest)) more
                 end

             in       
               partit fvr eer parts
             end
             | [f as {name, arg, dom, cod, body, inline, ...}] =>
             let
               val (fvb, fv, total, ebody) = onebod f

               (* ?!?! *)
               val recu = isSome (VM.find (fvb, name))
                                 
               val _ = if recu andalso !debugopt 
                       then print ("Setting " ^ V.show name ^ " recu\n")
                       else ()

             in
               (let
                    (* check how many times the fn is used
                       (in the rest; recursive calls don't
                       count) *)
                    val (fvr, info) = VM.remove(fvr, name)

                    fun fill (utvs, os) =
                        if length utvs = ntvs
                        then OS.set (os, Polyvar(utvs, name))
                        else raise ILOpt 
                            ("type error: poly fun " ^
                             V.show name ^ 
                             " used with wrong number of type args")

                    val fix = 
                        (repoly tvs [{name=name,arg=arg,dom=dom,
                                      cod=cod,body=ebody,inline=inline,
                                      recu=recu, total=total}])

                    (* our canonical inlined format
                       looks itself inlinable -- don't do it *)
                    fun already_inlined (utvs, os) =
                        (case eer of
                             Deferred oos => OS.eq(os, oos)
                            | _ => false)
                in
                    (* inline functions if used once
                       a different pass beta-reduces *)
                  
                       (* XXX inlining ctors always doesn't help! *)
                    if ((length info = 1) orelse (inline andalso !inline_ctors))
                       andalso not (List.exists already_inlined info)
                    then (* inline! *)
                      let 
                          fun inline (utvs, os) =
                              if length utvs = ntvs
                              then (prog ("inline-fn " ^ V.show name) 1000;
                                    let 
                                      val orig = 
                                        (Let(Fix fix,
                                             Polyvar (utvs, name)))

                                      (* whenever we duplicate a term, we have to
                                         alpha-vary it to preserve globally unique
                                         binding sites. This is not easy here, because
                                         we have imperative oneshots around each open
                                         variable in the term. But since we only
                                         duplicate constructor functions for the moment, 
                                         we can assume they are closed. *)
                                      val e = 
                                      if (length info > 1)
                                      then 
                                        (* Oneshot must be set by this point! *)
                                        (ILAlpha.alphavary orig)
                                         handle ILAlpha.Alpha s =>
                                           raise ILOpt ("tried to multi-inline an open function: " ^
                                                        V.tostring name ^ "(" ^ s ^ ")")
                                      else orig

                                    in
                                      OS.set (os, e)
                                    end)
                              else raise ILOpt
                                  ("type error: (inlining) poly fun " ^
                                   V.show name ^ 
                                   " used with wrong number of type args")

                      in
                          app inline info;
                          (* keep around the function definition--if
                             we don't need it, it'll be erased on the
                             next pass.

                             (should only do this if we inline only
                             to application positions.) *)
                          (eer, (* no! Let(Fix fix, eer), *)
                           union fv fvr, varest)
                      end
                    else
                      let in
                          app fill info;
                          (Let(Fix fix, eer),
                           union fv fvr,
                           (* the fn def is always valuable *)
                           varest)
                      end
                end) handle LibBase.NotFound =>
                    (* unused (non-recursively) functions 
                       can always be removed *)
                    (prog ("unused fn " ^ V.show name) 150; 
                     (eer, fvr, varest))
             end)
      end

    (* same, for a list of expressions in the same scope *)
    and deadcodel G el =
        let
            fun folder (e, (es, fvs, vas)) =
                let
                    val (ee, fv, va) = deadcode G e
                in
                    (ee :: es, union fv fvs, va andalso vas)
                end
        in
            foldr folder (nil, VM.empty, true) el
        end

    and deadletval G vtep rest =
        let
            fun unpoly acc (Mono (v, t, e)) = (rev acc, v, t, e)
              | unpoly acc (Quant (tv, p)) = unpoly (tv :: acc) p
                
            val (tvs, v, t, e) = unpoly nil vtep
            val ntvs = length tvs
                
            val restG = G ++ (v, false)

            val (eer, fvr, varest) = deadcode restG rest
                
            val (ee, fv, va) = deadcode G e
        in
            (let
                 (* check how many times the var is used *)
                 val (fvr, info) = VM.remove(fvr, v)
                     
                 fun fill (utvs, os) =
                     if length utvs = ntvs
                     then OS.set (os, Polyvar(utvs, v))
                     else raise ILOpt 
                         ("type error: poly var " ^
                          V.show v ^ 
                          " used with wrong number of args")
             in
                 (* if it's valuable, it might be inlined.
                    but we don't want to duplicate large
                    values. so it must be used once,
                    or it must be tiny. *)
                 
                 if va andalso (length info = 1 orelse istiny ee)
                 then
                     let
                         fun inline (utvs, os) =
                             let val s = 
                                 Subst.fromlist
                                 (ListPair.zip (tvs, utvs))
                             in
                                 if length utvs = ntvs
                                 then OS.set (os, ILUtil.tsubste s ee)
                                 else raise ILOpt
                                     ("type err: poly var " ^
                                      V.show v ^
                                      " wrong number of args " ^
                                      " when inlining")
                             end
                     in
                         if !debugopt
                         then print ("inline small/once " ^ V.show v ^ "\n")
                         else ();
                         app inline info;
                         (* ee is not necessarily closed. *)
                         (eer, union fv fvr, varest)
                     end
                 else
                     (* also assume any Deferred exp is actually
                        another var. It should be unset. We can
                        duplicate it any number of times, as long
                        as we ensure it is not itself instantiated
                        with something big. We do this by duplicating
                        it in the freevar set *)
                     (case ee of
                          Deferred osvar =>
                              let 
                                  (* actually a pain to find out what
                                     var this is (and its polymorphic
                                     arity) -- use oneshot identity *)
                                  fun finder (v, info, NONE) =
                                      (case ListUtil.example
                                           (fn (_, os) => 
                                            OS.eq(osvar, os)) info of 
                                           SOME (uu, _) => SOME (v, length uu)
                                         | NONE => NONE)
                                    | finder (_, _, a) = a

                                  (* both will be needed later *)
                                  val freevars = union fvr fv

                                  (* it should appear somewhere in
                                     our freevar-rest *)
                                  val (thevar, arity) = 
                                      case VM.foldli finder NONE freevars of
                                          SOME x => x
                                        | NONE =>
                                              raise ILOpt
                                              ("deadcode bug: " ^
                                               "saw oneshot, but it " ^
                                               "wasn't in freevar set.")

                                  (* same as inline above. *)
                                  fun inlinevar (utvs, os) =
                                      let val s = 
                                          Subst.fromlist
                                          (ListPair.zip (tvs, utvs))
                                      in
                                          (* subst is smart enough to
                                             delay the subst around
                                             ee (which is itself a
                                             delayed expression) *)
                                          if length utvs = ntvs
                                          then OS.set (os, ILUtil.tsubste s ee)
                                          else raise ILOpt
                                              ("type err: poly var " ^
                                               V.show v ^
                                               " wrong number of args " ^
                                               " when inlining var")
                                      end

                                  (* here's the only difference between
                                     this and the above. we need to
                                     generate bogus appearances of this
                                     var so that it is not instantiated
                                     with a big thing. *)
                                  val bogus =
                                      foldl (fn (_, m) =>
                                             union m (VM.insert
                                                      (VM.empty,
                                                       thevar,
                                                       [(List.tabulate
                                                         (arity,
                                                          fn _ => 
                                                          IL.TRec nil),
                                                         OS.oneshot ())])))
                                      VM.empty [1,2,3,4]
                              in
                                  if !debugopt
                                  then 
                                      print ("found " ^ V.show v ^
                                             " = " ^ V.show thevar ^ "\n")
                                  else ();

                                  prog "varsub" 50;
                                  app inlinevar info;
                                  (eer,
                                   union freevars bogus,
                                   varest)
                              end
                        | _ => 
                              (* normal used variable *)
                              let in
                                  
                                  if !debugopt
                                  then print (V.show v ^ " is used.\n")
                                  else ();

                                  app fill info;
                                  makeletval tvs (v, t, ee) eer fv fvr va varest
                              end)
             end) handle LibBase.NotFound =>
                 (* never used! *)
                 if va
                 then (prog ("dead val " ^ V.show v) 100;
                       
                       (* print "bound to: \n";
                          Layout.print (ILPrint.etol ee, print);
                          print "\n\n";
                          
                          print "so return body: \n";
                          Layout.print (ILPrint.etol eer, print);
                          print "\n\n"; *)
                       
                       (eer, fvr, varest))
                 else (prog ("unused var " ^ V.show v) 10; 
                       (Seq(ee, eer), union fv fvr, false))
        end

    (* also, try to flatten successive seqs

       (e1; e2); ee2

       becomes:

       e1; e2; ee2;
       
       .. this can allow us to remove e2 if it is
       valuable but e1 and ee2 are not. *)
    and makeseq ee ee2 fvs =
        (case ee of
             Seq (e1, e2) =>
                 let in
                     prog "flatten seq" 100;
                     (Seq(e1, Seq(e2, ee2)), fvs, false)
                 end
           | _ => (Seq(ee, ee2), fvs, false))

    (* if we're just going to put it back together,
       we might be able to at least flatten the let. *)
    and makeletval tvs (v, t, e) erest fv fvrest va varest =
        let 
            exception Don't of string
            val nothing = 
                (Let(Val (repoly tvs (v, t, e)),
                     erest),
                 union fv fvrest,
                 va andalso varest)
        in
            (case e of
                 Let(dec, e) =>
                     let 
                         (* don't care about anything but var names here *)
                         fun unpoly (Mono thing) = thing
                           | unpoly (Quant (_, more)) = unpoly more

                         (* the vars bound by this decl *)
                         val vv = 
                             case dec of
                                 Do _ => []
                               | Fix rlp => raise Don't "don't move fns"
                               | Val vtep => [#1 (unpoly vtep)]
                               | Tagtype _ => raise Don't "don't move tagtypes"
                               | Newtag (v, _, _) => [v]
                     in
                         (* XXX can introduce shadowing if bindings are not unique:

                            val x =
                              let y = 1
                              in (y, y)
                              end
                            
                            val z = 
                              let y = 2
                              in (y, y)
                              end
                              
                              ==>

                            val y = 1
                            val x = (y, y)
                            
                            val y = 2
                            val z = (y, y)
                            
                            *)
                         (* if it's free in the rest, don't transform *)
                         if List.exists (fn v =>
                                         isSome (VM.find (fvrest, v))) vv
                         then raise Don't "don't if var is free"
                         else
                             let 
                                 (* this doesn't affect the free vars
                                    or valuability *)
                                 val (bod, _, _) = 
                                     makeletval tvs (v, t, e) erest fv fvrest va varest
                             in
                                 prog ("flatten letlet(over " ^ V.show v ^ ")") 100;
                                 (* transform! *)
                                 (Let(dec, bod),
                                  union fv fvrest,
                                  va andalso varest)
                             end
                     end
             | _ => nothing) handle Don't s => 
                 let in
                     if !debugopt then print ("Don't: " ^ s ^ "\n") else ();
                     nothing
                 end
        end

    (* when we see a tuple bound to a variable,
       x = (e1, e2, e3)
       consider replacing it with
       
       x1 = e1
       x2 = e2
       x3 = e3 

       (Note this works even if en is effectful, but
        not if the tuple escapes.)

       First, put it in a table of "target" tuples, along
       with the variables x1,x2,x3.
       
       check the body of the let. 
       for each projection of label n from the variable found,
          make a oneshot and say tentatively that the os will
          be filled with xn. (but keep around the projection)

       if we see the variable x alone (not in a projection),
       then call the whole thing off. for each oneshot we've
       already created, set it with the old projection.

       *)

    fun untuple m exp = 
        let val self = untuple m
        in
         (case exp of
              Let(Val vtep, body) =>
                  let
                      fun unpoly acc (Mono (v, t, e)) = (rev acc, v, t, e)
                        | unpoly acc (Quant (tv, p)) = unpoly (tv :: acc) p
                      val (tvs, v, t, e) = unpoly nil vtep

                  in
                      (case e of
                           (* must be a record *)
                           Record lel =>
                               let
                                   val ltl = (case ILUtil.unevar t of
                                                  TRec ltl => ltl
                                                | ot => 
                                                      let in
                                                          print ("BUG: record binding did " ^
                                                                 "not have record type:\n");
                                                          Layout.print (ILPrint.ttol ot, print);
                                                          raise ILOpt "bad IL program"
                                                      end)

                                   val lvtel =
                                       map (fn (l, e) =>
                                            (l, 
                                             (V.namedvar 
                                              (V.show v ^ "_#" ^ l),
                                              (* XXX ty *)
                                              (case ListUtil.Alist.find op=
                                                   ltl l of
                                                   SOME t => t
                                                 | NONE => raise ILOpt
                                                       ("at record binding " ^
                                                        "type doesn't agree " ^
                                                        "with exp")),
                                                   (* before changing map *)
                                                   self e))) lel
                                       
                                   (* will be set true if we encounter 
                                      an escaping use of v *)
                                   val escapes = ref false

                                   (* list of functions to call with
                                      the ultimate value of !escapes *)
                                   val uses = ref nil :
                                       (bool -> unit) list ref

                                   val newmap =
                                       VM.insert(m,
                                                 v,
                                                 { arity = length tvs,
                                                   lvtel = lvtel,
                                                   escapes = escapes,
                                                   uses = uses })

                                   val result = untuple newmap body
                               in
                                   (* now that we know if we'll be doing
                                      the optimization or not, run the
                                      deciding functions *)
                                   app (fn f => f (!escapes)) (!uses);

                                   if !escapes
                                   (* not doing the opt *)
                                   then Let(Val (repoly tvs 
                                                 (v, t, 
                                                  Record(map (fn (l, (_, _, e)) =>
                                                              (l, e))
                                                             lvtel))),
                                            result)
                                   else
                                       (* doing the opt! *)
                                       let
                                           (* create x_1 ... x_n,
                                              and drop x *)
                                           fun makebinds nil = result
                                             | makebinds ((_, (vp, tf, e))::rest) =
                                               Let(Val(repoly tvs
                                                       (vp,
                                                        tf,
                                                        e)),
                                                   makebinds rest)
                                       in
                                           prog ("untuple " ^ V.show v)
                                             (100 * length lvtel);
                                             
                                           makebinds lvtel
                                       end
                               end
                         | _ => ILUtil.pointwise self exp)
                  end
            | Polyvar(tl, v) =>
                  (case VM.find(m, v) of
                       NONE => exp
                           (* could also typecheck arity *)
                     | SOME { escapes, ... } => (escapes := true; exp))
            | Proj(l, t, Polyvar(tl, v)) => 
                  (case VM.find(m, v) of
                       (* if we already know it escapes, 
                          don't bother doing anything. *)
                       SOME { arity, lvtel, escapes = ref false, uses } => 
                           let
                               (* XXX since we have the actual exp here,
                                  if it is tiny we could substitute it in,
                                  even if the tuple escapes. this probably 
                                  wins *)
                               val os = OS.oneshot ()
                               val targetvar =
                                   (case ListUtil.Alist.find op= lvtel l of
                                        NONE => raise ILOpt 
                                            ("bad label at proj site from " ^
                                             "known tuple..!")
                                      | SOME (v, _, _) => v)

                               fun thisuse true = OS.set (os, exp)
                                 | thisuse false = 
                                   OS.set (os, Polyvar (tl, targetvar))
                           in
                               uses := thisuse :: !uses;
                               Deferred os
                           end
                     (* actually, this does nothing. *)
                     | _ => ILUtil.pointwise self exp)
            | _ => ILUtil.pointwise self exp)

        end

    (* the apply pass reduces function applications of the form
       let fun f x = e
       in f<types>
       end e'

       Since there is no function expression in the il, we treat
       this as the canonical beta redex.
       *)
    fun apply exp =
        (case exp of
             App(Let(Fix flp, Polyvar(tl, v)),
                 args) =>
             let
                 fun unpoly acc (Mono fl) = (rev acc, fl)
                   | unpoly acc (Quant (tv, p)) = unpoly (tv :: acc) p

             in
                 (* must be a single non-recursive function *)
                (case unpoly nil flp of
                     (tvs, [{name, arg : Variable.var list, 
                             dom, cod, body, recu=false, ...}]) =>
                     if V.eq (name, v)
                     then
                         let
                             (* this will be bogus if the lengths don't
                                match, but we check that after *)
                             val s = Subst.fromlist
                                 (ListPair.zip (tvs, tl))
                             val dom = map (Subst.tsubst s) dom
                             (* val cod = Subst.tsubst s cod -- dropped *)
                             val body = ILUtil.tsubste s body

                             (* mklet  fml dom act *)
                             fun mklet nil nil nil = apply body
                               | mklet (v::tv) (t::tt) (a::ta) =
                                 IL.Let(IL.Val (Mono(v : Variable.var, 
                                                     t : IL.typ, 
                                                     a : IL.exp)),
                                        mklet tv tt ta)
                               | mklet _ _ _ =
                                 raise ILOpt ("bad input: formal/dom/actual " ^
                                              "mismatch")
                         in
                             length tvs = length tl
                             orelse raise ILOpt 
                              ("fn exp not isntantiated with " ^
                               "correct number of tyvars");
                              
                             (* inline *)
                              
                             (* instantiate tyvars at tl. also in dom,
                                since we use it to generate val bindings *)
                              prog "beta" 1000;
                              mklet arg dom args
                         end 
                     else ILUtil.pointwise apply exp
               | _ => ILUtil.pointwise apply exp)
             end
                     


           | _ => ILUtil.pointwise apply exp)

    (* eta reduce functions.
       
       let fun f (args) = g (args')
       in e

       where args = args'
       EXCEPT considering x = y whenever the types are unit
       and the argument is a literal unit
        (this arises from the pattern compiler)

       As a simplification we require these to be 
       monomorphic.

       ===>

       let val f = g
       in e

       *)
    fun eta exp =
        (case exp of
             Let(Fix (Mono [{name = f, arg = args : Variable.var list,
                             dom, cod, 
                             body = App(Polyvar(nil, g), args'),
                             recu = false, total, ...}]),
                 letbody) =>
             (* ok. now check condition of arg/args *)
             let
                 fun same_args nil nil nil = true
                   | same_args (_::_) nil _ = raise ILOpt "formal/dom mismatch in eta"
                   | same_args nil (_::_) _ = raise ILOpt "formal/dom mismatch in eta"
                     (* these can be the same if 
                        x' is Polyvar(nil, x),
                        t is unit and x' is () *)
                   | same_args (x::rest) (t::restt) (Polyvar(nil, x')::rest') =
                     V.eq (x, x') andalso same_args rest restt rest'
                   | same_args (x::rest) (TRec nil :: restt) (Record nil :: rest') =
                     same_args rest restt rest'
                   | same_args (_::rest) (_::restt) (_::rest') = false
                   (* not even the same number! *)
                   | same_args nil nil (_::_) = false
                   | same_args (_::_) (_::_) nil = false

             in
                 if same_args args dom args'
                 then
                     let in
                         prog "eta" 1000;
                         Let(Val(Mono(f, Arrow(total, dom, cod), Polyvar(nil, g))),
                             ILUtil.pointwise eta letbody)
                     end
                 else ILUtil.pointwise eta exp
             end
          | _ => ILUtil.pointwise eta exp)


    (* given a bunch of strings to be joined with jointext,
       flatten them as much as possible. 
       
       this could be faster, but it's still much better than
       doing it at runtime!
       *)
    fun joinpass e =
        let 
          val _ = progress := 0
          fun prog _ n = progress := !progress + n
          fun joinflatten nil = nil
            | joinflatten (String "" :: rest) =
            joinflatten (prog "jt unit" 10; rest)
            | joinflatten (String s :: String ss :: rest) =
            joinflatten (prog "jt concat" 40; String (s ^ ss) :: rest)
            | joinflatten (Jointext el :: rest) =
            joinflatten (prog "jt flatj" 100; el @ rest)
            | joinflatten (Primapp(P.PJointext, eel, _) :: rest) =
            joinflatten (prog "jt flatp" 100; eel @ rest)
            | joinflatten (e :: rest) =
            e :: joinflatten rest
            
          fun join exp =
            (case exp of
               Jointext el => Jointext (joinflatten (map (ILUtil.pointwise join) el))
             | _ => ILUtil.pointwise join exp)
          val e = join e
        in
          (e, !progress)
        end


    fun etapass e =
        let val _ = progress := 0
            val e = eta e
        in
            (e, !progress)
        end


    fun deadpass e =
        let 
            val _ = progress := 0
            val (e, fv, _) = deadcode VM.empty e

            (* fill the free variables (external calls)
               allow poly at top-level *)
            fun fill (v, tol) =
                app (fn (utvs, os) =>
                     let in
                         if !debugopt 
                         then print ("Toplevel free " ^ 
                                     V.show v ^ "\n")
                         else ();
                         OS.set (os, Polyvar(utvs, v))
                     end) tol
        in
            VM.appi fill fv;
            (e, !progress)
        end

    fun applypass e =
        let val _ = progress := 0
            val e = apply e
        in
            (e, !progress)
        end

    fun untuplepass e =
        let val _ = progress := 0
            val e = untuple VM.empty e
        in
            (e, !progress)
        end

    (* like the identity, forces deferred *)
    fun immediate exp =
        let 
            fun go e = ILUtil.pointwise go e
        in
            progress := 0;
            (go exp, 0)
        end

    (* cut off eventually even if we're making small progress.
       be smarter -- always do a few passes, even if they have
       low scores at first. 
       *)
    fun optimize e =
        let
            val passes = [("dead code", deadpass), 
                          (* after dead code, always run this
                             to remove 'deferred' exps *)
                          ("immediate", immediate),
                          ("join", joinpass),
                          ("eta", etapass),
                          ("apply", applypass),
                          (* also assumes no 'deferred' exps *)
                          ("untuple", untuplepass)]

            val () = 
              if !debugopt
              then 
                let in
                  print "======= BEFORE OPTIMIZATION ========\n";
                  Layout.print (ILPrint.etol e, print);
                  print "\n\n"
                end
              else ()

            fun opt e =
                let 
                    fun go nil ce sc = 
                      let in
                        print ("(" ^ Int.toString sc ^ ") ");
                        if sc <= Params.asint 0 iloptthresh then ce
                        else opt ce
                      end
                      | go ((pn, pf)::t) ce s =
                        let 
                            val _ = if !info
                                    then print (pn ^ ": ")
                                    else ()
                            val (ne, score) = pf ce
                        in
                            if !info
                            then print ("score " ^ Int.toString score ^ "\n")
                            else ();

                            if !showopt andalso score > 0
                            then 
                                let in
                                    print "after this pass:\n";
                                    Layout.print (ILPrint.etol ne, print);
                                    print "\n\n==================\n\n"
                                end
                            else ();

                            go t ne (s + score)
                        end
                in
                    go passes e 0
                end
            val efinal = opt e
        in
          print "\n";
          efinal
        end handle ILUtil.ILUtil s => raise ILOpt ("from ilutil: " ^ s)

end
