
structure Initial :> INITIAL =
struct

    open Variable
    structure P = Primop

    (* we (should) disallow rebinding of true, false *)

    val truename = "true"
    val falsename = "false"

    val ilint = IL.TVar (namedvar "int")
    val ilsint = IL.TVar (namedvar "sint")
    val ilchar = IL.TVar (namedvar "char")
    val ilstring = IL.TVec ilchar 

    val ilplus = namedvar "plus"

    (* XXX bools should be NONEs *)
    val ilboolsum = IL.Sum[(truename, IL.NonCarrier),
                           (falsename, IL.NonCarrier)]
    val ilbool = IL.Mu(0, [(namedvar "a", ilboolsum)])

    val ilunit = IL.TRec nil

    val cons = 
        [("ref", IL.Lambda (IL.TRef o hd), 1, IL.Regular),
         ("cont", IL.Lambda (IL.TCont o hd), 1, IL.Regular),
         ("array", IL.Lambda (IL.TVec o hd), 1, IL.Regular),
         ("int", IL.Typ ilint, 0, IL.Regular),
         ("string", IL.Typ ilstring, 0, IL.Regular),
         ("char", IL.Typ ilchar, 0, IL.Regular),
         ("unit", IL.Typ (IL.TRec nil), 0, IL.Regular)]

    val a = namedvar "alpha"
    val b = namedvar "beta"

    fun tuple l =
        let
            fun mktup _ nil = nil
              | mktup n (h::t) = (Int.toString n, h) :: mktup (n + 1) t
        in
            IL.TRec(mktup 1 l)
        end

    (* XXX reevaluate totality of these *)

    (* also put these in signature .. ? *)
    val monofuns =
        [
         (* signed *)
         ("<", P.B (P.PCmp P.PLess), [ilint, ilint], ilbool),
         (">", P.B (P.PCmp P.PGreater), [ilint, ilint], ilbool),
         ("<=", P.B (P.PCmp P.PLesseq), [ilint, ilint], ilbool),
         (">=", P.B (P.PCmp P.PGreatereq), [ilint, ilint], ilbool),

         (* array bounds check *)
         ("chk", P.B (P.PCmp P.PBChk), [ilint, ilint], ilbool),

         ("<>", P.B (P.PCmp P.PNeq), [ilint, ilint], ilbool),
         ("=", P.B (P.PCmp P.PEq), [ilint, ilint], ilbool),

         (* XXX floating-point stuff *)

         ("+", P.B P.PPlus, [ilint, ilint], ilint),
         ("-", P.B P.PMinus, [ilint, ilint], ilint),
         ("*", P.B P.PTimes, [ilint, ilint], ilint),
         ("div_", P.B P.PDiv, [ilint, ilint], ilint),
(*
         ("utos", P.PBind, [ilint], ilsint),
*)
         (* ("mod", P.B P.PMod, [ilint, ilint], ilint), *)
(*
         ("s+", P.B P.PPlus, [ilsint, ilsint], ilsint),
         ("s-", P.B P.PMinus, [ilsint, ilsint], ilsint),
         ("s*", P.B P.PTimes, [ilsint, ilsint], ilsint),
*)
         (* XXX change to signed int *)
         ("sdiv_", P.B P.PSDiv, [ilint, ilint], ilint),
(*
         ("stou", P.PBind, [ilsint], ilint),
*)
         (* ("mod", P.B P.PMod, [ilint, ilint], ilint), *)

         ("andb", P.B P.PAndb, [ilint, ilint], ilint),
         ("orb", P.B P.POrb, [ilint, ilint], ilint),
         ("xorb", P.B P.PXorb, [ilint, ilint], ilint),
         ("notb", P.PNotb, [ilint], ilint),
         (* shift (a, b) by b mod 32. *)
         ("shl", P.B P.PShl, [ilint, ilint], ilint),
         ("shr", P.B P.PShr, [ilint, ilint], ilint)

         ]

    val polyfuns =
        [

         ("putc", P.PPutc P.Console, IL.Mono(IL.Arrow(false, [ilchar], tuple nil))),
         ("getc_", P.PGetc P.Console, IL.Mono(IL.Arrow(false, [], ilint))),
         ("availc0", P.PAvail P.Console, IL.Mono(IL.Arrow(false, [], ilint))),

         ("putcs0", P.PPutc P.Serial0, IL.Mono(IL.Arrow(false, [ilchar], tuple nil))),
         ("getcs0_", P.PGetc P.Serial0, IL.Mono(IL.Arrow(false, [], ilint))),
         ("avails0", P.PAvail P.Serial0, IL.Mono(IL.Arrow(false, [], ilint))),

         ("setstatus_", P.PSetStatus, IL.Mono(IL.Arrow(false, [ilchar], tuple nil))),
         ("settestpoint_", P.PSetTestpoint, IL.Mono(IL.Arrow(false, [ilchar], tuple nil))),
         ("setcounter_", P.PSetCounter, IL.Mono(IL.Arrow(false, [ilint], tuple nil))),

         ("fromseconds", P.PFromSeconds, IL.Mono(IL.Arrow(true, [ilint], ilint))),
         ("sleep", P.PSleep, IL.Mono(IL.Arrow(false, [ilint], tuple nil))),

         (* XXX should really be exn cont, but there's no way to
            spell that type here. so make it unit cont and then the
            handler just can't use its argument. *)
         ("sethandler_", P.PSethandler, 
          IL.Mono(IL.Arrow(false, [IL.TCont ilunit], ilunit))),

         (* coercions *)
         ("ord", P.PBind, IL.Mono(IL.Arrow(true, [ilchar], ilint))),
         ("chr_", P.PBind, IL.Mono(IL.Arrow(true, [ilint], ilchar))),
         (* can only safely read 0..Runtime.DYNAMIC_WORDS-1 *)
         ("dynamic_", P.PDynamic, IL.Mono(IL.Arrow(true, [ilint], ilint))),

         ("halt", P.PHalt, IL.Quant(a, IL.Mono(IL.Arrow(false, [], IL.TVar a)))),

         ("showval_", P.PShowval, IL.Quant(a, IL.Mono(IL.Arrow(false, [IL.TVar a], ilunit)))),

         ("^", P.PJointext, IL.Mono(IL.Arrow(false, [IL.TVec ilchar,
                                                     IL.TVec ilchar], IL.TVec ilchar))),

         ("!", P.PGet, IL.Quant(a, IL.Mono
                                (IL.Arrow(false, [IL.TRef (IL.TVar a)],
                                          IL.TVar a)))),

         (":=", P.PSet, IL.Quant(a, IL.Mono
                                 (IL.Arrow(false, [IL.TRef (IL.TVar a),
                                                   IL.TVar a],
                                           tuple nil)))),

         ("ref", P.PRef, IL.Quant(a, IL.Mono
                                  (IL.Arrow(false, [IL.TVar a],
                                            IL.TRef (IL.TVar a))))),

         ("array0", P.PArray0, IL.Quant (a, IL.Mono
                                         (IL.Arrow(true, nil, 
                                                   IL.TVec (IL.TVar a))))),

         ("array", P.PArray, IL.Quant(a, IL.Mono 
                                      (IL.Arrow(false, [ilint, IL.TVar a],
                                                IL.TVec (IL.TVar a))))),
         ("length", P.PArraylength,
            IL.Quant(a, IL.Mono
                     (IL.Arrow(true, [IL.TVec (IL.TVar a)], ilint)))),

         (* unsafe versions *)
         ("sub_", P.PSub, 
            IL.Quant(a, IL.Mono
                     (IL.Arrow(false, [IL.TVec (IL.TVar a), ilint],
                               IL.TVar a)))),

         ("update_", P.PUpdate, 
            IL.Quant(a, IL.Mono
                     (IL.Arrow (false, [IL.TVec (IL.TVar a),
                                        ilint,
                                        IL.TVar a],
                                tuple nil))))
         ]

    val vals =

        map (fn (name, prim, ty) =>
             (name, ty, IL.Primitive prim)) polyfuns @

        map (fn (name, prim, cod, dom) =>
             (name, IL.Mono (IL.Arrow(false, cod, dom)), 
              IL.Primitive prim)) monofuns

    val initialc = foldl (fn ((s, c, k, t), ctx) =>
                          Context.bindc ctx s c k t) Context.empty cons

    val initial = foldl (fn ((s, c, t), ctx) =>
                         Context.bindex ctx NONE s c (namedvar "dummy") t) 
                        initialc vals


    (* XXX infix *)
    val consname = "::"
    val nilname = "nil"
    val caretname = "^"

    val matchname = "Match"

    val exnname = "exn"
    val boolname = "bool"
    val listname = "list"

    (* Wrap an expression with declarations of things that are
       needed by elaboration, like bool and list. *)
    fun wrap (e as (_, loc)) =
        let fun %x = (x, loc)
            fun decbool e =
                %(EL.Let(%(EL.Datatype 
                           (nil, [(boolname, 
                                   [(truename, NONE),
                                    (falsename, NONE)])])),
                         e))

            fun declist e =
                %(EL.Let(%(EL.Datatype
                           (["a"], [(listname,
                                     [(consname, 
                                       SOME(EL.TRec[("1", EL.TVar"a"),
                                                    ("2",
                                                     EL.TVar listname)])),
                                      (nilname, NONE)])])),
                         e))

            fun decexn e =
                %(EL.Let
                  (%(EL.Tagtype exnname),
                   %(EL.Let
                     (%(EL.Exception (matchname, NONE)),
                      e))))

            fun addsig e = 
                %(EL.Let
                  (%(IFunc.toplevel),
                   e))
        in
            (declist o decbool o decexn o addsig) e
        end

    fun trueexp loc = (EL.Var truename,loc)
    fun falseexp loc = (EL.Var falsename, loc)

    (* XXX bools should be NONEs *)      
    val trueexpil  = IL.Roll(ilbool, IL.Inject(ilboolsum, truename,  NONE))
    val falseexpil = IL.Roll(ilbool, IL.Inject(ilboolsum, falsename, NONE))

    val truepat = EL.PApp (truename, NONE)
    val falsepat = EL.PApp (falsename, NONE)

    fun matchexp loc = (EL.App ((EL.Var matchname, loc), 
                                (EL.Record nil, loc)), loc)

end
