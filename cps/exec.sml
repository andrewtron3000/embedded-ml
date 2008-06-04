
(* interpret a closure-converted CPS expression, for debugging only *)

structure CPSExec =
struct

    open CPS
    open Primop

    exception Exec of string
    exception Halt

    structure VM = Variable.Map
    val vs = Variable.tostring
    val itos = Int.toString

    datatype mstring =
        S of string
      (* marshalled *)
      | M of value
      (* submitted task *)
      | T of value

    and value =
        Vint of Word32.word
      | Vtup of value list
      | Vtag of int * value
      | Vlab of Variable.var
      | Varray of value Array.array
      | Vuninitialized
      | Vnull

      | Vfun of { G : value VM.map,
                  f : Variable.var, 
                  args : Variable.var list,
                  ce : CPS.cexp }

    fun vtos Vnull = "NULL"
      | vtos (Varray a) = "{| " ^ StringUtil.delimit ", " (map vtos (Array.foldr op:: nil a)) ^ " |}"
      | vtos (Vlab v) = Variable.tostring v
      | vtos (Vtag (i, v)) = "(tag_" ^ Int.toString i ^ " " ^ vtos v ^ ")"
      | vtos (Vtup vl) = "(" ^ StringUtil.delimit ", " (map vtos vl) ^ ")"
      | vtos (Vint w) = IntInf.toString (Word32.toLargeIntX w)
      | vtos (Vfun _) = "(-closure-)"
      | vtos (Vuninitialized) = "(-uninitialized-)"

    local val ctr = ref 0
    in fun newint () = (ctr := !ctr + 1; Word32.fromInt (!ctr))
    end

    val hvar = Variable.namedvar "_CPSEXEC_handler"

    infix ?
    infix ++

    fun G ++ (v, va) = 
      let in
        (case VM.find (G, v) of
           SOME _ => print ("[CPSEXEC] shadowing " ^ Variable.tostring v ^ "\n")
         | NONE => ());
        VM.insert (G, v, va)
      end
        
    (* hack -- not right, should have global effect *)
    fun sethandler G va = G ++ (hvar, va)

    fun null G nil = G
      | null G (h :: t) = null ( G ++ (h, Vnull) ) t

    fun run G funs ce =
      let

          fun go G e =
            let
                fun err s =
                    let in
                        print ("Error at start of instruction seq:\n" ^
                               CPSPrint.etosi 3 e);
                        print ("\n\nError was: " ^ s ^ "\n");
                        (* XXX print context, etc. *)
                        raise Exec s
                    end

                fun G ? v =
                    (case VM.find (G, v) of
                         SOME va => va
                       | NONE => err 
                             ("variable " ^ vs v ^ " was not bound"))
                         
                fun gethandler G = G ? hvar

                fun value G (Var v) = G ? v
                  | value G (Label l) = Vlab l
                  | value G (Int i) = Vint i
            in
              case e of
                  (Fix ([(f, args, bod)], e')) =>
                    (* XXX should report an error if doing closure conversion *)
(*                  err "code is not closure-converted: encountered a fix" *)
                    let
                      val G = G ++ (f, Vfun { f = f, G = G, args = args, ce = bod })
                    in
                      go G e'
                    end

                | (Fix _) => err "mutual recursion not implemented"

                | (App (va, vl)) =>
                  let

                    fun getf lab =
                      (case VM.find (funs, lab) of
                         SOME (ar, ce) => (lab, G, ar, ce)
                       | NONE => raise Exec ("function " ^ vs lab ^ 
                                             " not def'd?"))

                    val (f, Gclo, ar, ce) =
                      (case va of
                         Label l => getf l
                       | Var v => 
                           (case G ? v of
                                Vlab l => getf l
                              | Vfun { f, G, args, ce } => (f, G, args, ce)
                              | _ => err (vs v ^ 
                                          " not a fun/label"))
                       | _ => err ("value in app not label or var"))

                    (* bind args, bind self recursively *)
                    fun binds G' nil nil = go (G' ++ (f, Vfun { f=f, G=Gclo, args=ar, ce=ce})) ce
                      | binds G' (arg::arest) (va::vrest) =
                        binds (G' ++ (arg, value G va)) arest vrest
                      | binds _ _ _ =
                        err ("wrong number of args")
                  in
                      binds Gclo ar vl
                  end
                | Sumswitch(va, _, v, iel, def) =>
                   (case value G va of
                        Vtag (i, vu) =>
                            (case ListUtil.Alist.find op= iel i of
                                 SOME ee => go (G ++ (v, vu)) ee
                               | NONE => go (G ++ (v, vu)) def)
                      | ob => err ("sumswitch object was " ^ vtos ob))

                | (Alloc(INT, [va], v, ce)) =>
                  go (G ++ (v, case value G va of
                            vv as Vint _ => vv
                          | _ => err ("alloc int at non int")))
                  ce
                | (Alloc (STRING s, [], v, ce)) =>
                  go (G ++ 
                      (v, Varray (Array.fromList (map (Vint o Word32.fromInt o ord) (explode s))))) 
                  ce
                | (Alloc (CODE, [va], v, ce)) =>
                  go (G ++ (v, case value G va of
                            vv as Vlab _ => vv
                          | _ => err ("alloc lab at non lab")))
                  ce
                | (Alloc (INT_T n, [va], v, ce)) =>
                  go (G ++ (v, Vtag (n, value G va))) ce

                | (Alloc (INT_T n, [], v, ce)) =>
                  go (G ++ (v, Vtag (n, Vnull))) ce

                | (Alloc (TUPLE n, vl, v, ce)) =>
                  let in
                      length vl = n
                          orelse err ("alloc tuple of wrong len");
                      go (G ++ (v, Vtup (map (value G) vl))) ce
                  end
                | (Alloc _) => err "bad alloc"
                | (Project (n, va, v, ce)) =>
                  (case value G va of
                       Vtup vl =>
                           let in
                               n < length vl
                                 orelse err ("project " ^ itos n ^
                                             " from tuple of len " ^
                                             itos (length vl));
                               go (G ++ (v, List.nth (vl, n))) ce
                           end
                   | _ => err "projection from non tuple")
                 | (Intswitch _) => err "intswitch not implemented"
                 | (Deferred os) =>
                       (case Util.Oneshot.deref (os()) of
                            NONE => err "unset oneshot!"
                          | SOME e => go G e)

                 | Primop (B(PCmp PLess), [vl, vr], [], [ct, cf]) => 
                       (case (value G vl, value G vr) of
                          (Vint l, Vint r) =>
                            let 
                              val l = Word32.toLargeIntX l
                              val r = Word32.toLargeIntX r
                            in
                              if IntInf.< (l, r)
                              then go G ct
                              else go G cf
                            end
                        | _ => err "need two ints for less")

                 | Primop (PBind, [va], [v], [ce]) =>
                            go (G ++ (v, value G va)) ce

                 | Primop (PNewtag, [], [v], [ce]) =>
                            go (G ++ (v, Vint (newint ()))) ce
                 | Primop (PGethandler, [], [v], [ce]) =>
                            go (G ++ (v, gethandler G)) ce
                 | Primop (PSethandler, [va], vs, [ce]) =>
                            go (sethandler (null G vs) (value G va)) ce

                 | Primop (PSub, [va, vi], [v], [ce]) => 
                            (case (value G va, value G vi) of
                               (Varray a, Vint i) =>
                                 go (G ++ (v, Array.sub(a, Word32.toInt i))) ce
                             | _ => err "bad psub")

                 | (Primop (PArraylength, [va], [v], [ce])) => 
                            (case value G va of
                               Varray a =>
                                 go (G ++ (v, Vint (Word32.fromInt (Array.length a)))) ce
                             | actual => err ("bad parraylength on "
                                              ^ vtos actual))

                 | Primop (PPutc Console, [va], vs, [ce]) => 
                            (case value G va of
                               Vint x =>
                                 (print (implode [chr (Word32.toInt x)]);
                                  go (null G vs) ce)
                             | _ => err "bad putc")

                 | Primop (PNull, [va], [], [z, nz]) => 
                            (case value G va of
                               Vnull => go G z
                             | Vtup [] => go G z
                             | _ => go G nz)

                 | Primop (PArray, [vl, vinit], [v], [ce]) => 
                     (case value G vl of
                        Vint i => 
                          go (G ++ (v, Varray (Array.array(Word32.toInt i, value G vinit)))) ce
                      | _ => err "parray size not int")

                 | Primop (PArray0, [], [v], [ce]) => 
                          go (G ++ (v, Varray (Array.fromList nil))) ce

                 | Primop (PUpdate, [va, vi, vv], vs, [ce]) => 
                     (case (value G va, value G vi) of
                        (Varray a, Vint i) =>
                          let in
                            Array.update(a, Word32.toInt i, value G vv);
                            go (null G vs) ce
                          end
                      | _ => err "bad update")

                 | Primop (PHalt, _, _, _) => (print "\nHALT."; raise Halt)

                 | Primop(PShowval, [va], vs, [ce]) =>
                      let
                        val va = value G va
                      in
                        print ("[SHOWVAL: " ^ vtos va ^ "]\n");
                        go (null G vs) ce
                      end

                 | (Primop (B (PCmp PEq), [a, b], [], [tt, ff])) => 
                    (case (value G a, value G b) of
                         (Vint aa, Vint bb) =>
                             if aa = bb then go G tt
                             else go G ff
                       | _ => err "bad args to eq")

                 | Primop (B po, [a, b], [v], [ce]) =>
                    (case (value G a, value G b) of
                         (Vint aa, Vint bb) =>
                           (case po of
                              PPlus => go (G ++ (v, Vint (Word32.+ (aa, bb)))) ce
                            | _ => err ("unimplemented binop " ^ Primop.tostring (B po)))
                       | _ => err "need ints for binop")

                 | (Primop (po, _, _, _)) =>
                    err ("unimplemented primop " ^ Primop.tostring po)
            end
                        
      in
          go (sethandler G (Vint 0wxDEADBEEF)) ce
      end


    (* execute a closure-converted expression *)
    fun exec_cc (Fix (fs, ce)) =
        let
            val funs =
                foldl (fn ((v, args, e), m) =>
                       VM.insert (m, v, (args, e)))
                      VM.empty fs
        in
            run VM.empty funs ce
        end
      | exec_cc (ce) = run VM.empty VM.empty ce

    fun exec ce = (run VM.empty VM.empty ce) handle Halt => ()

end
