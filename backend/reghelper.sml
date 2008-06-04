
structure RegHelper :> REGHELPER =
struct

  structure A = Array
  structure GA = GrowArray
  structure V = Variable

  fun veq v v' = V.eq (v, v')

  datatype regcontents = 
    Unknown
  | Int of CPS.intconst
(* XXX Label *)

  fun rctos Unknown = "?"
    | rctos (Int i) = Word32.toString i

  open UMA

  datatype lock =
    Unlocked
  | Locked
  (* if NONE, then it can never be freed!!! *)
  | Readonly of int option

  fun ltos Unlocked = "U"
    | ltos Locked = "L"
    | ltos (Readonly _) = "R"

  type regstatus = { contents : regcontents,
                            (* name * needs-to-be-spilled *)
                     vars : (V.var * bool) list,
                     lock : lock }

  fun rstos ({ contents, vars, lock } : regstatus) = 
      (rctos contents) ^ "[" ^ (StringUtil.delimit "," 
                                  (map (fn (v, s) => V.tostring v ^ 
                                                     (if s then "*" else "")) vars)) ^
      "](" ^ (ltos lock) ^ ")"


  structure VVM = CPS.VVM
   (* SplayMapFn(type ord_key = V.var * V.var
                val compare = Util.lex_order V.compare V.compare) *)

  type workingblock = 
    { regs  : regstatus array,
      block : inst GA.growarray,
      (* PERF better rep? *)
      lab : V.var,
      info : (int * bool) VVM.map,
      finalized : bool ref }

  exception RegHelper of string * workingblock
  fun error wb s = raise RegHelper (s, wb)

  val zz = 6

  fun dump ({ regs, block, ... } : workingblock) =
      let in 
        print "regs: ";
        print (A.foldli
                   (fn (r, c, s) => s ^ " " ^ (UMAPrint.regtos r) ^ "=" ^ (rstos c)) 
                   "" regs);
        print "\ncode: \n";
        print ((foldl (fn (inst, s) => s ^ "\n" ^ (UMAPrint.intos inst)) "" (A.foldr op:: nil (GA.finalize block))) ^ "\n");
        print "\n"
      end

  fun empty (info : (int * bool) VVM.map) (lab : V.var) = 
      { regs = A.tabulate(8, fn r => if r = zz then { contents = Int 0w0,
                                                      vars = nil, 
                                                      lock = Readonly NONE }
                                     else { contents = Unknown,
                                            vars = nil,
                                            lock = Unlocked }),
        block = GA.empty (),
        lab = lab,
        info = info,
        finalized = ref false }

  fun lock (wb as { regs, ... } : workingblock) r = 
      A.update (regs, r,
                let val { contents, vars, lock } = A.sub (regs, r) 
                in 
                  { contents = contents,
                    vars = vars,
                    lock = 
                    (case lock of 
                       Unlocked => Locked
                     | _ => let in dump wb; error wb ("can't lock register " ^ (UMAPrint.regtos r)) end ) }
                end)

  fun unlock (wb as { regs, ... } : workingblock) r =
    A.update(regs, r, 
                 let val { contents, vars, lock } = A.sub(regs, r)
                 in
                   { contents = contents,
                     vars = vars,
                     lock =
                     (case lock of
                        Locked => Unlocked
                      | Readonly NONE => Readonly NONE
                      | Readonly (SOME 1) => Unlocked
                      | Readonly (SOME n) => Readonly (SOME (n - 1))
                      | Unlocked => error wb ("unlocked an unlocked reg " ^ (UMAPrint.regtos r))) }
                 end)

  fun spillwithreg (wb as { regs, info, lab, ... } : workingblock) r tmp =
      let fun spill_var (v, true) =
              let in
                case VVM.find (info, (lab, v)) of
                  SOME (n, _) => let val () = emit wb (COMMENT ("spilling " ^ (V.tostring v) 
                                                                ^ " to slot " ^ (Int.toString n)))
                                 in
                                   emit wb (LITERAL (tmp, (Word32.fromInt (Limits.STACK_START + n))));
                                   emit wb (UPD (zz, tmp, r))
                                 end
                | NONE => error wb ("no slot for var " ^ (V.tostring v) ^ "\n")
              end
            | spill_var (_, false) = ()

          val { contents, vars, lock } = A.sub (regs, r) 
      in
        (* XXX spill order? which slot offsets are mostly likely to be used? *)
        app spill_var vars;
        A.update (regs, r, { contents = contents, 
                             vars = List.map (fn (v, _) => (v, false)) vars,
                             lock = lock } )
      end

  and spill wb r =
      let val tmp = getreg wb in
        spillwithreg wb r tmp;
        unlock wb tmp
      end

  and unspillwithreg (wb as { regs, info, lab, ... } : workingblock) v r =
      let in 
        case VVM.find (info, (lab, v)) of
          SOME (n, _) => let val () = emit wb (COMMENT ("unspilling " ^ (V.tostring v) 
                                                        ^ " from slot " ^ (Int.toString n)))
                             val tmp = getlitro wb (Word32.fromInt (Limits.STACK_START + n))
                         in
                           emit wb (ASUB (r, zz, tmp));
                           let val { contents, vars, lock } = A.sub (regs, r) in
                             (* do update after ASUB *)
                             A.update (regs, r, { contents = contents,
                                                  vars = (v, false)::vars,
                                                  lock = lock })
                           end;
                           unlock wb tmp
                         end
        | NONE => error wb ("unspill for unknown var " ^ (V.tostring v))
      end

  and unspill wb v = 
      let val r = getreg wb
      in 
        unspillwithreg wb v r;
        r
      end

  (* Useful registers are exactly those that are unlocked and that we can use
    without any spilling. *)
    and isuseful regs r = 
      let val { contents, vars, lock } = A.sub (regs, r) in 
        (not (List.exists (fn (_, x) => x) vars))
        andalso
        (case lock of Unlocked => true
                    | _ => false)
      end

  and tryreg (wb as { regs, ... } : workingblock) r =
      let 
        val rs = unlockedregs regs
        val rs = List.filter (fn r' => r <> r') rs

      val rs = map (fn x => 
                       let val { contents, vars, lock } = A.sub (regs, x)
                       in
                         (x, 
                          (* Count how many variables would be spilled if we
                           were to reuse this register but still prefer reg
                           with no vars over those that have already been
                           spilled *)
                          (List.foldl (fn ((_, true), s) => 3 + s
                                        | ((_, false), s) => 1 + s) 0 vars,
                           contents))
                       end) rs

      val rs = ListUtil.sort (ListUtil.bysecond 
                               (Util.lex_order
                                 Int.compare
                                 (fn (Unknown, Unknown) => EQUAL
                                   | (Unknown, _) => LESS
                                   | (_, Unknown) => GREATER
                                   (* larger constants are more useless 
                                      because they are less common *)
                                   | (Int x, Int y) => Word32.compare (y, x))))
                             rs
      val rs = map #1 rs

      in
        (* if the best register needs to be spilled, we are in trouble *)
        if isuseful regs r then ()
        else error wb ("need to spill " ^ (UMAPrint.regtos r) 
                       ^ " but no other registers to help:" 
                       ^ (A.foldli (fn (r, rs, s) => s ^ " " 
                                                     ^ (UMAPrint.regtos r) ^ "="
                                                     ^ (rstos rs)) "" regs));
        
        (* lock this register *)
        lock wb r;

        (* take this opportunity to spill if necessary *)
        (case (List.filter (isuseful regs) rs, rs)
          of (nil, r' :: _) => 
             (* No other useful registers, so use the
               current one to spill the next best
               register, and the return the latter *)
             let in
               (* emit wb (COMMENT ("was going to allocate " ^ (UMAPrint.regtos r) ^ "=" ^ (rstos (A.sub(regs, r))) ^ " (the last useful one) so spilling " ^ (UMAPrint.regtos r') ^ "=" ^ (rstos (A.sub(regs, r'))) ^ " so it will be useful too")); *)
               lock wb r';
               spillwithreg wb r' r;
               (* unlock the first one *)
               unlock wb r';
               r
             end
           | _ => r)
      end

  and unlockedregs regs =
      let val rs = List.tabulate(8, fn x => x)
          val rs = List.filter (fn r => 
                                   case #lock(A.sub (regs, r)) of
                                     Unlocked => true | _ => false) rs
      in
        rs
      end

  and usefulregs regs =
      let val rs = List.tabulate(8, fn x => x)
          val rs = List.filter (isuseful regs) rs
      in
        rs
      end


  (* here we have to find an unlocked register
     and return it, so we just do that. 

     for now we just greedily choose a register
     based on its contents. *)
  and getreg (wb as { regs, ... } : workingblock) =
    let 
        val rs = unlockedregs regs
        val rs = map (fn x => 
                       let val { contents, vars, lock } = A.sub (regs, x)
                       in
                         (x, 
                          (* Count how many variables would be spilled if we
                           were to reuse this register but still prefer reg
                           with no vars over those that have already been
                           spilled *)
                          (List.foldl (fn ((_, true), s) => 2 + s
                                        | ((_, false), s) => 1 + s) 0 vars,
                           contents))
                       end) rs

      val rs = ListUtil.sort (ListUtil.bysecond 
                               (Util.lex_order
                                 Int.compare
                                 (fn (Unknown, Unknown) => EQUAL
                                   | (Unknown, _) => LESS
                                   | (_, Unknown) => GREATER
                                   (* larger constants are more useless 
                                      because they are less common *)
                                   | (Int x, Int y) => Word32.compare (y, x))))
                             rs

    in
      case rs of
        (r, _) :: _ => tryreg wb r
      | nil => error wb "there are no more unlocked registers"
    end

  (* PERF should search existing regs *)
  and getlitwithreg wb w r =
    let in
      (* check for some special constants that we can load quickly *)
      if w = 0wxFFFFFFFF then
        emit wb (NAND (r, zz, zz))
      else if w = Conventions.SIGN_BIT then
        let val t = getaddrro wb Conventions.lab_signbit in
          emit wb (ASUB (r, zz, t));
          unlock wb t
        end
      else if w = Conventions.MANTISSA_MASK then
        let val t = getaddrro wb Conventions.lab_mantissamask in
          emit wb (ASUB (r, zz, t));
          unlock wb t
        end
      (* too big for LITERAL? *)
      else if W.andb(W.<<(0wxFFF, 0w25), w) <> 0w0
      then 
        let val t = getreg wb in
          (* XXX should probably do expansion here *)
          emit wb (LITERAL_ANY(r, w, t));
          unlock wb t
        end
      else
        let in
          emit wb (LITERAL(r, w))
        end
    end

  and getlit wb w =
      let val r = getreg wb in
        (* PERF XXX implement *)
        getlitwithreg wb w r;
        r
      end

  and getlitro (wb as { regs, ... } : workingblock) w = 
      let in
        (* First, look for another readonly register
          with the same value *)
        case A.findi (fn (_, { contents = Int w', 
                               vars = _, 
                               lock = Readonly _ }) => w = w'
                       | (_, { contents = Int w', 
                               vars = _, 
                               lock = Unlocked }) => w = w'
                       | _ => false) regs of
          SOME (r, { contents, vars, lock = Readonly (SOME i) }) => 
            let in
              A.update (regs, r, { contents = contents,
                                   vars = vars,
                                   lock = Readonly (SOME (i + 1)) });
              emit wb (COMMENT ("reuse " ^ (UMAPrint.regtos r) ^ " = " ^ (W.toString w)));
              r
            end
        | SOME (r, { contents, vars, lock = Readonly NONE }) => 
            let in 
              emit wb (COMMENT ("reuse " ^ (UMAPrint.regtos r) ^ " = " ^ (W.toString w)));
              r
            end
        | SOME (r, { contents, vars, lock = Unlocked }) => 
          (* This could be the last useful register.
            If it is, check to see if there are other
            available registers and spill one of them. *)
          let val r = if isuseful regs r then tryreg wb r else r
              val { contents, vars = _, lock = _ } = A.sub (regs, r)
              val () = case contents of 
                         Int w' => if w = w' then 
                                     let in
                                       emit wb (COMMENT ("reuse " ^ (UMAPrint.regtos r) 
                                                         ^ " = " ^ (W.toString w)))
                                     end
                                   else
                                     getlitwithreg wb w r
                       | _ => getlitwithreg wb w r
              val { contents, vars, lock = _ } = A.sub (regs, r)
          in
            A.update (regs, r, { contents = contents,
                                       vars = vars,
                                       lock = Readonly (SOME 1) });
            r
          end
        | SOME _ => error wb "impossible"

        (* Otherwise, just get a new register and
           set its contents to w and its state to 
           read-only *)
        | NONE => 
          let val r = getlit wb w
          in
            A.update (regs, r, { contents = Int w,
                                 vars = nil,
                                 lock = Readonly (SOME 1) });
            r
          end
      end

  and getaddr (wb as { regs, ... } : workingblock) v = 
      let val r = getreg wb 
      in
        (* XXX what about large addrs? *)
        emit wb (LITERAL_ADDR_SMALL (r, v));
        r
      end

  and getaddrro (wb as { regs, ... } : workingblock) v = 
      (* PERF check regs for reuse *)
      getaddr wb v

  and getvar (wb as { regs, ... } : workingblock) v = 
      let in
        (* XXX don't give away the last useful register *)
        (* First look in the registers to see if its there. *)
        case A.findi (fn (_, { contents = _, vars, lock = Unlocked }) => 
                         List.exists (veq v o #1) vars
                       | (_, { contents = _, vars, lock = Readonly _ }) => 
                         List.exists (veq v o #1) vars
                       | (_, { contents = _, vars, lock = Locked }) => false )
                     regs of
          SOME (r, { contents, vars, lock = Unlocked }) => 
          (* This could be the last useful register.
            If it is, check to see if there are other
            available registers and spill one of them. *)
          let val r = if isuseful regs r then tryreg wb r else r
              val { contents = _, vars, lock = _ } = A.sub (regs, r)
              val () = if List.exists (veq v o #1) vars then 
                         let in
                           emit wb (COMMENT ("reuse " ^ (UMAPrint.regtos r) ^ " = " ^ (V.tostring v)))
                         end
                       else
                         unspillwithreg wb v r
              val { contents, vars, lock = _ } = A.sub (regs, r)
          in
            A.update (regs, r, { contents = contents,
                                 vars = vars,
                                 lock = Readonly (SOME 1) });
            r
          end
        (* Not useful in any of these cases, so no need for tryreg *)
        | SOME (r, { contents, vars, lock }) =>
                   let val lock = case lock of
                                    Unlocked => error wb "unlocked vars handled separately"
                                  | Readonly (SOME n) => Readonly (SOME (n + 1))
                                  | Readonly NONE => Readonly NONE
                                  | Locked => error wb ("found var " ^ (V.tostring v) 
                                                        ^ " in locked reg " ^ (UMAPrint.regtos r))
                   in
                     A.update (regs, r, { contents = contents,
                                          vars = vars, 
                                          lock = lock });
                     emit wb (COMMENT ("reuse " ^ (UMAPrint.regtos r) ^ " = " ^ (V.tostring v)));
                     r
                   end
        (* If not then it's already be spilled... *)
        | NONE => unspill wb v
      end

  and emit (wb as { regs, block, ... } : workingblock) inst =
    let 
      (* Check that this register is locked
         for Readonly or Locked *)
      fun checksrc x =
        case #lock (A.sub(regs, x)) of
          Unlocked => error wb ("tried to use unlocked register in " ^ UMAPrint.intos inst
                                ^ " (" ^ (rstos (A.sub(regs, x))) ^ ")")
        | _ => ()

      fun checkdst x =
        case #lock (A.sub(regs, x)) of
          Locked => ()
        | _ => error wb ("tried to use unlocked/readonly register in " ^ UMAPrint.intos inst)

      fun checkforspills r =
          let in
            if List.exists #2 (#vars (A.sub (regs, r))) then
              error wb ("can overwrite register w/ spills " 
                        ^ (UMAPrint.regtos r) ^ " in instruction " 
                        ^ (UMAPrint.intos inst))
            else
              ()
          end

      fun junkreg r =
          let in
            checkforspills r;
            A.update(regs, r, { lock = #lock(A.sub(regs, r)),
                                vars = nil,
                                contents = Unknown })
          end

      fun intreg r w =
          let in
            checkforspills r;
            A.update(regs, r, { lock = #lock(A.sub(regs, r)),
                                vars = nil,
                                contents = Int w })
          end
        
      val s = checksrc
      val d = checkdst
      val j = junkreg
      val i = intreg

      fun check inst =
      case inst of
        CMOV (a, b, c) => (d a; s b; s c; j a)
      | LOADPROG (a, b) => (s a; s b)
      | LITERAL (r, w) => (d r; i r w)
      | HALT => ()
      | READ r => (d r; j r)
      | WRITE r => (s r)
      | ALLOC (a, b) => (d a; s b; j a)
      | FREE r => s r
      | ASUB (a, b, c) => (d a; s b; s c; j a)
      | UPD (a, b, c) => (s a; s b; s c)
      | ADD (a, b, c) => (d a; s b; s c; j a)
      | MUL (a, b, c) => (d a; s b; s c; j a)
      | DIV (a, b, c) => (d a; s b; s c; j a)
      | NAND (a, b, c) => (d a; s b; s c; j a)
      | COMMENT _ => ()
      | BREAK => ()

      | INFO (w, r) => (s r)
      | RDTSC r => (d r; j r)

      | JNZO (a, l, b, c, t) => (s a; d b; d c; Option.app d t;
                                 j b; j c; Option.app j t)

      | JZO (a, l, b, c, t) => (s a; d b; d c; Option.app d t;
                                j b; j c; Option.app j t)
        
      | JLZO (a, l, b, c, t) => (s a; d b; d c; Option.app d t;
                                 j b; j c; Option.app j t)

      | CALLWITH (a, v, b, c) => (d a; d b; d c;
                                  j a; j b; j c)

      | LITERAL_ADDRO (a, v, c) => (d a; Option.app d c;
                                    j a; Option.app j c)

      | LITERAL_ANY (a, w, t) => (d a; d t;
                                  i a w; j t)

      | SUB (a, b, c, t) => (d a; s b; s c; d t;
                             j a; j t)

      | OR (a, b, c, t) => (d a; s b; s c; d t;
                            j a; j t)
        
      | XOR (a, b, c, t) => (d a; s b; s c; d t;
                             j a; j t)

      | NOT (a, b) => (d a; s b; j a)

      | AND (a, b, c) => (d a; s b; s c; j a)

      | DEC (a, b) => (d a; d b; j b;
                       (* PERF if we know a, it's now a - 1 *)
                       j a)

      | DECTO (a, b) => (d a; s b; j a)

      | SWAPEND _ => error wb "whoa, swapend to emit (!)"

      (* could poison all registers?? *)
      | DATA _ => ()
      | DATALAB _ => ()
      | BSSDATA => ()

      | MANY l => app check l

      (* XXX uhhh... someone is jumping here? *)
      | LABEL _ => ()

      (* Special case and expand some macro ops here
        -- especially those that load literals! *)
      fun append (SUB (a, b, c, t)) =
          let val one = getlitro wb 0w1 in
            GA.append block (NAND (t, c, c));
            GA.append block (ADD (a, b, t));
            GA.append block (ADD (a, one, a));
            unlock wb one
          end
        (* PERF, if both sources of a binary primop
           are known, we could not emit the
           instruction. Also, additive and
           muliplicitave units, etc... *)

        | append (MANY l) = app append l
        | append inst = GA.append block inst

    in
      check inst;
      append inst (* ;
      emit_status wb *)
    end

  and emit_status (wb as { regs, block, ... } : workingblock) =
      GA.append block
           (COMMENT ("reg status:" 
                     ^ (A.foldli
                            (fn (r, c, s) => s ^ " " ^ (UMAPrint.regtos r) ^ "=" ^ (rstos c)) 
                            "" regs)))

  fun forcespillall (wb as { regs, ... } : workingblock) =
      let in
        A.appi (fn (r, { contents = _, vars, lock = l }) => 
                   let in
                     if List.exists #2 vars then
                       let in
                         (case l of Unlocked => lock wb r
                                  | _ => ());
                         spill wb r;
                         (case l of Unlocked => unlock wb r
                                  | _ => ())
                       end
                     else ()
                   end) regs
      end

  fun emit_DEC wb r =
      let val negone = getlitro wb 0wxFFFFFFFF
      in
        emit wb (ADD (r, r, negone));
        unlock wb negone
      end

  fun emit_JZ wb forcespill (r, l) =
      let val t1 = getreg wb
          val t2 = getreg wb
      in
        if forcespill then forcespillall wb else ();
        emit wb (JZ_SMALL (r, l, t1, t2));
        (* N.B. note that this unlocks the temporaries in BOTH branches! *)
        unlock wb t1;
        unlock wb t2
      end

  fun emit_JNZ wb forcespill (r, l) =
      let val t1 = getreg wb
          val t2 = getreg wb
      in
        if forcespill then forcespillall wb else ();
        emit wb (JNZ_SMALL (r, l, t1, t2));
        (* N.B. note that this unlocks the temporaries in BOTH branches! *)
        unlock wb t1;
        unlock wb t2
      end

  fun emit_JMP wb forcespill l =
      let val t1 = getaddrro wb l
      in
        if forcespill then forcespillall wb else ();
        emit wb (LOADPROG (zz, t1));
        unlock wb t1
      end  

  fun split (wb as { regs, block, info, lab, finalized } : workingblock) : workingblock =
    let 
      val _ = if !finalized then error wb "can't split finalized block" else ()
      val _ = case GA.sub block ((GA.length block) - 1) of
                LOADPROG _ => ()
              | JNZO _ => () | JZO _ => () | JLZO _ => ()
                                                       (* XXX *)
                                                       | COMMENT _ => ()
              | inst => error wb ("last instruction of a split block "
                                  ^ "must be a branch, not " ^ (UMAPrint.intos inst))
      val block = GA.empty ()
      val wb = { regs = A.tabulate(A.length regs,
                                   (fn r =>
                                       A.sub(regs, r))),
                 block = block,
                 info = info,
                 lab = lab,
                 finalized = ref false }
    in
      emit wb (COMMENT "splitting workingblock");
      emit_status wb;
      wb
    end

  fun finalize (wb as { regs, block, finalized, ... } : workingblock) =
      let val _ = if !finalized then (* emit wb (COMMENT "this block finalized twice!") *)
                    error wb "can't finalized block twice" else ()
          val _ = finalized := true
          (* only spill live registers if the spill code would be reachable *)
          val needsspilled = 
              GA.length block > 0 andalso
              case GA.sub block (GA.length block - 1) of
                               LOADPROG _ => false
                             | HALT => false
                             (* XXX other branching insts *)
                             | _ => true
      in 
        (* XXX 6 *)
        (* XXX no need to spill if last inst is LOADPROG or HALT *)
        A.appi (fn (6, { contents = Int 0w0, vars = _, lock = Readonly NONE }) =>
                   (* some vars may be set to zero, so we still need to spill *)
                   if needsspilled then spill wb zz else ()
                 | (6, _) => error wb ("reg " ^ (Int.toString zz) ^ " not zero")
                 | (r, { contents = _, vars = _, lock = Unlocked }) => 
                   if needsspilled then
                     let in
                       lock wb r;
                       spill wb r;
                       unlock wb r
                     end
                   else ()
                 | (r, _) => error wb ("register " ^ (UMAPrint.regtos r) 
                                       ^ " still locked at finalize")) regs;
        (* emit_status wb; *)
        let val insts = A.foldr op:: nil (GA.finalize block)
in
(*        print ("finalizing... " ^ (foldl (fn (inst, s) => s ^ "\n" ^ (UMAPrint.intos inst)) "" insts) ^ "\n");  *)
insts
end
      end

  fun bindvar (wb as { regs, ... } : workingblock) v r =
      let val { contents, vars, lock } = A.sub (regs, r) 
          val lock = case lock of Locked => Readonly (SOME 1)
                                  (* No need to increment as the
                                    caller should already hold the
                                    lock. *)
                                | Readonly _ => lock
                                | Unlocked => error wb ("can bind " ^ (V.tostring v) 
                                                        ^ "with unlocked reg")
          val vars = (v, true) :: vars
      in
        A.update (regs, r, { contents = contents, 
                             vars = vars,
                             lock = lock });
        emit wb (COMMENT ("bind " ^ (UMAPrint.regtos r) ^ " = " ^ (V.tostring v)))
      end

  fun bindvarlit (wb as { regs, ... } : workingblock) v w =
      let val r = getlitro wb w
          val { contents, vars, lock } = A.sub (regs, r)
      in
        A.update (regs, r, { contents = contents,
                             vars = (v, true)::vars,
                             lock = lock });      
        emit wb (COMMENT ("bind " ^ (UMAPrint.regtos r) ^ " = " ^ (V.tostring v) 
                          ^ "(" ^ (W.toString w) ^ ")"));
        (* Release our lock *)
        unlock wb r
      end

  fun killvar ({ regs, ... } : workingblock) v =
      let in
        A.appi (fn (r, { contents, vars, lock }) =>
                   A.update (regs, r, { contents = contents,
                                        vars = List.filter (fn (v', _) => 
                                                             not (V.eq (v, v'))) vars,
                                        lock = lock }))
               regs
      end

  fun clearunlocked (wb as { regs, ... } : workingblock) = 
      let in
        emit wb (COMMENT "start cleaning");
        emit_status wb;
        A.appi (fn (r, { contents = _, vars, lock = Unlocked }) => 
                   let in
                     if List.exists #2 vars then
                       let in
                         lock wb r;
                         spill wb r;
                         unlock wb r
                       end
                     else ();
                     A.update (regs, r, { contents = Unknown,
                                          vars = nil,
                                          lock = Unlocked })
                   end
                 | _ => ()) regs;
        emit wb (COMMENT "finish cleaning");
        emit_status wb
      end

  fun checkstate (wb as { regs, ... } : workingblock) =
      let in
        A.appi (fn (6, { contents = Int 0w0, vars = _, lock = Readonly NONE }) => ()
                 | (6, _) => error wb ("reg " ^ (Int.toString zz) ^ " not zero")
                 | (r, { contents = _, vars = _, lock = Unlocked }) => ()
                 | (r, _) => error wb ("register " ^ (UMAPrint.regtos r) 
                                       ^ " still locked at checkstate")) regs;
        wb
      end

end
