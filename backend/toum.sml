
(* Convert from allocation-converted CPS language
   to Universal Machine Assembly. *)
structure ToUM =
struct

  open CPS
  open UMA
  open Primop

  exception ToUM of string
  structure V = Variable

  fun veq v v' = V.eq (v, v')

  infixr 1 `
  fun a ` b = a b

  infixr 1 @@
  fun a @@ b = a @ b

  val itow = W.fromInt

  open Conventions

  open RegHelper

  structure VVM = CPS.VVM
   (* SplayMapFn(type ord_key = V.var * V.var
                val compare = Util.lex_order V.compare V.compare) *)

  (* convert a function to UM form.

     the 'stack' is a reserved area near the
     beginning of the 0 array. It is not a stack
     in the traditional sense, though it is used
     for local data storage within a function
     body. We ensure that this stack is at least
     big enough to hold the maximum number of
     bound variables that any function body ever
     has. We maintain the invariant that every
     stack slot has a valid traceable value in
     it at every point, or 0.

     At the beginning of a function, all live data
     in the entire program is reachable from the
     arguments, so we do a GC check at this point.

     Note that we cannot do a GC check in the middle
     of a function since some stack slots may 
     contain unboxed integers.
     
     We then simply process each instruction in
     turn, writing to and reading from these
     stack slots as variables. *)

  (* register conventions.

     gg always holds 0.
     hh is free game. it is destroyed at will between
     instructions.

     *)
  val zz = gg

  fun withreg wb r k =
    (k r;
     unlock wb r)

  fun with2regs wb r1 r2 k =
    (k (r1, r2);
     unlock wb r2;
     unlock wb r1)

  val ct = ref 0
  fun convertfn info total (lab, args, ce) =
    let
      val blockset = ref nil

        (* ENH should do this every second or two *)
(*
      val _ = ct := !ct + 1
      val _ = print ("  convert " ^ V.tostring lab ^ "(" ^ 
                     Int.toString (!ct) ^ " of " ^ Int.toString total ^ ")\n")
*)

      fun slot v = case VVM.find (info, (lab, v)) of
                        NONE => raise ToUM ("(" ^ V.tostring lab ^ 
                                            ") no info for var: " ^ V.tostring v)
                      | SOME i => i

      fun putinreg dst (Var v) =
        let val (n, _) = slot v
        in
          [COMMENT ("read stack " ^ V.tostring v),
           LITERAL(dst, itow ` Limits.STACK_START + n),
           ASUB (dst, zz, dst)]
        end
        | putinreg _ _ = raise ToUM "value was not variable in putinreg"

      fun storestack v src tmp =
        let val (n, _) = slot v
        in
          [COMMENT ("store stack " ^ V.tostring v),
           LITERAL(tmp, itow ` Limits.STACK_START + n),
           UPD (zz, tmp, src)]
        end

      fun nullstack v tmp =
        let val (n, _) = slot v
        in
          [COMMENT ("null stack " ^ V.tostring v),
           LITERAL(tmp, itow ` Limits.STACK_START + n),
           UPD(zz, tmp, zz)]
        end

      fun nullstack' wb v =
        let val (n, _) = slot v
        in
          emit wb ` COMMENT ("null stack " ^ V.tostring v);
          withreg wb (getlit wb (itow ` Limits.STACK_START + n)) (fn tmp =>
          emit wb ` UPD (zz, tmp, zz))
        end

      fun nullstacks vs tmp =
        List.concat
        (map (fn v => nullstack v tmp) vs)

      fun nullstacks' wb vs =
        app (fn v => nullstack' wb v) vs

      fun getint' wb (Var v) f =
          let val res = getvar wb v
              val (_, boxed) = slot v 
          in
            if boxed then
              let val res' = getreg wb
              in
                emit wb ` COMMENT (V.tostring v ^ " is boxed");
                withreg wb (getlitro wb ` itow GC.gc_header_size) (fn tmp =>
                emit wb ` ASUB(res', res, tmp));
                unlock wb res;
                f res';
                unlock wb res'
              end
            else
              let in
                emit wb ` COMMENT (V.tostring v ^ " is unboxed");
                f res;
                unlock wb res
              end
          end
        | getint' wb (Int i) f =
          let in
            withreg wb (getlitro wb i) f
          end
        | getint' _ _ _ = raise ToUM "expected var or int in getint"

      (* write an int from a register back into the stack *)
      fun putint' wb v va =
          let val (_, boxed) = slot v in
            if boxed then
              let in
                (* allocate 1 word untraced, into obj *)
                emit wb ` COMMENT ("boxed int");
                GC.gc_alloc_static' wb 1 GC.UNTRACED (fn obj => (
                (* Make obj read-only and bind to v: *)
                bindvar wb v obj;
                withreg wb (getlitro wb ` itow GC.gc_header_size) (fn off =>
                (* initialize obj[off] = va *)
                emit wb ` UPD (obj, off, va))))
                (* no need to update stack as we've bound v *)
              end
            else 
              let in
                emit wb ` COMMENT ("unboxed int");
                (* XXX use bindvarlit *)
                bindvar wb v va 
              end
          end

      fun getaddress' wb (Label l) f =
          let in
            withreg wb (getaddrro wb l) (fn res =>
            f res)
          end
        | getaddress' wb (Var v) f =
          let in
            withreg wb (getreg wb) (fn res => (
            with2regs wb (getvar wb v) (getlitro wb (itow GC.gc_header_size)) (fn (obj, off) =>
            (* XXX unboxed rep for labels? *)
            emit wb ` ASUB (res, obj, off));
            f res))
          end
        | getaddress' _ _ _ = 
          raise ToUM "expected constant label or var in getaddress'"

      (* produce a "basic block" of straight-line UMA
         code from a continuation expression. To
         generate a basic block from a cexp, we
         generally need to generate other basic
         blocks from other branches. *)
      fun basic _ (Fix _) = raise ToUM "cvt/invt: fix"
        | basic _ (Deferred _) = raise ToUM "cvt/invt: def"

        (* might have continuations or returns if
           it was used as a primop and
           we didn't optimize them out yet... *)
        | basic wb (Primop(PHalt, [], _, _)) = 
          let in
            emit wb ` HALT;
            finalize wb
          end
        | basic _ (Primop(PHalt, _, _, _)) = raise ToUM "bad phalt"

        | basic wb (Primop(PSet, [Var re, Var va], vs, [c])) =
        let
        in
          emit wb ` COMMENT "cexp Primop PSet";

          withreg wb (getvar wb re) (fn re =>
          withreg wb (getvar wb va) (fn va =>
          withreg wb (getlitro wb ` itow (1 + GC.gc_header_size)) (fn off =>
          emit wb ` UPD (re, off, va))));
          nullstacks' wb vs;

          basic wb c
        end
        | basic _ (Primop(PSet, _, _, _)) = raise ToUM "bad setref"

        | basic wb (Primop(PGet, [Var va], [v], [c])) =
        let
        in
          emit wb ` COMMENT "cexp Primop PGet";

          withreg wb (getreg wb) (fn res => (
          withreg wb (getvar wb va) (fn va =>
          withreg wb (getlitro wb ` itow (1 + GC.gc_header_size)) (fn off =>
          emit wb ` ASUB (res, va, off)));
          bindvar wb v res));

          basic wb c
        end
        | basic _ (Primop(PGet, _, _, _)) = raise ToUM "bad getref"

        | basic wb (Primop(PRef, [Var va], [v], [c])) =
        let
        in
          emit wb ` COMMENT "cexp Primop PRef";

          (* need to use tagged pointer, but we'll just
             leave the tag as zero *)
          GC.gc_alloc_static' wb 2 GC.TAGGED_POINTER (fn res => (
          bindvar wb v res;
          withreg wb (getvar wb va) (fn cts =>
          withreg wb (getlitro wb ` W.fromInt (1 + GC.gc_header_size)) (fn off =>
          emit wb ` UPD (res, off, cts)))));

          basic wb c
        end
        | basic _ (Primop(PRef, _, _, _)) = raise ToUM "bad newref"

        | basic wb (Primop(PNewtag, [], [v], [c])) =
        let
        in
          (* PERF could unbox tags since rep is
             always known *)
          emit wb ` COMMENT ("cexp Primop PNewtag");

          withreg wb (getreg wb) (fn tag => (
          withreg wb (getlitro wb ` itow Limits.LAST_TAG) (fn off => (
          emit wb ` ASUB (tag, zz, off);
          withreg wb (getlitro wb 0w1) (fn one =>
          emit wb ` ADD (tag, one, tag));
          emit wb ` UPD (zz, off, tag)));
          putint' wb v tag));

          basic wb c
        end

        | basic wb (Primop(PGethandler, [], [v], [c])) =
        let
        in
          emit wb ` COMMENT ("cexp Primop PGethandler");

          withreg wb (getreg wb) (fn res => (
          withreg wb (getlitro wb ` itow Limits.EXCEPTION_HANDLER) (fn off =>
          emit wb ` ASUB (res, zz, off));
          bindvar wb v res));

          basic wb c
        end
        | basic _ (Primop(PGethandler, _, _, _)) = raise ToUM "bad gethandler"

        | basic wb (Primop(PSethandler, [Var va], vs, [c])) =
        let
        in
          emit wb ` COMMENT ("cexp Primop PSethandler");

          withreg wb (getvar wb va) (fn hlr =>
          withreg wb (getlitro wb ` itow Limits.EXCEPTION_HANDLER) (fn off =>
          emit wb ` UPD (zz, off, hlr)));
          (* want to catch case where this is used internally
             and when it is invoked as a primop. in the second
             case, vs is non-empty, but we just wanna stick unit
             there. *)
          nullstacks' wb vs;

          basic wb c
        end
        | basic _ (Primop(PSethandler, _, _, _)) = raise ToUM "bad sethandler"

        (* takes int, initial value *)
        | basic wb (Primop(PArray, [vlen, Var vinit], [v], [c])) =
        let
          val looptop = V.namedvar "array_init_loop"
          val done    = V.namedvar "array_init_done"
        in
          emit wb ` COMMENT "cexp Primop PArray";

          withreg wb (getreg wb) (fn size => (
          getint' wb vlen (fn len =>
          withreg wb (getlitro wb 0w1) (fn one =>
          (* (actual) size = (requested) length + 1 *)
          emit wb ` ADD (size, one, len)));

          GC.gc_alloc' wb size GC.ARRAY_TRACED (fn res => (

          withreg wb (getlitro wb ` itow GC.gc_header_size) (fn off =>
          (* put length after header *)
          getint' wb vlen (fn len =>
          emit wb ` UPD (res, off, len)));
          (* do initialization... *)

          (* we essential do explicit register
           assignment here by forcing the helper to
           discard some register contents *)
          (* we need registers for: res size init one zz *)

          (* get the init value *)
          withreg wb (getvar wb vinit) (fn init =>
          (* lock 1 in a register *)
          withreg wb (getlitro wb 0w1) (fn one => (

          (* clear out the rest of the register state
            so that it's correct w.r.t. the loop below *)
          clearunlocked wb;

          emit wb ` LABEL looptop;
          emit_DEC wb size;
          withreg wb (getlit wb ` itow (GC.gc_header_size)) (fn t1 => (
          withreg wb (getreg wb) (fn t2 =>
          emit wb ` SUB (t1, t1, size, t2));
          emit_JZ wb false (t1, done)));

          add done
              (let val wb = split wb in
                 unlock wb size;
                 unlock wb init;
                 unlock wb one;
                 bindvar wb v res;
                 unlock wb res;
                 basic wb c
               end);

          emit wb ` UPD (res, size, init);

          withreg wb (getaddrro wb looptop) (fn addr => (
          emit wb ` LOADPROG (zz, addr))))))))));
          
          finalize wb
        end
        | basic _ (Primop(PArray, _, _, _)) = raise ToUM "bad array"

        | basic wb (Primop(PArray0, [], [v], [c])) =
          let in
            emit wb ` COMMENT "cexp Primop PArray0";
            (* PERF maybe doesn't need to be traced? *)
            (* allocate empty array: just enough for the length *)
            GC.gc_alloc_static' wb 1 GC.ARRAY_TRACED (fn res =>
            (* already initialized zero size *)
            (* LITERAL(cc, itow GC.gc_header_size) :: *)
            (* UPD(dd, cc, zz) :: *)
            bindvar wb v res);

            basic wb c
          end
        | basic _ (Primop(PArray0, _, _, _)) = raise ToUM "bad array0"
        
        (* array concat, assuming same type (typically char) *)
        | basic wb (Primop(PJointext, arrs, [v], [c])) =
          let 
            (* adds total length into aa *)
            fun addlengths nil = nil
              | addlengths (va :: t) =
              putinreg bb va @
              ASUB(ee, bb, cc) :: 
              ADD(aa, aa, ee) :: 
              addlengths t

            fun computelengths l =
              LITERAL(cc, itow GC.gc_header_size) ::
              addlengths l

            (* dd : array, cc : slot to initialize *)
            fun copyto arr =
              let
                val looptop = V.namedvar "jointext_initone"
                val done = V.namedvar "jointext_doneone"
                val wordsleft = V.namedvar "jointext_wordsleft"
                val x = V.namedvar "jointext_x"
              in
                add wordsleft [BSSDATA];
                add x [BSSDATA];
                MANY
                (putinreg aa arr @
                 [LITERAL(bb, itow GC.gc_header_size),
                 (* ee : this length *)
                 ASUB(ee, aa, bb),

                  (* BREAK, *)

                 (* save it in 'wordsleft' *)
                 LITERAL_ADDR(bb, wordsleft, hh),
                 UPD(zz, bb, ee),

                 (* PERF could initialize instead to
                    gc_header *)
                 (* initialize x = 0 *)
                 LITERAL_ADDR(ff, x, hh),
                 UPD(zz, ff, zz),

                 (* so copy from
                    aa[x + gc_header + 1] to
                    dd[cc] for 'wordsleft' words. *)

                 LABEL looptop,
                 (* if out of words, done *)
                 (* free for our use: bb, ff, hh, ee *)
                 LITERAL_ADDR(bb, wordsleft, hh),
                 ASUB(bb, zz, bb),
                 JZ(bb, done, ee, ff, hh),
                 
                 (* wordsleft not zero. decrement.. *)
                 DEC(bb, hh),
                 LITERAL_ADDR(ee, wordsleft, hh),
                 UPD(zz, ee, bb), 
                 
                 LITERAL_ADDR(ff, x, hh),
                 ASUB(bb, zz, ff),
                 (* bb = x, save aa, dd, cc *)
                 LITERAL(ee, 0w1),
                 ADD(bb, bb, ee),

                 (* bb = x + 1 *)
                 (* save it back, since we want to increment x *)
                 UPD(zz, ff, bb),

                 (* read aa[x + gc_header + 1] *)
                 LITERAL(ee, itow GC.gc_header_size),
                 ADD(ee, ee, bb),
                 (* so ee : x + gc_header + 1; read it *)
                 ASUB(ee, aa, ee),
                 (* ee = char *)

                 (* write to dd[cc] *)

                 (* LITERAL(ee, itow (ord #"@")), *)
                 UPD(dd, cc, ee),

                 LITERAL(ee, 0w1),
                 ADD(cc, cc, ee),
                 
                 (* loop *)
                 LITERAL_ADDR(ee, looptop, hh),
                 LOADPROG(zz, ee),
                 
                 LABEL done])
              end

          in
            finalize wb @@
            (* compute length for new string.
               start with 1 (size at header) *)
            COMMENT("cexp Primop PJointext") ::
            LITERAL(aa, 0w1) ::
            MANY (computelengths arrs) ::

            (* allocate aa words into dd *)
            GC.gc_alloc ff hh dd aa GC.ARRAY_TRACED @
            
            (* cc still points at size word for arrays *)
            (* aa was #chars+1, now decrement *)
            DEC(aa, hh) ::
            UPD(dd, cc, aa) ::

            (* init... *)
            LITERAL(cc, 0w1 + itow GC.gc_header_size) ::

            (* dd : array,
               aa : total length,
               cc : first slot to initialize *)
            map copyto arrs @

            (* save dd into the stack slot *)
            storestack v dd hh @
            basic (empty info lab) c

          end

        | basic _ (Primop(PJointext, _, _, _)) = raise ToUM "bad jointext"

        (* unsafe sub *)
        (* PERF special case const offset *)
        | basic wb (Primop(PSub, [Var varr, voff], [v], [c])) =
          let in
            emit wb ` COMMENT "cexp Primop PSub";

            withreg wb (getvar wb varr) (fn arr =>
            withreg wb (getreg wb) (fn res => (
            getint' wb voff (fn off =>
            (* first element *)
            withreg wb (getlitro wb ` 0w1 + itow GC.gc_header_size) (fn first =>
            (* offset of elem *)
            emit wb ` ADD (res, first, off)));
            emit wb ` ASUB (res, arr, res);
            bindvar wb v res)));
            
            basic wb c
          end
        | basic _ (Primop(PSub, _, _, _)) = raise ToUM "bad array sub"

        (* unsafe update with constant offset *)
        | basic wb (Primop(PUpdate, [Var varr, Int off, Var velem], vs, [c])) =
          let in
            emit wb ` COMMENT ("cexp Primop PUpdate (const)");

            (* offset of elem *)
            withreg wb (getlitro wb ` itow (1 + GC.gc_header_size) + off) (fn off =>
            withreg wb (getvar wb varr) (fn arr =>
            withreg wb (getvar wb velem) (fn elem =>
            emit wb ` UPD(arr, off, elem))));
            
            nullstacks' wb vs;
            basic wb c
          end
        | basic wb (Primop(PUpdate, [Var varr, voff, Var velem], vs, [c])) =
          let in
            emit wb ` COMMENT ("cexp Primop PUpdate (var)");

            withreg wb (getreg wb) (fn which => (
            getint' wb voff (fn off =>
            (* first element *)
            withreg wb (getlitro wb ` itow (1 + GC.gc_header_size)) (fn first =>
            (* offset of elem *)
            emit wb ` ADD (which, off, first)));
            withreg wb (getvar wb varr) (fn arr =>
            withreg wb (getvar wb velem) (fn elem =>
            emit wb ` UPD(arr, which, elem)))));
            
            nullstacks' wb vs;
            basic wb c
          end
        | basic _ (Primop(PUpdate, _, _, _)) = raise ToUM "bad array upd"

        | basic wb (Primop(PArraylength, [Var varr], [v], [c])) =
          let in 
            emit wb ` COMMENT ("cexp Primop PArraylength");

            withreg wb (getreg wb) (fn res => (
            withreg wb (getvar wb varr) (fn arr =>
            withreg wb (getlitro wb ` itow GC.gc_header_size) (fn off =>
            emit wb ` ASUB (res, arr, off)));
            putint' wb v res));

            basic wb c
          end
        | basic _ (Primop(PArraylength, _, _, _)) = raise ToUM "bad array len"

        | basic wb (App(f, vas)) =
          let fun movearg (Var va, i) =
                  let in
                    emit wb ` COMMENT ("store arg #" ^ Int.toString i);
                    withreg wb (getvar wb va) (fn arg =>
                    (* SUSP makes assumptions about arg layout..? valid? *)
                    withreg wb (getlitro wb (itow ` Limits.STACK_START + i)) (fn tmp =>
                    (* PERF could avoid this if there is a variable in arg
                    with slot tmp that has already be spilled *)
                    emit wb ` UPD (zz, tmp, arg)))
                  end
                | movearg (_, _) = raise ToUM "bad args in app"
          in
            (* move all arguments into position *)
            emit wb ` COMMENT ("cexp App");

            ListUtil.appi movearg vas;
            getaddress' wb f (fn addr =>
            emit wb ` LOADPROG (zz, addr));

            finalize wb
          end

        | basic wb (Primop(PDynamic, [va], [v], [c])) =
        let
        in
          emit wb ` COMMENT ("cexp Primop PDynamic");

          withreg wb (getaddr wb Runtime.lab_dynamic) (fn res => (
          getint' wb va (fn arg =>
          emit wb ` ADD (res, arg, res));
          emit wb ` ASUB (res, zz, res);
          putint' wb v res));

          basic wb c
        end
        | basic _ (Primop(PDynamic, _, _, _)) = raise ToUM "bad dynamic"

        | basic wb (Sumswitch(Var obj, num, v, ics, def)) =
        (* decide if we are going to generate a jump table. *)
        let
          val actual = length ics + 1 (* default *)
          val density = actual * 100 div num

          (* NOTE don't turn this on; it modifies the behavior of
             the output program! *)
          val debugswitch = false

          val () =
            if debugswitch
            then 
              print("sumswitch: " ^ Int.toString num ^ " max, " ^
                    Int.toString (actual) ^ " actual; " ^
                    Int.toString (density) ^ "% density\n")
            else ();

          fun ds wb s = if debugswitch then
                          let in
                            withreg wb (getreg wb) (fn tmp =>
                            emit wb ` MANY (emitstring [tmp] s))
                          end
                        else ()

          (* sequential tests are only good when the density is
             VERY low *)
          val jumptable = density > 20

          fun computelab (i, c) = (i, (V.namedvar ("sumswitch_is_" ^ Int.toString i), c))

          val ilcs = map computelab ics

          (* either way, emit each branch *)
          fun emitbranch wb r (i, (l, c)) =
              add l (let in
                       ds wb (".." ^ Int.toString i ^ "\n");
                       (* unlock this register here
                        since it is used in computing
                        each branch *)
                       unlock wb r;
                       basic wb c
                     end)

          val comment = 
            "cexp Sumswitch (" ^ (if jumptable then "jumptable"
                            else "sequential") ^ ")"

          (* the default label is only used in the
           jump table case, but we emit it in both
           cases to keep things simple *)
          val deflab = V.namedvar "sumswitch_default"

        in
          emit wb ` COMMENT (comment);
          ds wb (comment ^ "\n");
          if debugswitch then emit wb BREAK else ();

          withreg wb (getreg wb) (fn tag => (
          (* get the scrutinee *)
          withreg wb (getvar wb obj) (fn sc => (
          (* get its tag *)
          withreg wb (getlitro wb ` itow GC.gc_header_size) (fn off =>
          emit wb ` ASUB (tag, sc, off)); (* unlock off *)
          withreg wb (getlitro wb ` itow (1 + GC.gc_header_size)) (fn off =>
          (* put contents in var *)
          withreg wb (getreg wb) (fn cts => (
          emit wb ` ASUB(cts, sc, off);
          bindvar wb v cts))))); (* unlock off, cts, and sc *)

          (* emit the jump calculation or dispatch *)
          (if jumptable then 
             let
               (* PERF quadratic *)
               val dests =
                 List.tabulate(num,
                               fn i =>
                               case ListUtil.Alist.find op= ilcs i of
                                 SOME (l, _) => l
                               | NONE => deflab)

               val tlab = V.namedvar "sumswitch_table"
             in
               (* add a block for the table itself *)
               add tlab (map DATALAB dests);
               
               withreg wb (getaddr wb tlab) (fn addr => (
               (* get table *)
               emit wb ` ADD (addr, addr, tag);
               (* get dest *)
               emit wb ` ASUB (addr, zz, addr);
               (* jump *)
               emit wb ` LOADPROG (zz, addr)));

               (* emit all branches here, since all will
                 start with the same register file state *)
               app (fn ilc => emitbranch (split wb) tag ilc) ilcs
             end
           else
             let 
               fun dispatch nil = ()
                 | dispatch ((i, (l, c)) :: rest) =
                   let in
                     withreg wb (getlit wb ` itow (i + 1)) (fn addr => (
                     (* add the tag. if zero, it's equal *)
                     emit wb ` ADD (addr, tag, addr);
                     emit_JZ wb false (addr, l)));
                     (* emit this branch here, since
                      the state of wb is correct if the
                      jump is taken *)
                     emitbranch (split wb) tag (i, (l, c));
                     dispatch rest
                   end
             in
               (* cheaper to pre-negate, since we have
                  no subtraction instruction *)
               emit wb ` NAND (tag, tag, tag);
               dispatch ilcs
             end)));

          (* now the default case.  note that it is ok
           to emit this label since we will only jump
           to it from the previous instruction (and
           therefore the register file is in a known
           state). *)
          emit wb ` LABEL deflab;
          ds wb "..def\n";

          basic wb def
        end

        | basic wb (Primop(PBind, [Var va], [v], [c])) =
        let
        in
           withreg wb (getvar wb va) (fn r =>
           bindvar wb v r);

           basic wb c
        end
        | basic _ (Primop(PBind, _, _, _)) = raise ToUM "bad pbind"

        | basic wb (Primop(PKill, vas, nil, [c])) =
        let fun kill (Var v) = killvar wb v
              | kill _ = ()
        in
          emit wb ` COMMENT ("cexp Primop PKill (" ^
                             (StringUtil.delimit ", " `
                               List.mapPartial (fn (Var v) => SOME ` V.tostring v
                                            | _ => NONE) vas) ^ ")");
          map kill vas;
          basic wb c
        end
        | basic _ (Primop(PKill, _, _, _)) = raise ToUM "bad pkill"

        | basic wb (Primop(PGetc, [], [v], [c])) =
        let
        in
          emit wb ` COMMENT ("cexp Primop PGetc (" ^ (V.tostring v) ^ " <- input)");

          withreg wb (getreg wb) (fn res => (
          emit wb ` READ res;
          putint' wb v res));

          basic wb c
        end
        | basic _ (Primop(PGetc, _, _, _)) = raise ToUM "bad getc"

        | basic wb (Primop(PPutc, [va], vs, [c])) =
        let
        in
          emit wb ` COMMENT ("cexp Primop PPutc");

          getint' wb va (fn chr =>
          emit wb ` WRITE chr);
          (* putc returns unit *)
          nullstacks' wb vs;

          basic wb c
        end
        | basic _ (Primop(PPutc, _, _, _)) = raise ToUM "bad putc"

        | basic wb (Primop(PCompileWarn s, [], vs, [c])) =
        let in
          print ("Warning: " ^ s ^ "\n");

          nullstacks' wb vs;
          basic wb c
        end
        | basic _ (Primop(PCompileWarn _, _, _, _)) =
        raise ToUM "bad compilewarn??"

        | basic wb (Primop(PNull, [Var va], [], [cz, cnz])) =
        let
          val zlab = V.namedvar "null_z"
          val nzlab = V.namedvar "null_nz"
        in
          (* really cheap using cmov *)
          emit wb ` COMMENT "cexp Primop PNull";
          withreg wb (getvar wb va) (fn arg =>
          withreg wb (getaddrro wb zlab) (fn zl =>
          withreg wb (getaddrro wb nzlab) (fn nzl => (
          emit wb ` CMOV (zl, nzl, arg);
          emit wb ` LOADPROG (zz, zl)))));

          let val wb = split wb in
            add zlab (basic wb cz)
          end;
          let val wb = split wb in
            add nzlab (basic wb cnz)
          end;

          finalize wb
        end
        | basic _ (Primop(PNull, _, _, _)) = raise ToUM "bad null"

        | basic wb (e as Primop(B (PCmp c), [vl, vr], [], [ct, cf])) =
        let
          fun do_cmp wb c ls ll rs rr cyes cno =
            let 
              val noless = V.namedvar "no_less"
              val yesless = V.namedvar "yes_less"

              fun emitbranches () =
                  let in
                    add noless (basic (empty info lab) cno);
                    add yesless (basic (empty info lab) cyes)
                  end

              (* unsigned comparison: is ll < rr?
                assumes sign bits are not set.
                assumes ll and rr are locked/readonly
                regs for the ll and rr arguments and
                that no other registers are
                locked. will unlock ll and rr.  *)
              fun unsigned_compare wb ll rr allow_eq noless yesless =
                  let in
                    withreg wb (getreg wb) (fn ltmp => (
                    withreg wb (getreg wb) (fn rtmp => (
                    withreg wb (getlitro wb Conventions.MANTISSA_MASK) (fn mtsa_mask => (
                    emit wb ` AND(ltmp, ll, mtsa_mask);
                    emit wb ` AND(rtmp, rr, mtsa_mask)));

                    (* no need for the original args anymore *)
                    unlock wb ll;
                    unlock wb rr;
                        
                    (* we now have enough bits to
                      correctly check the sign of the
                      result.

                      if rr - ll is positive,
                      then true *)
                    withreg wb (getreg wb) (fn tmp =>
                    emit wb ` SUB (ltmp, rtmp, ltmp, tmp)))); (* unlock rtmp *)
                    
                    (* test for equality here, if
                       we're doing < but ignore equal
                       case for <= *)
                    (if allow_eq
                     then ()
                     else
                       (* zero? then it isn't less. *)
                         emit_JZ wb true (ltmp, noless));
                    
                    withreg wb (getlitro wb Conventions.SIGN_BIT) (fn sign_bit =>
                    emit wb ` AND (ltmp, ltmp, sign_bit));

                    (* PERF use CMOV *)
                    (* negative? jump *)
                    emit_JNZ wb true (ltmp, noless))); (* unlock ltmp *)

                    emit_JMP wb true yesless;
                    finalize wb
                  end

              fun lesscode (SOME LESS) rs allow_eq = 
                 let
                   val () = emitbranches ()
                   (* SUSP these must be unlocked manually *)
                   val sign_bit = getlitro wb Conventions.SIGN_BIT
                   val res = getreg wb
                 in
                   emit wb ` COMMENT "ll is always negative";
                   emit wb ` AND (res, rr, sign_bit);
                   (* if sign bit clear in r,
                      then it is positive,
                    so l < r true *)
                   emit_JZ wb true (res, yesless);

                   unlock wb sign_bit;
                   unlock wb res;

                   emit wb ` COMMENT "both negative";
                   (* since they're both negative, the
                      direction of comparison is reversed
                      here. (0xFFFF meaning -1 is
                      *smaller* than 0x0001 meaning -big) *)

                   unsigned_compare wb rr ll (not allow_eq) yesless noless
                 end
              | lesscode (SOME EQUAL) rs false =
                let 
                  val ltl = V.namedvar "le_z" in
                  unlock wb ll;

                  emit wb ` COMMENT "ll is always zero";
                  withreg wb (getlit wb 0wxFFFFFFFF) (fn res => (
                  emit wb ` CMOV (res, rr, rr);

                  unlock wb rr;

                  withreg wb (getlitro wb Conventions.SIGN_BIT) (fn sign_bit =>
                  emit wb ` AND (res, res, sign_bit));

                  emit_JNZ wb false (res, ltl)));

                  add ltl (let val wb = split wb in
                             basic wb cno
                           end);
                  basic wb cyes
                end
              | lesscode (SOME EQUAL) rs true =
                let 
                  val ltl = V.namedvar "le_z" in
                  unlock wb ll;

                  emit wb ` COMMENT "ll is always zero";
                  withreg wb (getreg wb) (fn res => (
                  withreg wb (getlitro wb Conventions.SIGN_BIT) (fn sign_bit =>
                  emit wb ` AND (res, rr, sign_bit));

                  unlock wb rr;

                  emit_JNZ wb false (res, ltl)));

                  add ltl (let val wb = split wb in
                             basic wb cno
                           end);
                  basic wb cyes
                end
              | lesscode (SOME GREATER) rs allow_eq = 
                 let
                   val () = emitbranches ()
                   (* SUSP these must be unlocked manually *)
                   val sign_bit = getlitro wb Conventions.SIGN_BIT
                   val res = getreg wb
                 in
                   emit wb ` COMMENT "ll is always non-negative";
                   emit wb ` AND (res, rr, sign_bit);
                     (* if sign bit is set in r,
                        then it is negative, but l
                        is positive here, so false *)
                   emit_JNZ wb true (res, noless);

                   unlock wb res;
                   unlock wb sign_bit;

                   emit wb ` COMMENT "both positive";
                   unsigned_compare wb ll rr allow_eq noless yesless
                 end
              | lesscode ls (SOME LESS) allow_eq = 
                 let
                   val () = emitbranches ()
                   val lneg = V.namedvar "ll_neg"
                   (* SUSP these must be unlocked manually *)
                   val sign_bit = getlitro wb Conventions.SIGN_BIT
                   val res = getreg wb
                 in
                   emit wb ` COMMENT "rr is always negative";
                   (* check sign bits. *)
                   emit wb ` AND (res, ll, sign_bit);
                   emit_JNZ wb false (res, lneg);

                   add lneg
                       (let val wb = split wb in
                          emit wb ` COMMENT "ll is negative";

                          unlock wb sign_bit;
                          unlock wb res;

                          (* since they're both
                           negative, the direction of
                           comparison is reversed
                           here. (0xFFFF meaning -1 is
                           *smaller* than 0x0001
                           meaning -big) *)
                          unsigned_compare wb rr ll (not allow_eq) yesless noless
                        end);

                   unlock wb res;
                   unlock wb sign_bit;

                   unlock wb ll;
                   unlock wb rr;
                     
                   emit_JMP wb true noless;
                   finalize wb
                 end
              | lesscode ls (SOME EQUAL) false =
                let 
                  val ltl = V.namedvar "lt_z" in
                  unlock wb rr;

                  emit wb ` COMMENT "rr is always zero";
                  withreg wb (getreg wb) (fn res => (
                  withreg wb (getlitro wb Conventions.SIGN_BIT) (fn sign_bit =>
                  emit wb ` AND (res, ll, sign_bit));

                  unlock wb ll;

                  emit_JNZ wb false (res, ltl)));

                  add ltl (let val wb = split wb in
                             basic wb cyes
                           end);
                  basic wb cno
                end
              | lesscode ls (SOME EQUAL) true =
                let 
                  val ltl = V.namedvar "le_z" in
                  unlock wb rr;

                  emit wb ` COMMENT "rr is always zero";
                  withreg wb (getlit wb 0wxFFFFFFFF) (fn res => (
                  emit wb ` CMOV (res, ll, ll);

                  unlock wb ll;

                  withreg wb (getlitro wb Conventions.SIGN_BIT) (fn sign_bit =>
                  emit wb ` AND (res, res, sign_bit));

                  emit_JNZ wb false (res, ltl)));

                  add ltl (let val wb = split wb in
                             basic wb cyes
                           end);
                  basic wb cno
                end
              | lesscode ls (SOME GREATER) allow_eq = 
                 let
                   val () = emitbranches ()
                   val lneg = V.namedvar "ll_neg"
                   (* SUSP these must be unlocked manually *)
                   val sign_bit = getlitro wb Conventions.SIGN_BIT
                   val res = getreg wb
                 in
                   emit wb ` COMMENT "rr is always non-negative";
                   (* check sign bits. *)
                   emit wb ` AND (res, ll, sign_bit);
                   emit_JNZ wb false (res, lneg);

                   add lneg
                       (let val wb = split wb in
                          emit wb ` COMMENT "ll is negative";

                          unlock wb res;
                          unlock wb sign_bit;

                          unlock wb ll;
                          unlock wb rr;

                          emit_JMP wb true yesless;
                          finalize wb
                        end);

                   unlock wb res;
                   unlock wb sign_bit;

                   unsigned_compare wb ll rr allow_eq noless yesless
                 end
              (* Assume we know nothing about the arguments *)
              | lesscode _ _ allow_eq = 
                 let
                   val () = emitbranches ()
                   val lneg = V.namedvar "ll_neg"
                   (* SUSP these must be unlocked manually *)
                   val sign_bit = getlitro wb Conventions.SIGN_BIT
                   val res = getreg wb
                 in
                   (* check sign bits. *)
                   emit wb ` AND (res, ll, sign_bit);
                   emit_JNZ wb false (res, lneg);

                   add lneg
                       (let val wb = split wb in
                          emit wb ` COMMENT "ll is negative";
                          (* sign_bit is still SIGN_BIT *)
                          emit wb ` AND (res, rr, sign_bit);
                          (* if sign bit clear in r,
                            then it is positive,
                            so l < r true *)
                          emit_JZ wb true (res, yesless);

                          unlock wb sign_bit;
                          unlock wb res;

                          emit wb ` COMMENT "both negative";
                          (* since they're both
                           negative, the direction of
                           comparison is reversed
                           here. (0xFFFF meaning -1 is
                           *smaller* than 0x0001
                           meaning -big) *)
                          unsigned_compare wb rr ll (not allow_eq) yesless noless
                        end);

                    (* check sign bit of other argument *)
                    emit wb ` AND (res, rr, sign_bit);
                    (* if sign bit is set in r,
                       then it is negative, but l
                       is positive here, so false *)
                    emit_JNZ wb true (res, noless);

                    unlock wb res;
                    unlock wb sign_bit;

                    emit wb ` COMMENT "both positive";
                    unsigned_compare wb ll rr allow_eq noless yesless
                 end

            in
              case c of
                PEq => 
                let
                  val eql = V.namedvar "not_eq"
                in
                  withreg wb (getreg wb) (fn t1 => (
                  withreg wb (getreg wb) (fn t2 =>
                  emit wb ` SUB (t1, ll, rr, t2));
                  emit_JNZ wb false (t1, eql))); (* unlock t1, t2 *)

                  unlock wb ll;
                  unlock wb rr;

                  add eql (let val wb = split wb in
                             basic wb cno
                          end);
                  basic wb cyes
                end

              (* signed lt. *)
              | PLess => lesscode ls rs false
              | PLesseq => lesscode ls rs true

              (* array bounds check -- note that the labels are switched! *)
              | PBChk => (emitbranches (); unsigned_compare wb ll rr false yesless noless)

              (* for these, just swap the meaning of the continuations *)
              | PGreater => do_cmp wb PLesseq ls ll rs rr cno cyes
              | PGreatereq => do_cmp wb PLess ls ll rs rr cno cyes
              | PNeq => do_cmp wb PEq ls ll rs rr cno cyes
            end

          val () = emit wb ` COMMENT ("cexp Primop " ^ Primop.tostring (B (PCmp c)))

          (* SUSP similar to getint', but different in that
               1. no automatic unlocking
               2. return an "order option" comparing the value to zero 
           *)
          fun getintandcompare wb (Var v) =
              let val res = getvar wb v
                  val (_, boxed) = slot v 
              in
                if boxed then
                  let val res' = getreg wb
                  in
                    emit wb ` COMMENT (V.tostring v ^ " is boxed");
                    withreg wb (getlitro wb ` itow GC.gc_header_size) (fn tmp =>
                    emit wb ` ASUB(res', res, tmp));
                    unlock wb res;
                    (NONE, res')
                  end
                else
                  let in
                    emit wb ` COMMENT (V.tostring v ^ " is unboxed");
                    (NONE, res)
                  end
              end
            | getintandcompare wb (Int i) =
              let val or = if i = 0w0 then EQUAL
                           else if W.> (W.andb (i, 0wx80000000), 0w0) then
                             LESS
                           else GREATER
              in
                (SOME or, getlitro wb i)
              end
            | getintandcompare _ _ = 
              raise ToUM "expected var or int in getintandcompare"
                                   
          (* SUSP these regs must be unlocked manually! *)
          val (ls, ll) = getintandcompare wb vl
          val (rs, rr) = getintandcompare wb vr
        in
          do_cmp wb c ls ll rs rr ct cf
        end
        | basic _ (Primop(B (PCmp c), _, _, _)) = raise ToUM "bad po cmp"

        | basic wb (Primop(PNotb, [va], [v], [c])) =
          let in
            emit wb ` COMMENT ("cexp PNotb");

            withreg wb (getreg wb) (fn res => (
            getint' wb va (fn arg =>
            emit wb ` NAND (res, arg, arg));
            putint' wb v res));

            basic wb c
         end
        | basic _ (Primop(PNotb, _, _, _)) = raise ToUM "bad po not"

        | basic wb (Primop(B bop, [vl, vr], [v], [c])) =
          let 
            fun do_op dst srcl srcr p =
              (case p of
                 PPlus => emit wb ` ADD (dst, srcl, srcr)
               | PTimes => emit wb ` MUL (dst, srcl, srcr)
               | PDiv => emit wb ` DIV (dst, srcl, srcr)
               | PMinus => withreg wb (getreg wb) (fn t1 =>
                           emit wb ` SUB (dst, srcl, srcr, t1))
               | PAndb => emit wb ` AND (dst, srcl, srcr)
               | PXorb => withreg wb (getreg wb) (fn t1 =>
                          emit wb ` XOR (dst, srcl, srcr, t1))
               | POrb => withreg wb (getreg wb) (fn t1 =>
                         emit wb ` OR (dst, srcl, srcr, t1))
               | PShl => with2regs wb (getaddr wb Runtime.lab_pow2_table)
                                   (getlit wb 0w31) 
                                   (fn (t1, t2) => (
                         emit wb ` AND (t2, t2, srcr);
                         emit wb ` ADD (t1, t2, t1);
                         emit wb ` ASUB (t1, zz, t1);
                         emit wb ` MUL (dst, srcl, t1)))
               | PShr => with2regs wb (getaddr wb Runtime.lab_pow2_table)
                                   (getlit wb 0w31) 
                                   (fn (t1, t2) => (
                         emit wb ` AND (t2, t2, srcr);
                         emit wb ` ADD (t1, t2, t1);
                         emit wb ` ASUB (t1, zz, t1);
                         emit wb ` DIV (dst, srcl, t1)))
               | PMod => raise ToUM "mod not implemented"
               | PCmp _ => raise ToUM "impossible" )

          in
            emit wb ` COMMENT ("cexp Primop " ^ Primop.tostring (B bop));

            getint' wb vl (fn ll =>
            getint' wb vr (fn rr =>
            withreg wb (getreg wb) (fn res => (
            do_op res ll rr bop;
            emit wb ` COMMENT ("primop int result");
            putint' wb v res))));

            basic (checkstate wb) c
          end
        | basic _ (Primop(B bop, _, _, _)) =
        raise ToUM "bad po bop"

        | basic wb (Project(n, Var va, v, c)) =
        let
        in
          emit wb ` COMMENT ("cexp project #" ^ Int.toString n);

          withreg wb (getreg wb) (fn res => (
          withreg wb (getvar wb va) (fn tup =>
          withreg wb (getlitro wb ` itow (GC.gc_header_size + 1 + n)) (fn off =>
          emit wb ` ASUB(res, tup, off)));
          bindvar wb v res));

          basic (checkstate wb) c
        end

        | basic wb (Alloc(STRING s, [], v, c)) =
        let
          (* for better obfuscation but worse
             compression, could make this an 
             arbitrary 8-bit value *)
          val BYTE_START = 0w20

          val prevchar = ref (BYTE_START : W.word)

          val n = size s

          val words =
            List.tabulate
            (n div 4 +
             (if n mod 4 = 0 then 0
              else 1),
                 fn x =>
                 let
                   fun gb z =
                     let val u = (x * 4) + z
                     in
                       (* print ("   u = " ^ Int.toString u ^ "\n"); *)
                       if u >= n
                       (* doesn't need difference code; not read *)
                       then 0w0
                       else 
                         let 
                           val c = Word32.fromInt (ord (CharVector.sub(s, u)))
                           val code = Word32.andb(0w255, c - !prevchar)
                         in
                           prevchar := c;
                           code
                         end
                     end

                   val a = gb 0
                   val b = gb 1
                   val c = gb 2
                   val d = gb 3

                   val w = 
                     (W.orb
                      (W.<<(d, 0w24),
                       W.orb
                       (W.<<(c, 0w16),
                        W.orb
                        (W.<<(b, 0w8), a))))
                 in
                   (* print (Int.toString x ^ " => " ^ W.toString w ^ " \n"); *)
                   DATA w
                 end)

          val dlab = V.namedvar "stringdata"
          val looptop = V.namedvar "string_init_loop"
          val done    = V.namedvar "string_init_done"

          val prevbyte = V.namedvar "string_init_prevbyte"
        in
          add prevbyte [BSSDATA];
          add looptop
          [(* aa : allocated array,
              cc : byte #
              *)
           LITERAL_ANY(bb, W.fromInt (size s), hh),
           SUB(bb, cc, bb, hh),
           JZ(bb, done, ee, ff, hh),

           (* XXX spoons should this be (possibly) unboxed? *)
           MANY (GC.gc_alloc_static hh ff ee 1 GC.UNTRACED),

           (* BREAK, *)

           (* (aa,cc) ee : allocated char *)

           (* offset of input word we want *)
           LITERAL(hh, 0w4),
           DIV(ff, cc, hh),
           
           (* string data *)
           LITERAL_ADDR(bb, dlab, hh),
           ADD(bb, bb, ff),
           ASUB(bb, zz, bb),
           
           (* bb : contains word
              ee : allocated char
              (aa, cc) *)
           
           LITERAL(hh, 0w3),
           AND(ff, cc, hh),
           LITERAL_ADDR(dd, Runtime.lab_shifttable, hh),
           (* dd[cc mod 4] gives divisor *)
           ADD(dd, dd, ff),
           ASUB(dd, zz, dd),

           (* divide and mask the byte we want *)
           DIV(bb, bb, dd),
           
           (* difference code for obfuscation *)
           LITERAL_ADDR(ff, prevbyte, hh),
           ASUB(hh, zz, ff),
           (* we ADD the previous byte to get the new char *)
           ADD(bb, hh, bb),

           (* now mask it *)
           LITERAL(hh, 0w255),
           AND(bb, bb, hh),

           (* save it as prevbyte *)
           UPD(zz, ff, bb),

           (* debug *)
(*            WRITE bb, *)

           (* put it in boxed char *)
           LITERAL(ff, W.fromInt GC.gc_header_size),
           UPD(ee, ff, bb),
           
           (* stick boxed thing in string (at cc + 1 + gc_header_size) *)
           LITERAL(hh, 0w1),
           ADD(cc, cc, hh),
           LITERAL(dd, itow GC.gc_header_size),
           ADD(dd, cc, dd),
           UPD(aa, dd, ee),

           LITERAL_ADDR(ee, looptop, hh),
           LOADPROG(zz, ee)];

          (* when done, stick initialized string into stack *)
          add done
           ((* LITERAL(hh, itow (ord #"@")) ::
            WRITE hh :: *)
            storestack v aa hh @
            basic (empty info lab) c);
           
          (* PERF should form string pool -- could happen in ADD. 
             (note:it must know which things can be written to) *)
          add dlab words;

          finalize wb @@
          [COMMENT ("cexp Alloc STRING " ^ StringUtil.harden (Char.isAlphaNum) #"?" 32 s),

          (* allocate enough for string, into aa *)
          MANY
          (GC.gc_alloc_static bb cc aa (size s + 1) GC.ARRAY_TRACED),

          (* put size at front *)
          LITERAL_ANY(cc, W.fromInt (size s), hh),
          LITERAL(dd, itow GC.gc_header_size),
          UPD(aa, dd, cc),

          LITERAL(cc, BYTE_START),
          LITERAL_ADDR(ff, prevbyte, hh),
          UPD(zz, ff, cc),

          (* cc : character num for initialization *)
          LITERAL(cc, 0w0),
   
          LITERAL_ADDR(ee, looptop, hh),
          LOADPROG(zz, ee)]
        end
        | basic _ (Alloc(STRING _, _, _, _)) = raise ToUM "bad string alloc"

        (* allocate constant int *)
        | basic wb (Alloc (INT, [Int i], v, c)) =
        let in
          (* PERF move this down close to where it's used *)
          emit wb ` COMMENT ("cexp Alloc INT " ^ Word32.toString i);

          withreg wb (getlitro wb i) (fn lit =>
          putint' wb v lit);

          basic wb c
        end
        | basic _ (Alloc (INT, _, _, _)) = raise ToUM "bad int alloc"

        | basic wb (Alloc (TUPLE 0, [], v, c)) =
        (* special case for empty tuples -- store 0 *)
        let in
          emit wb ` COMMENT ("cexp Alloc NULL " ^ V.tostring v);

          withreg wb (getlitro wb 0w0) (fn res =>
          bindvar wb v res);

          basic wb c
        end

        | basic wb (Alloc (TUPLE n, vas, v, c)) =
        let 
          fun initialize res l =
              ListUtil.appi (fn (Var v, i) =>
                                let in
                                  withreg wb (getvar wb v) (fn cts =>
                                  withreg wb (getlitro wb ` itow (GC.gc_header_size + 1 + i)) (fn off =>
                                  emit wb ` UPD (res, off, cts)))
                                end
                              | _ => raise ToUM "bad tuple init val") l
        in
          if n <> length vas 
          then raise ToUM "bad tuple: incorrect number of values"
          else ();
          emit wb ` COMMENT ("cexp Alloc TUPLE " ^ Int.toString n);

          GC.gc_alloc_static' wb (n + 1) GC.ARRAY_TRACED (fn res => (
          withreg wb (getlitro wb ` itow n) (fn size =>
          withreg wb (getlitro wb ` itow GC.gc_header_size) (fn off =>
          emit wb ` UPD(res, off, size)));
          initialize res vas;
          bindvar wb v res));

          basic wb c
        end

        | basic wb (Alloc (CODE, [Label l], v, c)) =
          let in
            emit wb ` COMMENT ("cexp Alloc CODE " ^ V.tostring l);

            (* XXX should this be (possibly) unboxed? *)
            GC.gc_alloc_static' wb 1 GC.UNTRACED (fn res => (
            withreg wb (getaddrro wb l) (fn addr => 
            withreg wb (getlitro wb ` itow GC.gc_header_size) (fn off =>
            emit wb ` UPD (res, off, addr)));
            bindvar wb v res));

            basic wb c
          end
        | basic _ (Alloc (CODE, _, v, c)) = raise ToUM "bad code alloc"

        | basic wb (Alloc (INT_T t, vas, v, c)) =
        let in
          if length vas > 1 then raise ToUM "can't put more than one thing in an int_t"
          else ();

          emit wb ` COMMENT ("cexp Alloc TAGGED " ^ Int.toString t ^ " <- (something)");
          
          (* two words: one tag, one pointer *)
          GC.gc_alloc_static' wb 2 GC.TAGGED_POINTER (fn res => (
          (* every datatype has a tag 0, and we can avoid
             writing that, because the allocation already
             contains zero *)
          (if t <> 0
           then (
             withreg wb (getlitro wb ` itow t) (fn tag =>
             withreg wb (getlitro wb ` itow GC.gc_header_size) (fn off =>
             emit wb ` UPD (res, off, tag))))
           else 
             emit wb ` COMMENT "zero tag!");

          (* here we expect vas to be one or zero in length.
             (if it is a non-carrier, then we're supposed to
             store NULL, but NULL is zero, which is already there.) *)
          app (fn (Var va) => 
                  let in
                    withreg wb (getvar wb va) (fn cts => 
                    withreg wb (getlitro wb ` itow (1 + GC.gc_header_size)) (fn off =>
                    emit wb ` UPD (res, off, cts)))
                  end
                | _ => raise ToUM "bad int_t insides") vas;

          bindvar wb v res));

          basic wb c
        end

        | basic _ c = 
        let in
          print "Not implemented:\n";
          CPSPrint.printe c;
          print "\n";
          raise ToUM "CPS instruction not implemented in toum"
        end

      (* add a basic block to the blockset
         using the given label *)
      and add l b =
        blockset := (l, b) :: !blockset
        
      val f = basic (empty info lab) ce
          handle RegHelper (s, wb) => 
                 let in 
                   print s;
                   print " in CPS block:\n";
                   CPSPrint.printe ce;
                   dump wb;
                   raise RegHelper (s, wb)
                 end
      (* then add GC call and any other prologue *)

      val bodylab = V.namedvar (V.basename lab ^ "_body")

      val prolog =
        (* load the return address (bodylab) and
           then do a gc check with 'n' roots, where
           n is simply the number of arguments plus
           the number of globals that are always traced. *)
        (LITERAL_ADDR (Conventions.gc_returnreg, bodylab, 
                       Conventions.gc_notreturnreg) ::
         GC.gc_check (length args + Limits.TRACED_GLOBALS))
    in
      add bodylab (* ` (emitstring [hh] (V.basename lab ^ "\n")) @ *) f;
      add lab prolog;
      !blockset
    end

  (* When calling a function, we will need to
     initialize its arguments. To do so, we'll
     copy the actual parameters atop the formal
     parameters. But if the actual parameters are
     themselves stored in conflicting positions,
     we might overwrite them with each other in
     the process. This transformation moves the
     actual arguments into new temporary variables
     right before each call, if necessary. 
     
     also removes all deferred exps for the
     later passes. *)

  fun safeargs (lab, args, ce) =
    let
      fun sa (Deferred f) =
        (case Util.Oneshot.deref (f ()) of
           NONE => raise ToUM "unset oneshot"
         | SOME c => sa c)
        | sa (Fix _) = raise ToUM "cps not hoisted"
        | sa (Primop (a, b, c, e)) =
           Primop (a, b, c, map sa e)
        | sa (Sumswitch (a, num, b, el, e)) =
           Sumswitch (a, num, b, ListUtil.mapsecond sa el, sa e)
        | sa (Intswitch (a, el, e)) =
           Intswitch (a, ListUtil.mapsecond sa el, sa e)
        | sa (Project (a, b, c, e)) =
           Project(a, b, c, sa e)
        | sa (Alloc (a, b, c, e)) =
           Alloc(a, b, c, sa e)
        | sa (App(f, vas)) =
           (* we only have to do something if
              the variable is one of our own 
              arguments (PERF could also avoid
              it in some more situations) *)
           let
             fun isarg v = List.exists (fn v' => 
                                        V.eq(v, v')) args
             fun moveargs acc nil =
               App(f, rev acc)
               | moveargs acc (Var v :: rest) =
               if isarg v
               then 
                 let val vnew = V.namedvar "argcopy"
                 in
                   Primop(Primop.PBind, [Var v], [vnew], 
                          [moveargs (Var vnew :: acc) rest])
                 end
               else moveargs (Var v :: acc) rest
               | moveargs acc (x :: rest) = moveargs (x :: acc) rest
           in
             moveargs nil vas
           end

    in
      (lab, args, sa ce)
    end

  structure VS = SplaySetFn (type ord_key = V.var
                             val compare = V.compare)

  (* creates an assignment for every variable in the
     program to a stack slot. 

     one thing we have to be careful of is to not assign
     any variables into slots that will be arguments to
     functions that we call. So we first traverse the
     expression to find max_called_args and then start
     allocating at max(my_args, max_called_args).

     PERF? could keep stack smaller by allocating variables
     whose lifetimes do not overlap to the same stack slots.
     but there is no performance benefit to this that I know
     of, except if the number of stack slots grows HUGE and
     the offsets become larger than the literals we can load
     with a single instruction. (maybe some kind of run-time
     cache performance too, but that's not worth worrying 
     about...) *)
  fun assignargs fns =
    let
      val info = ref VVM.empty
      fun add (l, v, n) = 
        info := VVM.insert(!info, (l, v), (n, false))

      fun onefun (lab, args, ce) =
        let 
          val ctr = ref 0
          fun put v = (add (lab, v, !ctr);
                       if !ctr >= (Limits.STACK_SIZE - 1)
                       then raise ToUM "too many bound variables"
                       else ();
                       ctr := !ctr + 1)

          fun gma (Deferred _) = raise ToUM "gma/invt: Def"
            | gma (Fix _) = raise ToUM "gma/invt: Fix"
            | gma (Primop (_, _, _, cs)) =
            foldl Int.max 0 (map gma cs)
            | gma (Sumswitch (_, _, _, iel, def)) =
            foldl Int.max (gma def) (map (gma o #2) iel)
            | gma (Intswitch (_, iel, def)) =
            foldl Int.max (gma def) (map (gma o #2) iel)
            | gma (App (_, l)) = length l
            | gma (Project (_, _, _, c)) = gma c
            | gma (Alloc (_, _, _, c)) = gma c

          val max_called_args = gma ce
          val start_alloc = Int.max(max_called_args, length args)

          (*
          val () = print ("GMA for " ^ V.tostring lab ^ " is " ^
                          Int.toString max_called_args ^ " : " ^
                          Int.toString start_alloc ^ "\n")
          *)

          fun crawl (Deferred _) = raise ToUM "ass/invt: Def"
            | crawl (Fix _) = raise ToUM "ass/invt: Fix"
            | crawl (Primop (_, _, vs, cel)) =
            (app put vs; app crawl cel)
            | crawl (Sumswitch (_, _, v, iel, def)) =
            (put v;
             ListUtil.appsecond crawl iel;
             crawl def)
            | crawl (Intswitch (_, iel, def)) =
            (ListUtil.appsecond crawl iel;
             crawl def)
            | crawl (App _) = ()
            | crawl (Project (_, _, v, c)) =
            (put v; crawl c)
            | crawl (Alloc (_, _, v, c)) =
            (put v; crawl c)

          fun vars vas = List.mapPartial (fn (Var v) => SOME v
                                           | _ => NONE) vas

          fun notmember live (Var v) = not ` VS.member (live, v)
            | notmember live _ = false

          (* PERF should remove defs from live list *)
          fun add_kills (Deferred _) = raise ToUM "ass/invt: Def"
            | add_kills (Fix _) = raise ToUM "ass/invt: Fix"

            | add_kills (Primop (oper, vas, vs, cel)) =
              let val (live, cel) = foldr (fn (ce, (live, cel)) => 
                                              let val (live', ce) = add_kills ce
                                              in
                                                (VS.union (live, live'), ce :: cel)
                                              end) (VS.empty, nil) cel
                  val cel = case List.filter (notmember live) vas
                             of nil => cel
                              | kills => map (fn ce => Primop (PKill, kills, nil, [ce])) cel
              in
                (VS.addList (live, vars vas), Primop (oper, vas, vs, cel))
              end

            | add_kills (Sumswitch (va, num, v, iel, def)) =
              let val (live, iel) = foldr (fn ((i, e), (live, iel)) => 
                                              let val (live', e) = add_kills e
                                              in
                                                (VS.union (live, live'), (i, e) :: iel)
                                              end) (VS.empty, nil) iel
                  val (live', def) = add_kills def
                  val live = VS.union (live, live')
                  val (iel, def) = case List.filter (notmember live) [va]
                                    of nil => (iel, def)
                                     | kills => (map (fn (i, e) => (i, Primop (PKill, kills, nil, [e]))) iel,
                                                 Primop (PKill, kills, nil, [def]))
              in
                (VS.addList (live, vars [va]), Sumswitch (va, num, v, iel, def))
              end

            | add_kills (Intswitch (va, iel, def)) =
              let val (live, iel) = foldr (fn ((i, e), (live, iel)) => 
                                              let val (live', e) = add_kills e
                                              in
                                                (VS.union (live, live'), (i, e) :: iel)
                                              end) (VS.empty, nil) iel
                  val (live', def) = add_kills def
                  val live = VS.union (live, live')
                  val (iel, def) = case List.filter (notmember live) [va]
                                    of nil => (iel, def)
                                     | kills => (map (fn (i, e) => (i, Primop (PKill, kills, nil, [e]))) iel,
                                                 Primop (PKill, kills, nil, [def]))
              in
                (VS.addList (live, vars [va]), Intswitch (va, iel, def))
              end

            | add_kills (ce as App (f, vas)) = (VS.addList (VS.empty,
                                                           vars (f::vas)),
                                                ce)

            | add_kills (Project (n, va, v, c)) =
              let val (live, c) = add_kills c
                  val c = case List.filter (notmember live) [va]
                           of nil => c
                            | kills => Primop (PKill, kills, nil, [c])
              in
                (VS.addList (live, vars [va]), Project (n, va, v, c))
              end

            | add_kills (Alloc (t, vas, v, c)) =
              let val (live, c) = add_kills c
                  val c = case List.filter (notmember live) vas
                           of nil => c
                            | kills => Primop (PKill, kills, nil, [c])
              in
                (VS.addList (live, vars vas), Alloc (t, vas, v, c))
              end

          fun force_box_var v = 
              let in
                case VVM.find (!info, (lab, v))
                 of NONE => raise ToUM ("unknown var " ^ (V.tostring v))
                  | SOME (n, _) => info := VVM.insert (!info, (lab, v), (n, true))
              end

          fun force_box_val (Var v) = force_box_var v
            | force_box_val _ = ()

          fun force_box (Deferred _) = raise ToUM "esc/invt: Def"
            | force_box (Fix _) = raise ToUM "esc/invt: Fix"

            | force_box (Primop (p, vls, vrs, cel)) =
              let val () = case (p, vls, vrs) of
                             (PSet, [_, va], _) => force_box_val va
                           | (PGet, _, [v]) => force_box_var v
                           | (PRef, [va], [v]) => (force_box_val va; force_box_var v)
                           | (PUpdate, [_, _, velem], _) => force_box_val velem
                           | (PSub, _, [v]) => force_box_var v
                           | (PArray, [_, vinit], [v]) => (force_box_val vinit; force_box_var v)
                           | (PSethandler, [va], _) => force_box_val va
                              (* be conservative *)
                           | (PBind, [va], [v]) => (force_box_val va; force_box_var v)
                           | _ => ()
              in 
                app force_box cel
              end
            | force_box (Intswitch (_, icels, ce)) =
              let in
                app (fn (_, ce) => force_box ce) icels;
                force_box ce
              end
            | force_box (Sumswitch (_, _, v, icels, ce)) =
              let in 
                force_box_var v;
                app (fn (_, ce) => force_box ce) icels;
                force_box ce
              end
            | force_box (App (_, vs)) = app force_box_val vs
            | force_box (Project (_, _, v, ce)) = 
              let in
                force_box_var v;
                force_box ce
              end
            | force_box (Alloc (INT, [Int _], _, ce)) =
              (* Avoid boxing ints when possible *)
              force_box ce
            | force_box (Alloc (_, vls, v, ce)) = 
              let in
                app force_box_val vls;
                (* XXX is the following necessary? *)
                force_box_var v;
                force_box ce
              end

        in
          (* always assign arguments to sequential 
             stack slots starting at 0; this is the
             calling convention. *)
          app put args;
          (* now move to first local arg slot, which
             is after the args list by definition *)
          ctr := start_alloc;
          crawl ce;
          (* args must always by boxed *)
          app force_box_var args;
          force_box ce;

          (* add variable kills *)
          (lab, args, #2 (add_kills ce))
        end
    in
      (map onefun fns, !info)
    end

  fun printinfo info =
    let 
      val t = 
        ["lab", "var", "idx", "box?", "abs (hex)"] ::
        VVM.foldr op:: nil 
        (VVM.mapi (fn ((f, v), (i, b)) =>
                   [V.tostring f, V.tostring v,
                    Int.toString i, 
                    Bool.toString b,
                    Word32.toString (itow (Limits.STACK_START + i))]) info)
    in
      print "Variable assignment:\n";
      print (StringUtil.table 75 t);
      print "\n"
    end

  fun convert (Fix(fns, App(Label main, nil))) =
    let
      val _ = print "  safeargs..\n"
      val fns  = map safeargs fns


      val _ = print "Safeargs:\n"
      val _ = CPSPrint.printe (Fix(fns, App(Label main, nil)))


      val _ = print "  assignargs..\n"
      val (fns, info) = assignargs fns

        (* ENH could print this to a file for debuggers *)
      (* val _ = printinfo info *)
      val code = map (convertfn info (length fns)) fns
    in
      ( List.concat code,
        main )
    end
    | convert _ = raise ToUM "cps in wrong form"


end
