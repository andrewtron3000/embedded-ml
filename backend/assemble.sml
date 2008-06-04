
(* Assembly expands macro-ops,
   does label assignment, and
   generates an executable. *)

structure Assemble =
struct

  val writeasm = Params.flag true
    (SOME ("-writeasm", "Write .uma 'assembly' file")) "writeasm"

  val prof = Params.flag true
    (SOME ("-prof", "Write .shadow and .syms profiling information")) "prof"

  open UMA

  structure GA = GrowArray
  structure V = Variable
  structure VM = V.Map
  exception Assemble of string

  infixr 1 `
  fun a ` b = a b

  (* conventions for code generation, including
     the zero register *)
  open Conventions

  val cr0 = V.namedvar "__cr0"

  (* expands macro-ops within an instruction stream. The only macro
     instructions this will leave is LITERAL_ADDR and DATALAB, since
     we need a more complex patching scheme for those. *)

  fun expand_ops il =
    let
      fun oneop (SUB (a, b, c, tmp)) =
        (if a = tmp orelse b = tmp
         then raise Assemble "reg conflict in SUB" else ();
        [NAND (tmp, c, c), (* c = ~c *)
         ADD (a, b, tmp),
         LITERAL (tmp, 0w1),
         ADD (a, tmp, a)])

        | oneop (DEC (a, tmp)) =
        [NAND (tmp, zz, zz),
         ADD (a, tmp, a)]

        | oneop (DECTO (a, b)) =
        [NAND (a, zz, zz),
         ADD (a, b, a)]

        | oneop (OR (a, b, c, tmp)) =
        (if a = tmp orelse c = tmp
         then raise Assemble "reg conflict in OR" else ();
        [NAND (tmp, b, b), (* tmp = ~b *)
         NAND (a, c, c),   (* a = ~c *)
         NAND (a, tmp, a)])

        | oneop (AND (a, b, c)) =
        [NAND (a, b, c),
         NAND (a, a, a)]

        | oneop (XOR (a, b, c, tmp)) =
        (if a = b orelse a = c orelse a = tmp
            orelse b = tmp orelse c = tmp
         then raise Assemble "reg conflict in XOR" else ();
        expand_ops 
        [OR (a, b, c, tmp),
         NAND (tmp, b, c),
         NAND (a, tmp, a),
         NAND (a, a, a)])
        | oneop (NOT (a, b)) = [NAND (a, b, b)]
        | oneop (LITERAL_ANY (dst, w, tmp)) =
        if W.andb(W.<<(0wxFFF, 0w25), w) <> 0w0
        then 
        (* PERF for most negative numbers, 
           can do it in two instructions with
           NOT *)

          (* if low bits are zero, don't load them. 
             PERF we can get 25(?) bits like this,
             not just 16
             *)
          (if W.andb(w, 0wxFFFF) <> 0w0 then
              [LITERAL(dst, W.>>(w, 0w16)),
               LITERAL(tmp, 0wx10000),
               MUL(dst, tmp, dst),
               LITERAL(tmp, W.andb(w, 0wxFFFF)),
               ADD(dst, tmp, dst)]
           else
             (* low bits zero; just shift *)
             [LITERAL(dst, W.>>(w, 0w16)),
              LITERAL(tmp, 0wx10000),
              MUL(dst, tmp, dst)])

        else [LITERAL(dst, w)]
        | oneop (MANY l) = List.concat (map oneop l)

        | oneop normal = [normal]

    in
      List.concat (map oneop il)
    end

  (* makes a program into a vector of words.
     puts the 'header' code at position 0,
     which must be smaller than Limits.MAX_CR0.
     Must back-patch jumps to labels whose addresses 
     are not yet known. *)
  fun linearize { for_humlock, obfuscate } f header blocks =
    let
      fun ++ p = p := !p + 1

      (* for profiling, we have a collection of symbols *)
      local
        val num_syms = ref 0

        val allsyms = ref nil
      in
        (* assuming it doesn't already exist *)
        fun add_sym str =
          let 
            val this = W.fromInt (!num_syms)
          in
            allsyms := V.tostring str :: !allsyms;
            ++ num_syms;
            this
          end
        
        fun getsyms () = rev (!allsyms)
      end

      val sym_data = add_sym (V.namedvar ".DATA")

      (* addresses that need to be backpatched
         with literal loads. *)
      val backlist = ref nil
      fun backpatch addr sym dest reg =
        backlist := (addr, sym, dest, reg) :: !backlist

      (* same, but for DATALAB instead of LITERAL *)
      val bdatalist = ref nil
      fun backdata addr sym dest =
        bdatalist := (addr, sym, dest) :: !bdatalist

      val labs = ref VM.empty
      fun checkduplab l =
        (case VM.find(!labs, l) of
           NONE => ()
         | SOME _ => raise Assemble ("duplicate label: " ^ V.tostring l))

      fun addlab l loc =
        (checkduplab l;
         labs := VM.insert(!labs, l, loc))

      fun getlab l =
        case VM.find(!labs, l) of
          NONE => raise Assemble ("can't find label " ^ V.tostring l)
        | SOME loc => loc

      val prog = GA.empty ()
      val shadow = GA.empty ()

      val pos = ref 0

      fun putw w s =
        let in
          GA.update prog (!pos) w;
          GA.update shadow (!pos) s;
          ++ pos
        end

      fun emit s i = 
          let val (w, mask) = encodemask i

              val w = if obfuscate
                      then 
                          (W.orb(W.andb(W.notb mask,
                                        HumlockUtil.randomword()),
                                 w))
                      else w
          in
              putw w s
          end

      (* assuming small addresses for now *)
      fun emit_la s (dst, var, tmp) =
        let
        in
          backpatch (!pos) s var dst;

          (* skip one inst (linearization will fail later
             if this is not backpatched) *)
          ++ pos
        end
      fun emit_la_small s (dst, var) = 
          (* SUSP this assume the above implementation 
            uses small literals *)
          emit_la s (dst, var, fn () => raise Assemble "small addr?")

      (* as above, but this need make no assumptions *)
      fun emit_dl s dst =
        let
        in
          backdata (!pos) s dst;
          ++ pos
        end

      fun zero_regions () =
        let in
          (* check that we didn't already emit too much *)
          if (!pos) > Limits.MAX_CR0
          then raise Assemble "cr0 too big"
          else ();
          
          (* zero out everything up to the code start. 
             at some point we might want to include
             global data here. *)

          (* Zero gc region *)
          while(!pos) < Limits.STACK_START
            do putw 0wx00000000 sym_data;

          (* sentinels in stack region *)
          while (!pos) < Limits.CODE_START
             do putw 0wxFF00FF00 sym_data
        end

      (* emit a single block in the current location. *)
      fun doblock (lab, ins) =
        let 
          val sym_me = add_sym lab

          fun em (LITERAL_ADDRO (d, v, SOME t) :: rest) = (emit_la sym_me (d, v, t);
                                                           em rest)
            | em (LITERAL_ADDRO (d, v, NONE) :: rest) = (emit_la_small sym_me (d, v);
                                                         em rest)
            | em (DATALAB v :: rest) = (emit_dl sym_me v; em rest)

            | em (LABEL v :: rest) = (addlab v (!pos); em rest)

            (* could do conditional jumps here. *)
            | em (JNZO (testreg, v, t1, t2, t3) :: rest) =
            let 
              val ft = V.namedvar "jnz_fallthrough"
            in
              em [LITERAL_ADDRO(t1, v, t3),
                  LITERAL_ADDRO(t2, ft, t3),
                  CMOV(t2, t1, testreg),
                  LOADPROG(zz, t2)];

              (* emit fallthrough label *)
              addlab ft (!pos);

              em rest
            end
            | em (JZO (testreg, v, t1, t2, t3) :: rest) =
            let 
              val ft = V.namedvar "jz_fallthrough"
            in
              em [LITERAL_ADDRO(t1, v,  t3),
                  LITERAL_ADDRO(t2, ft, t3),
                  CMOV(t1, t2, testreg),
                  LOADPROG(zz, t1)];

              (* emit fallthrough label *)
              addlab ft (!pos);

              em rest
            end
            | em (JLZO (testreg, v, t1, t2, t3) :: rest) =
            let 
              val ft = V.namedvar "jlz_fallthrough"
            in
              (em o expand_ops)
                  [LITERAL_ANY (t1, 0wx80000000, t2),
                   AND (testreg, testreg, t1),
                   LITERAL_ADDRO(t1, ft,  t3),
                   LITERAL_ADDRO(t2, v, t3),
                   CMOV(t1, t2, testreg),
                   LOADPROG(zz, t1)];

              (* emit fallthrough label *)
              addlab ft (!pos);

              em rest
            end

            | em (CALLWITH (a, lab, t1, t2) :: rest) =
            let
              val next = V.namedvar "call_ret"
            in
              em [LITERAL_ADDR(a, next, t1),
                  LITERAL_ADDR(t2, lab, t1),
                  LOADPROG(zz, t2)];

              (* fallthrough ... *)
              addlab next (!pos);
              em rest
            end

          (* ignore .. *)
            | em (COMMENT _ :: rest) = em rest
            | em (i :: rest) = (emit sym_me i; em rest)
            | em nil = ()
        in
          case f of
            SOME f =>
              let in
                TextIO.output (f, "assemble " ^ V.tostring lab ^ " @ " ^ 
                               Int.toString (!pos) ^ ":\n");
                app (fn i =>
                     TextIO.output (f, "  " ^ UMAPrint.intos i ^ "\n")) ins;
                TextIO.output(f, "\n")
              end
          | _ => ();

          addlab lab (!pos);
          em ins
        end

      (* cr0 is the self-check and initialization code. *)
      fun writecr0 () = doblock (cr0, header)

      fun dopatch () = 
        let in
        (* for each entry in backlist *)
        app (fn (i, sym, dest, reg) =>
             let in
                 (* no free bits for obfuscation here. *)
               GA.update prog i 
                 (encode (LITERAL (reg, W.fromInt (getlab dest))));
               GA.update shadow i sym
             end) (!backlist);

        app (fn (i, sym, dest) =>
             let in
               GA.update prog i (W.fromInt (getlab dest));
               GA.update shadow i sym
             end) (!bdatalist)

        end
    in
      writecr0 ();
      (if for_humlock then zero_regions ()
       else ());
      (* should be at code_start now *)
      app doblock blocks;
      (* now backpatch *)
      dopatch ();

      (GA.finalize prog,
       GA.finalize shadow,
       getsyms (),
       !labs)
    end

  fun printprogto f (blocks, main) =
    let in
      
      TextIO.output(f, "PROGRAM (entry = " ^ V.tostring main ^ "):\n");
      app (fn (l, is) =>
           let in
             TextIO.output (f, V.tostring l ^ ":\n");
             app (fn i =>
                  TextIO.output (f, "  " ^ UMAPrint.intos i ^ "\n")) is
           end) blocks
    end

  (* give the code to be placed at 0, and then the blocks comprising the
     rest of the code. *)
  fun assemble_stream opts basef (header, blocks) =
    let
      val asmf = if !writeasm
                 then SOME (TextIO.openOut (basef ^ ".uma"))
                 else NONE

      val blocks = ListUtil.mapsecond expand_ops blocks

      (* get assembled data, shadow data of same length 
         and symbol table too *)
      val (dat, shad, syms, labmap) = linearize opts asmf (expand_ops header) blocks

      fun bytestream d =
        let
          fun byte w = Word8.fromInt (W.toInt (W.andb(w, 0w255)))
        in
            SimpleStream.flatten `
            SimpleStream.map 
            (fn w =>
             SimpleStream.fromlist `
             map byte [W.>> (w, 0w24),
                       W.>> (w, 0w16),
                       W.>> (w, 0w8),
                       w]) (SimpleStream.fromarray d)
        end

    in
      (SimpleStream.map (fn w =>
                         (if w > 0w60000
                          then print "very large sym ???"
                          else ();
                            w))) (SimpleStream.fromarray shad);
      

      (* maybe write profile data *)
      (if !prof
       then
         let in
           (* bin names *)
           StringUtil.writefile (basef ^ ".sym")
            (StringUtil.delimit "\n" syms);

           (* shadow file, aka "bins" *)
           SimpleStream.tobinfile (basef ^ ".bins") 
             (bytestream shad)
         end
       else ());

      (* close files we might have opened *)
      ignore (Option.map TextIO.closeOut asmf);
      (bytestream dat, labmap)
    end

  datatype target =
    TARG_HEADER | TARG_UM

  fun assemble { for_humlock, target, obfuscate } outfile prog =
    let
      val (bs, _) = assemble_stream { for_humlock = for_humlock,
                                      obfuscate = obfuscate } (#1 (FSUtil.splitext outfile)) prog
    in
      (case target of
         TARG_UM => SimpleStream.tobinfile outfile bs
       | TARG_HEADER => Header.write outfile bs)
    end

end
