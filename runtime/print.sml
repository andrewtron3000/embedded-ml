
(* code for self-printing *)
structure SelfPrint =
struct

    val revision = Params.param "7"
        (SOME ("-revision",
               "for self printer, revision number displayed")) "revision"

  exception Print of string

  structure V = Variable
  open Conventions
  open UMA

  val lab_prdone = V.namedvar "printdone"
  val lab_print = V.namedvar "printmain"
  val lab_prloop = V.namedvar "printloop"
  val lab_nwords = V.namedvar "prnwords"
  val lab_prog = V.namedvar "prprog"

  val lab_dynamic = V.namedvar "sp_dynamicregion"

  fun pr_code dynoff =
    let
      (* trashes ff, hh *)
      fun setvar var reg =
        MANY[LITERAL_ADDR(hh, var, ff),
             UPD(gg, hh, reg)]

      fun getvar reg var =
        MANY[LITERAL_ADDR(hh, var, ff),
             ASUB(reg, gg, hh)]

      (* copy from lsrc+off for n bytes to zz[lab_prog + adst] *)
      fun copylab (lsrc, _)   0 adst = nil
        | copylab (lsrc, off) n adst =
        LITERAL_ADDR(bb, lsrc, hh) ::
        LITERAL(ff, W.fromInt off) ::
        ADD(bb, bb, ff) ::
        ASUB(bb, zz, bb) ::
        LITERAL_ADDR(ff, lab_prog, hh) ::
        LITERAL_ANY(ee, adst, hh) ::
        ADD(ee, ff, ee) ::
        UPD(zz, ee, bb) ::
        copylab (lsrc, off + 1) (n - 1) (adst + 0w1)


      val lab_prompt = V.namedvar "prompt"
      val lab_read   = V.namedvar "read"
      val lab_exit = V.namedvar "exit"
    in
      (lab_dynamic,
       List.tabulate(Runtime.DYNAMIC_WORDS,
                     fn _ => DATA 0w0)) ::

      (lab_exit,
       [
        MANY (emitstring [aa, bb, cc, dd, ee, ff, hh]
              "See you soon!\n"),
        HALT]) ::

      (lab_print,
       [
	(* ENH:
	   (different formats? hex, binary, exit, etc.) *)
        MANY (emitstring [aa, bb, cc, dd, ee, ff, hh]
              ("\n == CBV ARCHIVE ==\n" ^
               "    VOLUME ID " ^ !revision ^ "\n\n" ^
               " Choose a command:\n\n" ^
               " p) dump UM data\n" ^
               " x) exit\n")),


        LABEL lab_prompt,

        MANY (emitstring [aa, bb, cc] "\n? "),

        LABEL lab_read,

        (* get char *)
        READ aa,

        (* make lowercase *)
        LITERAL(bb, 0w32),
        OR(aa, bb, aa, hh),
        
        (* is it x? exit *)
        LITERAL(bb, Word32.fromInt (ord #"x")),
        SUB(bb, aa, bb, hh),
        JZ (bb, lab_exit, ff, dd, hh),

        (* is it p? print *)
        LITERAL(bb, Word32.fromInt (ord #"p")),
        SUB(bb, aa, bb, hh),
        JNZ(bb, lab_prompt, ff, dd, hh),
        
        (* print time! *)
        MANY (emitstring [aa, bb, cc, dd, ee, ff, hh]
              "UM program follows colon:"),

        (* need to copy dynamic region (from 0 page to 0 page) *)
        MANY (copylab (lab_dynamic, 0) Runtime.DYNAMIC_WORDS dynoff),
  
        LITERAL_ADDR (aa, lab_prog, hh),

        getvar bb lab_nwords,
        ADD(bb, aa, bb),
        LITERAL_ADDR (cc, lab_prloop, hh),
        LOADPROG(zz, cc)]) ::

      (lab_prloop,
       [SUB(cc, aa, bb, hh),
        JZ(cc, lab_prdone, dd, ee, ff),
        
        (* otherwise, get this word and 
           print it *)
        ASUB(cc, zz, aa),
        
        LITERAL(dd, 0w255),
        LITERAL(ee, 0w256),

        DIV(ff, cc, ee),
        DIV(ff, ff, ee),
        DIV(ff, ff, ee),
        AND(ff, ff, dd),
        WRITE ff,

        DIV(ff, cc, ee),
        DIV(ff, ff, ee),
        AND(ff, ff, dd),
        WRITE ff,

        DIV(ff, cc, ee),
        AND(ff, ff, dd),
        WRITE ff,

        AND(cc, cc, dd),
        WRITE cc,

        (* increment counter *)
        LITERAL(ee, 0w1),
        ADD(aa, aa, ee),

        LITERAL_ADDR(cc, lab_prloop, hh),
        LOADPROG(zz, cc)]) ::

      (lab_prdone, [HALT]) :: nil
    end

  fun words prog =
    let
      fun to32 w8 = W.fromInt (Word8.toInt w8)

(*        
      (* n.b. this reverses the normal byte order, which 
         makes it easier on the out-end. *)
      fun progword () =
          case (prog (), prog (), prog (), prog ()) of
              (SOME a, SOME b, SOME c, SOME d) =>
                  SOME
                  (DATA (W.orb
                         (W.<<(to32 d, 0w24),
                          W.orb
                          (W.<<(to32 c, 0w16),
                           W.orb
                           (W.<<(to32 b, 0w8), to32 a)))))
                      
            | (NONE,   NONE,   NONE,   NONE) => NONE
            | _ => raise Print "byte stream not multiple of four"
*)

      (* back to normal order *)
      fun progword () =
          case (prog (), prog (), prog (), prog ()) of
              (SOME a, SOME b, SOME c, SOME d) =>
                  SOME
                  (DATA (W.orb
                         (W.<<(to32 a, 0w24),
                          W.orb
                          (W.<<(to32 b, 0w16),
                           W.orb
                           (W.<<(to32 c, 0w8), to32 d)))))
                      
            | (NONE,   NONE,   NONE,   NONE) => NONE
            | _ => raise Print "byte stream not multiple of four"
      
    in
      SimpleStream.tolist progword
    end

  fun self_print ((prog, labmap), dynlab) =
    let
      val cdata = words prog

      val dynoff = 
        (case V.Map.find (labmap, dynlab) of
           NONE => 
             raise Print ("can't wrap with " ^
                          "self-print because can't " ^
                          "find dynamic region " ^
                          V.tostring dynlab)
         | SOME off => W.fromInt off)
    in
        (pr_code dynoff @
         [(lab_prog, cdata),
          (lab_nwords, [DATA (W.fromInt (length cdata))])],
         lab_print)
    end

end
