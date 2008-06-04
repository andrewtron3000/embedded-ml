(* some code for self-checking the UM's correctness. *)

structure SelfCheck =
struct

(* To-do:

   LOADPROG should copy, not make aliases.
   can copy, then modify the old one, then check
   that the result is not mirrored.

*)

  val sc_verbose = Params.flag false
    (SOME ("-scverbose",
           "be verbose in selfcheck")) "scverbose"

  exception SelfCheck of string
  structure V = Variable
  
  local open Conventions UMA

  fun debugsay s =
    (* nb. temporarily uses zero reg, and then restores it *)
    if !sc_verbose
    then MANY [MANY(emitstring [zz] s),
               (* restore.. *)
               LITERAL(zz, 0w0)]
    else MANY []


  in
    val sclab = Variable.namedvar "selfcheck"

    val too_low  = errormsg "LOADPROG off (low)" @ [HALT]
    val too_high = errormsg "LOADPROG off (high)" @ [HALT]

    (* must be placed at 0 address 
       WARNING: these specific instruction and their order is very brittle
       *)
    val selfcheck0 =
      [

      (* these two instructions do nothing
         if we're in the right byte order. *)
         SWAPEND(LITERAL(aa, W.fromInt 8)),  
         SWAPEND(LOADPROG (zz, aa)),     

         (* now test that LOADPROG doesn't
            have off-by-one errors *)

         LITERAL(bb, 0w20),
         LITERAL(cc, W.fromInt (23 + length too_low + length too_high)),
         LITERAL(dd, W.fromInt (20 + length too_low)),
         LITERAL(aa, 0w13),

         LOADPROG(zz, aa),

         (* landing pad *)

         CMOV(aa, aa, aa),
         SWAPEND(LITERAL(aa, W.fromInt(27 + length too_low + length too_high))),
         SWAPEND(LOADPROG(zz, aa)),

         LOADPROG(zz, bb),
         LOADPROG(zz, bb),
         LOADPROG(zz, bb),
         LOADPROG(zz, cc),
         LOADPROG(zz, dd),
         LOADPROG(zz, dd),
         LOADPROG(zz, dd),

         (* too low *)
         CMOV(aa, aa, aa),
         CMOV(aa, aa, aa),
         CMOV(aa, aa, aa)]
         @ too_low @

         (* too high! *)
          [CMOV(aa, aa, aa),
           CMOV(aa, aa, aa),
           CMOV(aa, aa, aa)]
           @ too_high @

         (* just right! *)
          [LITERAL_ADDR (aa, sclab, hh),
           LOADPROG(zz, aa)] @

         (* endian error *)
         map SWAPEND 
         ([
          
          (* padding. land anywhere around
             here and you'll get a "bad endianness"
             error.  *)
          
          CMOV(aa, aa, aa),
          CMOV(aa, aa, aa),
          CMOV(aa, aa, aa),
          CMOV(aa, aa, aa),
          CMOV(aa, aa, aa),
          CMOV(aa, aa, aa),
          CMOV(aa, aa, aa)]
         
         @ errormsg "endian\n" @
         
         [HALT, SWAPEND HALT])

  fun selfcheck_code lab =
    let

      val fails = ref nil

      fun test_fail s =
        let
          val v = Variable.namedvar "test_fail"
        in
          fails := 
              (v, [
                   MANY (errormsg (s ^ "\n")),
                   HALT
                   ]) ::
              !fails;
          v
        end

      val alloc_size = 11

      val nandbadlab = V.namedvar "nand_bad"
      val testnandlab = V.namedvar "test_nand"

      val addbadlab = V.namedvar "add_bad"
      val testaddlab = V.namedvar "test_add"

      val mulbadlab = V.namedvar "mul_bad"
      val testmullab = V.namedvar "test_mul"

      val litbadlab = V.namedvar "lit_bad"
      val testlitlab = V.namedvar "test_lit"

      val divbadlab = V.namedvar "div_bad"
      val testdivlab = V.namedvar "test_div"

      val asubupd_paddedlab = V.namedvar "asubupd_padded"
      val asubupd_datalab = V.namedvar "asubupd_data"

    in
      (* code that must be at 0 *)
      (selfcheck0,

     (* relocatable code *)
      [(sclab, 
        [
         (* Make sure at least one of the registers is zero *)
         JNZ(zz, test_fail "reg 6 not zero", aa, bb, cc),
         (* zz = 0w0 *)

         (* ENH test jreed's ADD bug *)
         (* ENH test for no sign extension *)
         (* ENH test large literals *)

         (* ENH test loadprog should copy *)

         (* NAND *)
         JZ (zz, testnandlab, ee, ff, hh),

         LABEL nandbadlab,
         MANY (errormsg "NAND error"),
         HALT,

         LABEL testnandlab,
         LITERAL (aa, 0w0),
         LITERAL (bb, 0w0),
         NAND (cc, aa, bb),
         JZ (cc, nandbadlab, ee, ff, hh),

         LITERAL (aa, 0w0),
         LITERAL (bb, 0w1),
         NAND (cc, aa, bb),
         NAND (dd, bb, aa),
         NAND (aa, cc, dd),
         JNZ (aa, nandbadlab, ee, ff, hh),

         LITERAL (aa, 0wx2A),
         XOR (bb, aa, aa, ff),
         JNZ (bb, nandbadlab, ee, ff, hh), 

         (* ADD *)
         JZ (zz, testaddlab, ee, ff, hh),

         LABEL addbadlab,
         MANY (errormsg "ADD error"),
         HALT,

         LABEL testaddlab,
         LITERAL (aa, 0w0),
         LITERAL (bb, 0w234),
         ADD (cc, aa, bb),
         LITERAL (ee, W.+(0w0, 0w234)),
         XOR (dd, cc, ee, ff),
         JNZ (dd, addbadlab, ee, ff, hh),

         LITERAL (aa, 0w72),
         LITERAL (bb, 0w831),
         ADD (cc, aa, bb),
         LITERAL (ee, W.+(0w72, 0w831)),
         XOR (dd, cc, ee, ff),
         JNZ (dd, addbadlab, ee, ff, hh),

         (* same reg *)
         LITERAL (aa, 0w72),
         LITERAL (bb, 0w831),
         ADD (aa, aa, bb),
         LITERAL (ee, W.+(0w72, 0w831)),
         XOR (dd, aa, ee, ff),
         JNZ (dd, addbadlab, ee, ff, hh),

         (* MUL *)
         JZ (zz, testmullab, ee, ff, hh),

         LABEL mulbadlab,
         MANY (errormsg "MUL error"),
         HALT,

         LABEL testmullab,
         LITERAL (aa, 0w123),
         LITERAL (bb, 0w0),
         MUL (cc, aa, bb),
         LITERAL (ee, W.*(0w123, 0w0)),
         XOR (dd, cc, ee, ff),
         JNZ (dd, mulbadlab, ee, ff, hh),

         LITERAL (aa, 0w2),
         LITERAL (bb, 0w7),
         MUL (cc, aa, bb),
         LITERAL (ee, W.*(0w2, 0w7)),
         XOR (dd, cc, ee, ff),
         JNZ (dd, mulbadlab, ee, ff, hh),

         (* same reg *)
         LITERAL (aa, 0w2),
         LITERAL (bb, 0w7),
         MUL (bb, aa, bb),
         LITERAL (ee, W.*(0w2, 0w7)),
         XOR (dd, bb, ee, ff),
         JNZ (dd, mulbadlab, ee, ff, hh),
         
         (* overflow *)
         LITERAL (aa, 0wxF0000),
         LITERAL (bb, 0wx10000),
         MUL (cc, aa, bb),
         LITERAL (ee, W.*(0wxF0000, 0wx10000)),
         XOR (dd, cc, ee, ff),
         JNZ (dd, mulbadlab, ee, ff, hh),

         (* LARGER LITERALS *)
         JZ (zz, testlitlab, ee, ff, hh),
         LABEL litbadlab,
         MANY (errormsg "Orthography failed"),
         HALT,

         LABEL testlitlab,
         NAND (aa, zz, zz),
         LITERAL (aa, 0w0),
         JNZ (aa, litbadlab, ee, ff, hh),

         LITERAL (aa, 0wx1FFFFFF),
         (* construct the same literal using maths *)
         LITERAL (bb, 0wx1FF),
         LITERAL (cc, 0wx10000),
         MUL (bb, bb, cc),
         LITERAL (cc, 0wxFFFF),
         ADD (bb, bb, cc),
         XOR (dd, aa, bb, ee),
         JNZ (dd, litbadlab, ee, ff, hh),
         
         (* DIV *)
         JZ (zz, testdivlab, ee, ff, hh),

         LABEL divbadlab,
         MANY (errormsg "DIV error"),
         HALT,

         LABEL testdivlab,
         LITERAL (aa, 0w789),
         LITERAL (bb, 0w1),
         DIV (cc, aa, bb),
         LITERAL (ee, W.div(0w789, 0w1)),
         XOR (dd, cc, ee, ff),
         JNZ (dd, divbadlab, ee, ff, hh),

(*         LABEL testdivlab, *)
         LITERAL (aa, 0w1),
         NAND (bb, zz, zz),
         DIV (cc, aa, bb),
         LITERAL (ee, 0w0),
         XOR (dd, cc, ee, ff),
         JNZ (dd, divbadlab, ee, ff, hh),

         (* ASUB/UPD *)
         (* NB must agree with data below *)
         LITERAL_ADDR (aa, asubupd_datalab, ee),
         ASUB (bb, zz, aa),
         LITERAL (cc, 0w654321),
         XOR (dd, bb, cc, ee),
         JNZ (dd, test_fail "Index in 0-array fail", ee, ff, hh),

         LITERAL (cc, 0wxF0F0),
         UPD (zz, aa, cc),
         ASUB (bb, zz, aa),
         XOR (dd, bb, cc, ee),
         JNZ (dd, test_fail "Index/Amend in 0-array fail", ee, ff, hh),
       
         (* ALLOC/FREE/ASUB/UPD *)
         (* alloc empty *)
         debugsay "trying to Allocate array of size 0..\n",
         ALLOC (aa, zz),
         JZ (aa, test_fail "new ALLOC must be non-0", ee, ff, hh),

         (* free it *)
         debugsay "trying to Abandon size 0 allocation..\n", 
         FREE (aa),

         (* init to 0 *)
         (* words not bytes *)
         debugsay ("trying to Allocate size " ^ Int.toString alloc_size ^ "..\n"),
         LITERAL (bb, W.fromInt alloc_size),
         ALLOC(aa, bb),
         debugsay "trying Array Index on allocated array..\n",
         MANY (List.foldr op@ nil
           (List.tabulate (alloc_size,
              (fn i => [LITERAL (cc, W.fromInt i),
                        ASUB(bb, aa, cc),
                        JNZ(bb, test_fail "array not init to 0", ee, ff, hh)])))),

         (* update *)
         debugsay "trying Amendment of allocated array..\n",
         MANY (List.foldr op@ nil
           (List.tabulate (alloc_size,
              (fn i => [LITERAL (cc, W.fromInt i),
                        LITERAL (bb, W.fromInt (i + alloc_size)),
                        UPD(aa, cc, bb)])))),

         debugsay "checking Amendment of allocated array..\n",
         COMMENT "INDEX / AMEND",
         MANY (List.foldr op@ nil
           (List.tabulate (alloc_size,
              (fn i => [LITERAL (cc, W.fromInt i),
                        ASUB(bb, aa, cc),
                        LITERAL (ee, W.fromInt (i + alloc_size)),
                        SUB (dd, ee, bb, ff),
                        JNZ(dd, test_fail "INDEX / AMEND", 
                            ee, ff, hh)])))),
         FREE (aa),

         (* alloc and update, then save it for a while *)
         LITERAL (aa, 0w8),
         ALLOC (hh, aa),
         LITERAL (aa, 0w3),
         LITERAL (bb, 0wxCAFE),
         UPD (hh, aa, bb),
         
         (* same reg *)
         debugsay "trying Alloc(a,a) and amending it..\n",
         LITERAL (aa, 0w4000),
         ALLOC (aa, aa),
         LITERAL (bb, 0w3999),
         UPD (aa, bb, bb),
         FREE (aa),

         (* no reuse *)
         debugsay "comparing multiple allocations..\n",
         LITERAL (cc, 0w4),
         ALLOC (aa, cc),
         ALLOC (bb, cc),
         SUB (cc, aa, bb, ee),
         JZ (cc, test_fail "ALLOC not unique", ee, ff, dd),

         (* ptr arith *)
         debugsay "pointer arithmetic..\n",
         XOR (cc, aa, bb, ee),
         LITERAL (bb, 0w0),
         UPD (aa, bb, cc),
         ASUB (dd, aa, bb),
         XOR (bb, aa, dd, ee),
         FREE (aa),
         FREE (bb),

         (* check that the thing we alloced before is still there *)
         debugsay "check old allocation..\n",
         LITERAL (aa, 0w3),
         ASUB (cc, hh, aa),
         LITERAL (bb, 0wxCAFE),
         SUB (dd, bb, cc, ee),
         JNZ (dd, test_fail "INDEX / AMEND old alloc fail", ee, ff, bb),


         debugsay "simple tests ok!\n",
         (* jump off to lab *)
         LITERAL_ADDR (bb, lab, ff),
         LOADPROG (zz, bb)
         ]),

       (asubupd_paddedlab,
        [DATA 0w0,
         DATA 0w0,
         LABEL asubupd_datalab,
         DATA 0w654321,
         DATA 0w0,
         DATA 0w0])
       ]
      @ !fails)
    end
  end

  fun wrap (blocks, main) =
    let
      val (schead, sc_blocks) = selfcheck_code main
    in
      (schead, sc_blocks @ blocks)
    end

end
