(* self-checking wrapper for
   loadprog(nonzero). A bit expensive,
   so we might not want this to be
   a part of every UM binary. *)

structure SCLoadprog =
struct

  exception SCLoadprog of string

  structure V = Variable
  open Conventions
  open UMA

  val sc_verbose = Params.flag false
    (SOME ("-scverbose",
	   "be verbose in selfcheck")) "scverbose"

  structure W8 = Word8
  structure W8A = Word8Array
  structure W8V = Word8Vector

  (* we don't have a dynamic region. *)
  val lab_dynamic = V.namedvar "sc_loadprog_dynamic(never-used)"

  val lab_selfcheck = V.namedvar "selfcheck"
  val lab_program = V.namedvar "program"

  val lab_copytop = V.namedvar "copytop"
  val lab_copydone = V.namedvar "copydone"


  fun debugsay s =
    (* nb. temporarily uses zero reg, and then restores it *)
    if !sc_verbose
    then MANY [MANY(emitstring [zz] s),
	       (* restore.. *)
	       LITERAL(zz, 0w0)]
    else MANY []

  (* This special CR0 is the first thing that's executed 
     after we do the LOADPROG. It should jump to the
     label main. This one trashes the ee and hh registers
     but nothing else. Then it runs some self-check code
     before actually jumping to main. 

     in this code, aa should be the thing we just loadprog'd,
     and cc should be its length in words
     
     *)
  fun cr0 (blocks, main) =
    let 
      val lab_doselfcheck = V.namedvar "doselfcheck"
      val lab_samedone = V.namedvar "samedone"
      val lab_sametop = V.namedvar "sametop"
      val lab_not_equal_after = V.namedvar "not_equal_after"

      val lab_test_alias = V.namedvar "test_alias"
      val lab_test_alias2 = V.namedvar "test_aliasII"
      val lab_alias_old_to_0 = V.namedvar "oldto0"
      val lab_alias_0_to_old = V.namedvar "0toold"

      val lab_writea_fail = V.namedvar "writea"
      val lab_write0_fail = V.namedvar "write0"
    in
      ((* this must be a fixed length *)
       [LITERAL_ADDR(ee, lab_doselfcheck, hh),
	LOADPROG(zz, ee),
	DATA 0w8
	],

       (* this can be anything. *)
       (lab_doselfcheck,
	[
	 debugsay "success.\n",

	 (* do checks.

	    we should be in a state where
	    aa points to an array that is
	    identical to the zero array. *)
	    
	    (* verify that they have the same
	       contents. *)

	    debugsay "verifying that the array and its copy are the same...\n",
	    LITERAL_ADDR(bb, lab_sametop, hh),

	    (* bb : sametop
	       cc : words left,
	       aa : old array, *)
	       
	    LABEL lab_sametop,
	    JZ(cc, lab_samedone, ee, ff, hh),

	    DEC(cc, hh),

	    ASUB(ee, zz, cc),
	    ASUB(dd, aa, cc),
	    (* check equal. *)
	    SUB(ee, dd, ee, hh),
	    JNZ(ee, lab_not_equal_after, dd, ff, hh),

	    (* always jump back *)
	    LOADPROG(zz, bb),

	    LABEL lab_samedone,

	    debugsay "success.\ntesting aliasing..\n",
	    LITERAL_ADDR(ee, lab_test_alias, hh),
	    
	    (* update the 0 array. make sure the
	       update isn't reflected in the old copy *)
	    LITERAL(ff, 0wxBEEF),
	    UPD(zz, ee, ff),

	    ASUB(dd, aa, ee),
	    
	    SUB(dd, ff, dd, hh),
	    JZ(dd, lab_alias_0_to_old, bb, cc, hh),

	    (* make sure we could successfully read it. *)
	    ASUB(ee, zz, ee),
	    (* ee should have the value we wrote,
	       as should ff... *)
	    SUB(ee, ee, ff, hh),
	    JNZ(ee, lab_write0_fail, bb, cc, hh),


	    LITERAL_ADDR(ee, lab_test_alias2, hh),
	    LITERAL_ANY(ff, 0wxCA88A6E, hh),
	    (* update aa, make sure it doesn't change 0 *)
	    UPD(aa, ee, ff),
	    ASUB(dd, zz, ee),
	    SUB(dd, ff, dd, hh),
	    JZ(dd, lab_alias_old_to_0, bb, cc, hh),

	    (* make sure we can read that, too *)
	    ASUB(ee, aa, ee),
	    SUB(ee, ee, ff, hh),
	    JNZ(ee, lab_writea_fail, bb, cc, hh),

	    (* finally, free the array *)
	    debugsay "success.\nfree after loadprog..\n",
	    FREE aa,


	    (* then do a series of
	       allocs and frees to try
	       to make the consequences
	       of trashed memory happen
	       sooner *)

	    MANY (List.concat (map (fn r =>
				    [LITERAL(r, 0w7), ALLOC(r, r)]) 
			       [aa, bb, cc, dd, ee, ff, hh])),

	    MANY (map FREE [aa, bb, cc, dd, ee, ff, hh]),

	    debugsay "success.\n",
	    
	    debugsay "loadprog ok.\n",
	    (* run the actual wrapped code *)
	    MANY (map (fn r => (LITERAL(r, 0w0))) [aa, bb, cc, dd, ff, zz, hh]),
	    LITERAL_ADDR(ee, main, hh),
	    LOADPROG(zz, ee)]) ::

       (lab_test_alias, [DATA 0wx0DEAFDAD]) ::

       (lab_test_alias2, [DATA 0wxB0BAFE77]) ::

       (lab_write0_fail,
	emitstring [aa,bb,cc,dd,ee,ff,hh] "loadprog: write 0 array fail" @ [HALT]) ::

       (lab_writea_fail,
	emitstring [aa,bb,cc,dd,ee,ff,hh] "loadprog: write old array fail" @ [HALT]) ::

       (lab_alias_0_to_old,
	emitstring [aa,bb,cc,dd,ee,ff,hh] "loadprog: array not copied (1)" @ [HALT]) ::

       (lab_alias_old_to_0,
	emitstring [aa,bb,cc,dd,ee,ff,hh] "loadprog: array not copied (2)" @ [HALT]) ::

       (lab_not_equal_after,
	emitstring [aa,bb,cc,dd,ee,ff,hh] "loadprog copy fail" @ [HALT]) ::

       blocks)
    end

  (* will stick this at the head *)
  val lab_new0 = V.namedvar "new0"
  val new0code =
    emitstring [aa, bb, cc, dd, ee, ff, hh] "loadprog didn't replace 0 array" @ [HALT]

  val new0codelen = length new0code

  (* MUST be shorter than the self check code already at 0, or we could screw ourselves *)
  val () = if new0codelen >= length SelfCheck.selfcheck0
	   then raise SCLoadprog "whoops, new0code is too long!"
	   else ()

  val rewrite0done = V.namedvar "rw0done"  
  val rewrite0top  = V.namedvar "rw0top"

  fun sc_code proglen =
    [(lab_selfcheck,
      [
      
       LITERAL(zz, 0w0),

       (* first thing to do is to overwrite the
	  beginning of the current 0 array with 
	  code that tells the user that he hasn't
	  actually jumped to the new array. *)

       LITERAL_ANY(cc, W.fromInt new0codelen, hh),

       LITERAL_ADDR(dd, lab_new0, hh),

       (* dd : source base
	  cc : how many words left to copy
	       (also destination offset+1) *)
       LABEL rewrite0top,
       JZ(cc, rewrite0done, ee, ff, hh),

       DEC(cc, hh),

       (* now it is a proper offset *)
       ADD(aa, dd, cc),
       ASUB(aa, zz, aa),
       UPD(zz, cc, aa),

       LITERAL_ADDR(ff, rewrite0top, hh),
       LOADPROG(zz, ff),
       LABEL rewrite0done,

       (* start by allocating the destination. *)

       LITERAL_ANY(cc, W.fromInt proglen, hh),
       ALLOC(aa, cc),
       

       LITERAL_ADDR(ff, lab_program, hh),

       (* cc : number of words left to copy.
	  aa : destination array,
	  ff : source base
	  dd : label for top
	  *)
       LITERAL_ADDR(dd, lab_copytop, hh),

       LABEL(lab_copytop),
       JZ(cc, lab_copydone, bb, ee, hh),
       
       DEC (cc, hh),
       
       ADD(ee, cc, ff),
       ASUB(ee, zz, ee),
       UPD(aa, cc, ee),

       (* unconditional jump to top. *)
       LOADPROG(zz, dd),

       LABEL lab_copydone,
       (* well, just jump to it. *)

       (* need to save length for post-loadprog check too *)
       LITERAL_ANY(cc, W.fromInt proglen, hh),
       debugsay "about to load program from some allocated array..\n",
       LOADPROG(aa, zz)
       ]
      ),
    (lab_new0,
     new0code)
    ]


  (* takes a stream of bytes and produces a list of DATA words
     so that we can embed the inner program inside this one. *)
  fun programblock prog =
    let
      fun to32 w8 = W.fromInt (Word8.toInt w8)

      fun progword () =
          case (prog (), prog (), prog (), prog ()) of
              (SOME a, SOME b, SOME c, SOME d) =>
                  SOME
                  (W.orb
                   (W.<<(to32 a, 0w24),
                    W.orb
                    (W.<<(to32 b, 0w16),
                     W.orb
                     (W.<<(to32 c, 0w8), to32 d))))
                      
            | (NONE,   NONE,   NONE,   NONE) => NONE
            | _ => raise SCLoadprog "byte stream not multiple of four"

      val progblock = SimpleStream.tolist (SimpleStream.map DATA progword)

    in
        progblock
    end

  (* NOTE: we ignore dynlab, because we don't support
     reading or writing the dynamic region (this part
     comes BEFORE we do any initialization of it from
     the teamstuff)  *)
  fun self_check ((prog, labmap), dynlab) =
    let

      val progblock = programblock prog
      val proglen = length progblock

    in
      (sc_code proglen @
       [(lab_program, progblock)],
       lab_selfcheck)
    end

end
