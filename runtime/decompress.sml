
(* code for self-decompression *)

structure Decompress =
struct

  val showprogress = Params.flag true
    (SOME ("-sdprog",
           "Show progress in self-decompression")) "sdprog"

  exception Decompress of string
  structure V = Variable
  open Conventions
  open UMA

  (* header:
     [number of compressed codes]
     [decompressed size (words)]
     [maximum table size (words)]
     ... data ...
     *)
  val OFFSET_NUMCODES  = 0w0 : W.word
  val OFFSET_DECOMP    = 0w1 : W.word
  val OFFSET_MAXTAB    = 0w2 : W.word
  val OFFSET_DATASTART = 0w3 : W.word

  fun debug l = MANY l

  (* to turn off debugging: *)
  fun debug l = MANY []

  val lab_dynamic = V.namedvar "decompress_dynamicregion"

  val lab_decompress = V.namedvar "decompress"

  (* shift table *)
  val lab_pow2_table = V.namedvar "pow2"

  val pow2tab =
    (lab_pow2_table,
     List.tabulate
     (32,
      fn i => DATA (W.<<(0w1, Word.fromInt i))))

  (* given a label that points to compressed
     code (in register aa), this code will
     decompress the block into a newly
     allocated array and return its
     identifier in a (by jumping to the
     address in bb). *)
  fun dc_code { startbits } =
    let
      (* these are offsets of local variables holding
         array identifiers. *)
      val var_ptra   = V.namedvar "decompress.ptra"
      val var_lena   = V.namedvar "decompress.lena"
      val var_dest   = V.namedvar "decompress.dest"
      val var_src    = V.namedvar "decompress.src"

      val var_return = V.namedvar "decompress.return"

      (* size of code tables, output respectively *)
      val var_sz     = V.namedvar "decompress.sz"
      val var_os     = V.namedvar "decompress.os"

      val lab_getcode = V.namedvar "decompress_getcode"
      val lab_getcode_readbits = V.namedvar "decompress_getcode_readbits"
      val lab_getcode_readabit = V.namedvar "decompress_getcode_readabit"
      val lab_getcode_done = V.namedvar "decompress_getcode_done"
      val var_getcode_bits = V.namedvar "decompress_getcode.bits"
      val var_getcode_idx = V.namedvar "decompress_getcode.idx"
      val var_getcode_codesleft = V.namedvar "decompress_getcode.codesleft"
      val var_getcode_bitsleft = V.namedvar "decompress_getcode.bitsleft"
      val var_getcode_return = V.namedvar "decompress_getcode.return"

      val lab_decode  = V.namedvar "decompress_decode"
      val var_decode_oldstart = V.namedvar "decompress_decode.oldstart"
      val var_decode_oldlen = V.namedvar "decompress_decode.oldlen"
      val lab_emit    = V.namedvar "decompress_emit"
      val var_emit_what = V.namedvar "decompress_emit.what"
      val var_emit_nbytes = V.namedvar "decompress_emit.nbytes"

      val lab_add = V.namedvar "decompress_add"
      val var_add_pos = V.namedvar "decompress_add_pos"
      val var_add_len = V.namedvar "decompress_add_len"

      val lab_decode_many = V.namedvar "decompress_decode_many"
      val lab_decode_special = V.namedvar "decompress_decode_special"
      val var_decode_thislen = V.namedvar "decompress_decode.thislen"

      val lab_decodemany_tail = V.namedvar "decompress_decode_many_tail"

      val lab_getbyte = V.namedvar "decompress_getbyte"

      val lab_emitmany = V.namedvar "decompress_emitmany"
      val var_emitmany_start = V.namedvar "decompress_emitmany_start"
      val var_emitmany_len = V.namedvar "decompress_emitmany_len"

      val lab_done = V.namedvar "decompress_done"
      val lab_morebits = V.namedvar "decompress_morebits"

      val lab_progress_then_getcode = V.namedvar "decompress_prog"
      val var_prog_freq = V.namedvar "decompress_prog.freq"
      val var_prog_numleft = V.namedvar "decompress_prog.numleft"
      val var_prog_digit = V.namedvar "decompress_prog.digit"
        
      val lab_emitmany_bottom = V.namedvar "decompress_emitmany_bottom"

      val lab_byteshifts = V.namedvar "decompress.byteshifts"
        


      (* trashes ff, hh *)
      fun setvar var reg =
        MANY[LITERAL_ADDR(hh, var, ff),
             UPD(gg, hh, reg)]

      (* reg can be ff safely *)
      fun getvar reg var =
        MANY[LITERAL_ADDR(hh, var, ff),
             ASUB(reg, gg, hh)]

    in
      (* vars *)
      (lab_byteshifts, 
       [DATA 0w1,
        DATA 0w256,
        DATA (0w256 * 0w256),
        DATA (0w256 * 0w256 * 0w256)]) ::

      (map (fn l => (l, [DATA 0w0]))

       
      (
       (if !showprogress
        then [(* lab_progress_then_getcode, *)
              var_prog_freq, var_prog_numleft, var_prog_digit]
        else []) @
      [var_ptra,
       var_lena,
       var_dest,
       var_src,
       var_return,
       var_sz,
       var_os,
       var_getcode_bits,
       var_getcode_idx,
       var_getcode_codesleft,
       var_getcode_bitsleft,
       var_getcode_return,
       var_decode_oldstart,
       var_decode_oldlen,
       var_emit_what,
       var_emit_nbytes,
       var_emitmany_start,
       var_emitmany_len,
       var_decode_thislen,
       var_add_pos,
       var_add_len])) @

      (lab_dynamic,
       List.tabulate(Runtime.DYNAMIC_WORDS,
                     fn _ => DATA 0w0)) ::

      [(lab_decompress,
        [
         (* to perform decompression, we must maintain
            a table of the strings we've produced so
            far. 
            
            this is represented as two parallel arrays
            LEN and PTR, which hold the length of the
            string, and the index to the start of the
            string (in the output array). Each quantity 
            is measured in bytes. The code 'c' is stored
            at LEN[c-255] and PTR[c-255], unless c is
            less than 256. In this case, the string
            denoted by c is of length one, and consists 
            of the character c.

            the compression phase knows the maximum size
            of these tables, and so we read this from
            the compressed data's header (see above).

            *)

         (* first save return address. we need all the
            regs we can get... *)
         setvar var_return bb,
         (* save the start of the source as well. *)
         setvar var_src aa,

         (* set getcode.idx to the first word of the
            compressed data *)
         LITERAL (cc, OFFSET_DATASTART),
         ADD(bb, aa, cc),
         setvar var_getcode_idx bb,

         (* start with 32 bits left in first word *)
         LITERAL(cc, 0w32),
         setvar var_getcode_bitsleft cc,

         (* initialize number of codes left *)
         LITERAL (cc, OFFSET_NUMCODES),
         ADD(bb, aa, cc),
         ASUB(bb, zz, bb),
         setvar var_getcode_codesleft bb,

         (* set up progress indicator *)
         MANY (if !showprogress
               then [LITERAL(hh, 0w10),
                     (* every 'bb' codes, print digit *)
                     DIV(bb, bb, hh),
                     setvar var_prog_freq bb,
                     setvar var_prog_numleft bb,
                     LITERAL(bb, W.fromInt (ord #"9")),
                     setvar var_prog_digit bb] @

                     (* ENH: use param *)
                     emitstring [bb, cc, dd, ff, hh] "LOADING: "
               else []),

         (* set lab_dest to a new empty array of
            the appropriate size. *)
         (* aa : address of input in 0 array *)         
         LITERAL(cc, OFFSET_DECOMP),
         ADD(bb, aa, cc),
         ASUB(bb, zz, bb),
         (* bb : size of decompressed data (words) *)
         ALLOC(bb, bb),
         (* bb : decompressed data destination *)
         setvar var_dest bb, 

         (* allocate an initialize the ptr
            and len tables. *)
         LITERAL(cc, OFFSET_MAXTAB),
         ADD(bb, aa, cc),
         ASUB(bb, zz, bb),
         (* bb : size of max table (words) *)
         ALLOC(cc, bb),
         ALLOC(dd, bb),
         (* cc : PTR, dd : LEN *)
         setvar var_ptra cc,
         setvar var_lena dd,


         (* zero out sizes *)
         setvar var_sz zz,
         setvar var_os zz,
         (* how many bytes have been written yet *)
         setvar var_emit_nbytes zz,

         (* okay, set up invariants and start decoding!! *)
         
         (* length of first code determined by startbits. *)
         LITERAL(aa, W.fromInt startbits),
         setvar var_getcode_bits aa,

         CALLWITH(aa, lab_getcode, ff, hh),

         (* bb : first code *)

         (* NOTE: assuming that the first one will not be
            a morebits command
            (but do need to decrement it) *)
         DEC(bb, hh),

         (* emit the single character that is
            the code. *)
         setvar var_emit_what bb,
         CALLWITH(aa, lab_emit, ff, hh),

         LITERAL(aa, 0w1),
         setvar var_decode_oldlen aa,
         setvar var_decode_oldstart zz,
         
         (* start loop *)
         LITERAL_ADDR(aa, lab_decode, hh),
         LOADPROG(zz, aa)
         ]),

      (lab_morebits,
       [
        debug (emitstring [aa] "++"),

        getvar aa var_getcode_bits,
        LITERAL(bb, 0w1),
        ADD(aa, bb, aa),
        setvar var_getcode_bits aa,
        LITERAL_ADDR(aa, lab_decode, hh),
        LOADPROG(zz, aa)
        ]),
      
       (* decode loop. *)
       (lab_decode,
        [
         (* this is set in morebits above
            as requested by the input stream *)
         (* setvar var_getcode_bits aa, *)
         CALLWITH(aa, (if !showprogress
                       then lab_progress_then_getcode
                       else lab_getcode), ff, hh),

         (*
         LITERAL(ff, W.fromInt (ord #"-")),
         WRITE ff,
         *)

         (* we use ZERO to stand for the next-codebits-please
            marker *)
         JZ(bb, lab_morebits, ee, ff, hh),
        
         (* but rest of the code assumes 0 offset *)
         DEC(bb, hh),

         (* bb : code *)
         
         (* get the string corresponding to bb. 

            if bb is 0..255 then the string is
            that byte. otherwise we look in the
            table. *)
         LITERAL(aa, 0w255),
         AND(aa, bb, aa),
         SUB(aa, bb, aa, hh),
         (* now aa = 0 iff b <= 255. *)
         JNZ(aa, lab_decode_many, ee, ff, hh),
         
         (*
         LITERAL(ff, W.fromInt (ord #"@")),
         WRITE ff,
         *)

         (* bb : code, still.
            emit it. *)
         setvar var_emit_what bb,
         CALLWITH(aa, lab_emit, ff, hh),
         
         (* add last string emitted+first char
            of new string (= this char).
            intialize those args: *)

         (* add.len = oldlen + 1 *)
         getvar dd var_decode_oldlen,
         LITERAL(cc, 0w1),
         ADD(bb, dd, cc),
         setvar var_add_len bb,

         (* add.pos = oldstart *)
         getvar aa var_decode_oldstart,
         setvar var_add_pos aa,

         (* aa : oldstart
            dd : oldlen
            cc : 1
            when calling decode,
            dec.oldstart = oldstart + oldlen
            dec.oldlen = 1. we have these values
            around, so set up now *)

         ADD(aa, aa, dd),
         setvar var_decode_oldstart aa, 
         setvar var_decode_oldlen cc, (* length : 1 *)

         CALLWITH(aa, lab_add, ff, hh),
         
         (* added to table. now we just
            loop, the args to decode having
            already been initialized. *)
         LITERAL_ADDR(aa, lab_decode, hh),
         LOADPROG(zz, aa)]
         ),

       (lab_decode_many,
        (* PERF could implement "special" case *)
        (* code in bb *)
        [LITERAL(aa, 0w256),
         getvar cc var_sz,
         ADD(cc, cc, aa),
         SUB(cc, bb, cc, ff),
         JZ(cc, lab_decode_special, dd, ee, ff),

         (*  in ML:

            let val sl =
            let val (pos, len) = Array.sub(!d, n - C.radix)
             in
               AS.slice (!out, pos, SOME len)
             end
             
            AS.app emit sl
            
            (* add the last string we
               emitted previously, 
               plus the first char of 
               the new string *)
            add (oldstart, oldlen + 1);
            dec (oldstart + oldlen) (AS.length sl)

             *)

         (* now set up for (emission) loop. *)
         LITERAL(aa, 0w256),
         SUB(bb, bb, aa, ff),

         (* offset in bb *)
         getvar ff var_ptra,
         ASUB(cc, ff, bb),
         getvar ff var_lena,
         ASUB(dd, ff, bb),
         (* cc : ptr
            dd : len *)

         debug
         [LITERAL(aa, W.fromInt (ord #"[")),
          WRITE aa],

         setvar var_emitmany_start cc,
         setvar var_emitmany_len   dd,

         setvar var_decode_thislen dd,

         debug
         [LITERAL(hh, W.fromInt (ord #"{")),
         WRITE hh,

         LITERAL(hh, W.fromInt (ord #"0")),
         ADD(hh, hh, cc),
         WRITE hh,

         LITERAL(hh, W.fromInt (ord #"-")),
         WRITE hh,

         LITERAL(hh, W.fromInt (ord #"0")),
         ADD(hh, hh, dd),
         WRITE hh,
         
         LITERAL(hh, W.fromInt (ord #"}")),
         WRITE hh],


         LITERAL_ADDR(aa, lab_emitmany, hh),
         LOADPROG(zz, aa)]),

        (lab_decodemany_tail,
         [(* add (oldstart, oldlen + 1). *)

          debug [LITERAL(cc, W.fromInt (ord #"]")),
                 WRITE cc],

         getvar cc var_decode_oldlen,
         LITERAL(ff, 0w1),
         ADD(cc, cc, ff),
         setvar var_add_len cc,
         getvar cc var_decode_oldstart,
         setvar var_add_pos cc,
         CALLWITH(aa, lab_add, ff, hh),

         (* dec (oldstart + oldlen) (AS.length sl) *)
         getvar aa var_decode_oldstart,
         getvar bb var_decode_oldlen,
         ADD(aa, bb, aa),
         setvar var_decode_oldstart aa,

         getvar aa var_decode_thislen,
         setvar var_decode_oldlen aa,

         LITERAL_ADDR(aa, lab_decode, hh),
         LOADPROG(zz, aa)]),

       (lab_emitmany,
        (* emit from emitmany_start
           for emitmany_len bytes *)
        [
         (* no more? *)
         getvar bb var_emitmany_len,
         JZ(bb, lab_decodemany_tail, dd, ee, ff),

         (* decrement len. *)
         DEC(bb, aa),
         setvar var_emitmany_len bb,

         (* then increment start *)
         getvar dd var_emitmany_start,
         LITERAL(aa, 0w1),
         ADD(cc, aa, dd),
         setvar var_emitmany_start cc,

         (* CALLWITH(aa, lab_getbyte, ff, hh), *)
         (* get dd'th byte into bb *)
         LITERAL_ADDR(aa, lab_getbyte, hh),
         LOADPROG(zz, aa)]),

       (lab_emitmany_bottom,
        [
         (* then write it *)
         setvar var_emit_what bb,
         CALLWITH(aa, lab_emit, ff, hh),


         LITERAL_ADDR(aa, lab_emitmany, hh),
         LOADPROG(zz, aa)]),

       (lab_getbyte,
        (* read the dd'th byte from the output
           array into bb, and return to emitmany_bottom *)

        (* one complication here:
           the way we emit bytes means that
           the bytes in the last word might
           not be in the correct position. So 
           we treat the case that we're 
           looking at the last word specially. *)

        (* compute upshift-first, which slides
           the target word up to put every byte
           in the expected position, even if
           is incomplete. *)

       
        [
         debug
         [LITERAL(hh, W.fromInt (ord #"@")),
          WRITE hh,
          
          LITERAL(hh, W.fromInt (ord #"0")),
          ADD(hh, hh, dd),
          WRITE hh],

         getvar ee var_emit_nbytes, (* number of bytes emitted--
                                       must be at least 1 *)
        
        LITERAL(cc, 0w3),
        AND(bb, ee, cc),

        (* bb : nbytes mod 4 *)
        LITERAL(cc, 0w4),
        SUB(bb, cc, bb, hh),
        (* bb : 4 - (nbytes mod 4)
           = upshift for last word. *)

        (* so, are we looking at the last word? *)
        (* ee : nbytes *)
        DIV(ee, ee, cc),
        (* ee : current word.
           nb, I morally should DEC here because we
           might have advanced to the next word, but
           in that case the upshift would be 0 anyway,
           so I just save the instruction. *)
        DIV(aa, dd, cc),

        SUB(ff, ee, aa, hh),
        (* ff : ZERO if looking at last word *)
        (* bb : upshift we should apply *)

        (* overwrite the upshift by zero (normal case)
           unless ff is zero (looking at last word). *)
        CMOV(bb, zz, ff),

        (* phew.
           now bb : upshift amount
               dd : target byte
               aa : target word (dd/4) *)

        getvar ee var_dest,
        ASUB(aa, ee, aa),
        (* aa : actual word *)
        (* which byte is it? *)

        LITERAL(cc, 0w3),
        AND(dd, dd, cc),
        (* dd : bytenum mod 4 *)
        (* LITERAL(cc, 0w3), *)
        SUB(dd, cc, dd, hh),
        (* dd : 3 - (bytenum mod 4) 
              = downshift *)
        
        (* subtract upshift from downshift *)
        SUB(dd, dd, bb, hh),

        debug [LITERAL(hh, W.fromInt (ord #"^")),
               WRITE hh,
               LITERAL(hh, W.fromInt (ord #"0")),
               ADD(hh, hh, dd),
               WRITE hh],

        LITERAL_ADDR(bb, lab_byteshifts, hh),
        ADD(bb, bb, dd),
        ASUB(dd, zz, bb),

        (* dd = shifter for aa *)
        DIV(aa, aa, dd),
        LITERAL(cc, 0w255),
        AND(bb, cc, aa),

        (* done! ahh *)
        
        LITERAL_ADDR(aa, lab_emitmany_bottom, hh),
        LOADPROG(zz, aa)]),

       (* ENH: get rid of this or implement it *)
       (lab_decode_special,
         errormsg "ERROR: compressed data are corrupt?\n" @ [HALT]),

       (lab_add,
        [(* return address in aa *)
         (* just add pos and len at position
            var_sz and increment *)
         
         getvar bb var_sz,

         getvar cc var_add_len,
         getvar ff var_lena,
         UPD(ff, bb, cc),

         getvar dd var_add_pos,
         getvar ff var_ptra,
         UPD(ff, bb, dd),

         debug
         [LITERAL(hh, W.fromInt (ord #"(")),
          WRITE hh,
          
          LITERAL(hh, W.fromInt (ord #"0")),
          ADD(hh, hh, dd),
          WRITE hh,
          
          LITERAL(hh, W.fromInt (ord #",")),
          WRITE hh,
          
          LITERAL(hh, W.fromInt (ord #"0")),
          ADD(hh, hh, cc),
          WRITE hh,
          
          LITERAL(hh, W.fromInt (ord #")")),
          WRITE hh],


         LITERAL(cc, 0w1),
         ADD(bb, bb, cc),
         setvar var_sz bb,
         
         (* return *)
         LOADPROG(zz, aa)
         ]),

       (lab_emit,
        (* emit takes an argument byte (emit_what)
           and writes it to the next spot in the
           output array. *)
        [
         (* aa holds return address; we never touch it *)
         
         (* PERF, could save var_emit_nbytes label here *)
         getvar ee var_emit_nbytes,
         LITERAL(bb, 0w4),
         DIV(bb, ee, bb),
         (* bb : target word in dest array *)

         (* now increment the dest byte,
            since we don't read that again
            and we have the old value handy *)
         LITERAL(dd, 0w1),
         ADD(ee, ee, dd),
         setvar var_emit_nbytes ee,

         getvar ee var_dest,
         (* ee : destination array *)
         ASUB(cc, ee, bb),
         (* cc : word that's already there.
            shift it up by one byte *)
         LITERAL(dd, 0w256),
         MUL(cc, cc, dd),
         (* cc : word, shifted *)
         getvar dd var_emit_what,

         debug [WRITE dd],

         ADD(cc, cc, dd),
         (* cc : new word *)
         UPD(ee, bb, cc),

         (* getvar aa var_emit_return, *)
         LOADPROG(zz, aa)
         ]),

       (lab_done,
        (* nothing to do; just return 
           decompressed code in aa *)
        [(* MANY (errormsg "done\n"), *)
         MANY (if !showprogress
               then [LITERAL(hh, W.fromInt (ord #"\n")),
                     WRITE hh]
               else nil),
         getvar aa var_dest,
         getvar cc var_return,
         LOADPROG(zz, cc)])]

      @
      (* return address in aa.
         jump to lab_getcode when done,
         preserving aa *)
      (if !showprogress
       then [(lab_progress_then_getcode,

             [getvar bb var_prog_numleft,
              DEC (bb, hh),
              setvar var_prog_numleft bb,
              JNZ (bb, lab_getcode, cc, ff, hh),
              
              (* if it hit zero, then... *)
              (* reset *)
              getvar bb var_prog_freq,
              setvar var_prog_numleft bb,
              
              getvar bb var_prog_digit,
              WRITE bb,
              DEC (bb, hh),
              setvar var_prog_digit bb,
              
              (* now continue with getcode *)
              LITERAL_ADDR(bb, lab_getcode, hh),
              LOADPROG(zz, bb)
              ]
             )]
       else [])
       @

       [(lab_getcode,
        [
         (* return address in aa.
            put result in bb.
            getcode_bits stores number of bits to read *)

         (* save return address; we need regs *)
         setvar var_getcode_return aa,

         (* check if there are codes left. *)
         getvar cc var_getcode_codesleft,
         (* no more codes? then done *)
         JZ(cc, lab_done, dd, ee, ff),
         (* otherwise decrement *)
         NAND(dd, zz, zz), (* -1 *)
         ADD(cc, dd, cc),
         setvar var_getcode_codesleft cc,

         (*
         LITERAL(bb, W.fromInt (ord #"\n")),
         WRITE bb,
         *)

         LITERAL(bb, 0w0),
         getvar dd var_getcode_bits,

         LITERAL_ADDR(aa, lab_getcode_readbits, hh),
         LOADPROG(zz, aa)
         ]),

        (lab_getcode_readbits,

         (* 
            bb : code so far
            dd : number of bits left to read for 
                 this code. (must be > 0)
            *)
         [

          (* how many bits left in getcode_idx? *)
          getvar cc var_getcode_bitsleft,

          (* we know these are small positive 
             quantities, so we can just do
             AVAIL - NEED and check the sign bit. 
             
             cc : available bits
             dd : needed bits

             are there more bits than we need? *)
          SUB(aa, cc, dd, hh),
          LITERAL_ANY(ee, Conventions.SIGN_BIT, hh),
          NAND(ee, aa, ee),

          (* cc : bits available
             dd : bits needed
             bb : bits so far
             if ee is nonzero, then avail > need
             so let aa = min(need=dd, avail=cc)
             *)
          CMOV(aa, dd, ee),
          NAND(ee, ee, ee),
          CMOV(aa, cc, ee),

          (* number of bits left after we take these. *)
          SUB(cc, cc, aa, hh),
          (* if we ran out of bits, then there will
             be 32 next time (we update the index
             later) *)
          LITERAL(hh, 0w32),
          CMOV(cc, hh, ee),
          setvar var_getcode_bitsleft cc,

          SUB(dd, dd, aa, hh),
          (* aa : number we'll take.
             ee : nonzero if we've used them all up
             dd : bits we'll need after this read
             bb : code so far 

             so now read aa bits.
             *)

          (* read current value *)
          getvar cc var_getcode_idx,
          ASUB(cc, zz, cc),

          (* cc : word we read; low-order
             bits are our target *)

          (* shift bb up *)
          LITERAL_ADDR(ff, lab_pow2_table, hh),
          ADD(ff, aa, ff),
          ASUB(ff, zz, ff),
          (* ff : shift factor *)
          (* shift bb left *)
          MUL(bb, bb, ff),
          (* shift over source *)
          DIV(aa, cc, ff),

          (* aa : remaining bits in source *)

          (* subtract 1 from ff, giving mask *)
          NAND(hh, zz, zz),
          (* hh : ~1 *)
          ADD(ff, hh, ff),
          (* ff : mask *)
          AND(hh, ff, cc),
          (* hh : masked bits *)
          ADD(bb, bb, hh),

          (* write back *)
          (* (nb. getvar implicitly trashes ff, but it can be dest) *)
          getvar ff var_getcode_idx,
          UPD(zz, ff, aa),
          
          (* bb : bits so far
             cc : unshifted, unmasked src bits
             dd : bits we still need
             ee : nonzero if we've used up this word

             get mask... *)

          (* assuming var_getcode_idx is not zero *)
          (* CMOV(aa, ff, ff), *)

          LITERAL(aa, 0w0),
          LITERAL(hh, 0w1),
          CMOV(aa, hh, ee),
          (* now aa is 1 if we used up this word *)
          ADD(ff, aa, ff),
          
          (* increment the getcode index if we used
             up all bits. *)
          (* setvar, using temporaries aa and hh
             since we want to set to ff *)
          LITERAL_ADDR(aa, var_getcode_idx, hh),
          UPD(zz, aa, ff),

          (* if we have no bits left to read,
             then done; else loop *)
          JNZ(dd, lab_getcode_readbits, ff, hh, aa),

          getvar aa var_getcode_return,
          (* bb : next code, aa : return *)
          LOADPROG(zz, aa)])
       ]
    end

  structure LZW = 
  LZWFn(structure C = 
        struct 
          type ch = Word8.word
          val compare = Word8.compare
          val itoc = Word8.fromInt
          val ctoi = Word8.toInt
          val radix = 256
        end 
        val allow_special = false
        val tablesize = NONE)

  (* convert a word to a list of bools, using n bits *)
  fun wtol n w =
    let
      fun gbn b = 
        if b = n then nil
        else (0w0 <> (W.andb(w, W.<<(0w1, Word.fromInt b)))) :: gbn (b + 1)

      val l = gbn 0
    in
      rev l
    end

  (* make a list of bits (from least to most significant) into a 32-bit word *)
  fun ltow nil = 0w0 : Word32.word
    | ltow (h :: t) = Word32.orb(Word32.<<(ltow t, 0w1), 
                                 if h then 0w1 else 0w0)

  fun compress prog =
    let

        val STARTBITS = 9

      (* count length of input (bytes) *)
      val insize = ref 0
      val prog = SimpleStream.map (fn w => (insize := !insize + 1; w)) prog

      val prog = SimpleStream.map
                   (fn (LZW.CODE (c, b)) => (b, W.fromInt c)
                     | _ => raise Decompress "impossible: table reset from lzw")
                   (LZW.compress prog)

      (* val prog = SimpleStream.tovector prog *)

      (* compute max bits needed and save it; each code is represented with
         this many bits. PERF: we should keep the compressor and
         decompressor in sync, since lots of bits are wasted doing it
         this way! 

         (now synchronizing them with code 0; should rewrite this comment -tom)
         *)
      val maxbits = ref 0
      val curbits = ref STARTBITS
      val prog = SimpleStream.map (fn (r as (b, w)) => 
                                   SimpleStream.fromlist
                                   (if b > !maxbits then maxbits := b else ();

                                    (* use at least curbits *)

                                    if b < !curbits then [(!curbits, SOME w)]
                                    else
                                    (* but if we now need more, tell the decoder.. *)
                                    if b > !curbits then 
                                        [(!curbits, NONE),
                                         (curbits := b;
                                          (b, SOME w))]
                                    else [(b, SOME w)])) prog

(*
      val progv = SimpleStream.tovector (SimpleStream.flatten prog) 

      val () = print ("  PERF: Using " ^ Int.toString (!maxbits) ^ 
                      " bits for EVERY code!\n")

      val prog = SimpleStream.map (fn (_, c) => (!maxbits, c))
        (SimpleStream.fromvector progv)
*)
      val prog = SimpleStream.flatten prog


      val ncodes = ref 0

      local
        (* left-over word, and number of bits left over. *)
        val leftover = ref 0w0 : W.word ref
        (* must be < 32 *)
        val nleftover = ref 0
      in
        (* emit one 32-bit word *)
        fun data () =
          (* first put all the leftover bits at the bottom. *)
          let

            val nlo = !nleftover
            val lo = !leftover

                              (* exact hit *)
            fun packbits 0 w = SOME (DATA w)
              | packbits remain w =
              (case prog () of
                 NONE => (* out of codes *)
                   (* SUSP: does the reader try to read
                      one word past the end sometimes?
                      (and then 0 bits from it?) *)
                   if remain = 32 then NONE
                   else SOME (DATA w)
               | SOME (bits : int, wco : W.word option) => 
                   let 
                       (* code is incremented by one, so that
                          we can use 0 as a synchronization
                          token (meaning bits++) *)
                       val wc = 
                           case wco of
                               NONE => 0w0
                             | SOME w => 0w1 + w
                   in
                     ncodes := !ncodes + 1;
                     (* print ("Emit: " ^ 
                               Int.toString (W.toInt wc) ^ "\n"); *)
                     if bits <= remain
                     then packbits (remain - bits) 
                       (* shift into the next position
                          to the left of the data in w *)
                       (W.orb(w, W.<<(wc, 
                                      0w32 - Word.fromInt remain)))
                     else (* chop it *)
                       let
                         (* put the high part
                            (number of bits: remain)
                            in this word. *)
                         val lowbits = bits - remain
                         val lowpart = W.andb(wc,
                                              W.<<(0w1, 
                                                   Word.fromInt lowbits) 
                                              - 0w1)
                         val highbits = remain
                         val highpart = W.>>(wc, Word.fromInt lowbits)
                       in
                         nleftover := lowbits;
                         leftover := lowpart;

                         packbits 0 (W.orb(w, W.<<(highpart, 
                                                   0w32 - 
                                                   Word.fromInt highbits)))
                       end
                   end)
          in
            nleftover := 0;
            leftover := 0w0;
            (* start with leftover bits at the bottom *)
            packbits (32 - nlo) lo
          end
      end

      (* forces streams, side-effecting counters *)
      val data = SimpleStream.tolist data

      val num_codes = !ncodes
      val () = print ("(ncodes: " ^ Int.toString num_codes ^ ")\n")
      val size_decompressed = (if (!insize) mod 4 <> 0 
                               then 1 before 
                                 print " !! WARNING: input not 4*k !!\n"
                               else 0) + (!insize) div 4

      (* can't be more entries in the table than codes *)
      val max_table = num_codes + 2

      val size_compressed = length data

      (* test decompression! *)
          (*
      val () = print "Test decompress:\n"
      val _ = LZW.decompress (SimpleStream.map (fn (_, w) => W.toInt w)
                              (SimpleStream.fromvector progv))
      *)

    in
      print ("  Before compression: " ^ Int.toString (!insize) ^ " bytes\n" ^
             "  After compression:  " ^ Int.toString (size_compressed * 4) ^ " bytes (" ^
             (Real.fmt (StringCvt.FIX (SOME 1)) ((100.0 * real (size_compressed * 4)) /
                                                          real (!insize))) ^ "%)\n");
      ([DATA (W.fromInt num_codes),
        DATA (W.fromInt size_decompressed),
        DATA (W.fromInt max_table)] @
       data, STARTBITS)
    end

  (* Generate a program suitable for assembly that
     does self decompression. The program data are
     given as a byte stream *)
  fun self_decompress ((prog, labmap), dynlab) =
    let
      val lab_main = V.namedvar "self_decompress_main"
      val lab_compressed = V.namedvar "compressed_data"

      (* when done, jumps to the decompressed code *)
      val lab_trampoline = V.namedvar "self_decompress_trampoline"

      val (cdata, startbits) = compress prog
        handle LZW.LZW s => raise Decompress ("\nLZW: " ^ s ^ "\n")

      val dynoff = 
        (case V.Map.find (labmap, dynlab) of
           NONE => 
             raise Decompress ("can't wrap with " ^
                               "self-decompress because can't " ^
                               "find dynamic region " ^
                               V.tostring dynlab)
         | SOME off => W.fromInt off)

      (* copy from lsrc+off for n bytes to aa[adst] *)
      fun copylab (lsrc, _)   0 adst = nil
        | copylab (lsrc, off) n adst =
        LITERAL_ADDR(bb, lsrc, hh) ::
        LITERAL(ff, W.fromInt off) ::
        ADD(bb, bb, ff) ::
        ASUB(bb, zz, bb) ::
        LITERAL_ANY(ee, adst, hh) ::
        UPD(aa, ee, bb) ::
        copylab (lsrc, off + 1) (n - 1) (adst + 0w1)
    in
      (* just load the input into aa,
         the return address in bb,
         then branch to decompression code. *)
      ((lab_main,
        [LITERAL_ADDR(aa, lab_compressed, hh),
         LITERAL_ADDR(bb, lab_trampoline, hh),
         LITERAL_ADDR(cc, lab_decompress, hh),
         LOADPROG(zz, cc)])
       ::
       pow2tab ::
       dc_code { startbits = startbits } @
       [(lab_trampoline,
         (* decompressed code in aa *)
         (* emitstring [bb, cc, dd] "JUMP!\n" @ *)
         [(* BREAK, *)
          (* need to copy dynamic region 
             from lab_dynamic to aa[dynoff]
             *)
          MANY (copylab (lab_dynamic, 0) Runtime.DYNAMIC_WORDS dynoff),
          LOADPROG(aa, zz)]),
         (lab_compressed,
          cdata),
         (V.namedvar "padding",
          [DATA 0wxFFFFFFFF,
           DATA 0wx99999999])
         ],
       lab_main)
    end

end