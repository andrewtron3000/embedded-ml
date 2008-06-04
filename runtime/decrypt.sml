(* code for self-decryption 
   and team-id/secret embedding *)

(* The decryption key is 16 characters.
   This is followed by the team id,
   which is four characters. Next is
   the team secret, which is 8 characters.
   Finally, a four-character checksum
   is used to verify the teamid and
   team secret.

   The checksum CK will be represented as
   ASCII values 65(A)-122(z). We will
   never generate team id/secrets whose
   checksum includes the characters
   [\]^_`, which lie in between the
   ascii uppercase and lowercase characters.
   (So the checksum is only A-Za-z).

   The checksum value C, a number from
   0 to 11316496, is 
   
   (CK[0]-65) * (58^3) +
   (CK[1]-65) * (58^2) +
   (CK[2]-65) * (58^1) +
   (CK[3]-65) * (58^0)

   For the team info T (12 bytes), C is

   let W1 =
   T[0]  * (256^3) +
   T[1]  * (256^2) +
   T[2]  * (256^1) +
   T[3]  * (256^0) 
   and W2 =
   T[4]  * (256^3) +
   T[5]  * (256^2) +
   T[6]  * (256^1) +
   T[7]  * (256^0) 
   and W3 =
   T[8]  * (256^3) +
   T[9]  * (256^2) +
   T[10] * (256^1) +
   T[11] * (256^0) 

   C =  (((W1 * 31337 + W2) * 65521 + W3) mod 2^32)
         mod (58^4)
*)

structure Decrypt =
struct

  val showprogress = Params.flag true
    (SOME ("-decryptprog",
           "Show progress in self-decryption")) "decryptprog"

  exception Decrypt of string

  structure V = Variable
  open Conventions
  open UMA



  (* NOTE! Hard-coded password!
     It must be exactly 16 characters. *)
  val ARCFOUR_KEY = "(\\b.bb)(\\v.vv)06"
                  (* 0\1234567\89ABCDEF *)

  val () = if size ARCFOUR_KEY <> 16
           then raise Decrypt "arcfour_key must be 16 chars exactly"
           else ()

  (* SUSP: unused! if someone tries to wrap the decryptor,
     it will fail. *)
  val lab_dynamic = V.namedvar "decrypt_dynamicregion"

  (* prompts for key. *)
  val lab_prompt = V.namedvar "prompt"

  val lab_decrypt = V.namedvar "decrypt"

  (* key input by the user *)
  val lab_key = V.namedvar "key_region"

  (* crypted data input *)
  val lab_crypted = V.namedvar "crypted_data"
  val lab_nwords  = V.namedvar "crypted_nwords"

  val lab_teamstuff = V.namedvar "dc_teamstuff"

  (* rc4 state *)
  val lab_s = V.namedvar "rc4_s"
  val lab_i = V.namedvar "rc4_i"
  val lab_j = V.namedvar "rc4_j"

  val lab_decrypted = V.namedvar "decrypted"


  structure W8 = Word8
  structure W8A = Word8Array
  structure W8V = Word8Vector

  fun dc_code { firstword, dynregion } =
    let
      (* trashes ff, hh *)
      fun setvar var reg =
        MANY[LITERAL_ADDR(hh, var, ff),
             UPD(gg, hh, reg)]

      (* reg can be ff in this case *)
      fun getvar reg var =
        MANY[LITERAL_ADDR(hh, var, ff),
             ASUB(reg, gg, hh)]

      (* checksum stuff *)
      val lab_W1 = V.namedvar "dc_W1"
      val lab_W2 = V.namedvar "dc_W2"
      val lab_W3 = V.namedvar "dc_W3"

      val lab_Ccomputed = V.namedvar "dc_Ccomputed"
      val lab_Cgiven = V.namedvar "dc_Cgiven"

      val lab_onebyte = V.namedvar "rc4_onebyte"

      val lab_n = V.namedvar "dc_n"
      val lab_decrypt_done = V.namedvar "dc_done"
      val lab_decrypt_loop = V.namedvar "dc_loop"
      val lab_decrypt_keyinit = V.namedvar "dc_keyinit"

      val lab_checkteam = V.namedvar "dc_checkteam"

      val lab_wrongpass = V.namedvar "dc_wrongpass"

      val lab_saveret = V.namedvar "dc_saveret"
        
      (* reads 16 characters as words
         into the zero page at offset ff *)
      fun read16 () =
        let
          fun r 0 = nil
            | r n =
            READ aa ::
            UPD(zz, ff, aa) :: 
            ADD(ff, ff, bb) ::
            r (n - 1)
        in
          LITERAL(bb, 0w1) ::
          r 16
        end
    in
        
      [
       (lab_prompt,
        emitstring [aa, bb, cc, dd, ee, ff, hh] 
        ("self-check succeeded!\n" ^
         "enter decryption key:\n") @
        (* then read sixteen chars *)
        LITERAL_ADDR(ff, lab_key, hh) ::
        read16 () @
        LITERAL_ADDR(ff, lab_teamstuff, hh) ::
        read16 () @

        (* prepare and check teamstuff *)
        [LITERAL_ADDR(aa, lab_checkteam, hh),
         LOADPROG(zz, aa)]),

       (lab_checkteam,
        let 
          (* bb : 1,
             cc : 256, (or other multiplier)
             aa : start of data in 0 page
             if offset = true then subtract
             the value in ff from each byte.
             reads word into dd *)
          fun readword offset =
            let
              fun rd 0 = nil
                | rd n =
                (if n = 4
                 then LITERAL(dd, 0w0) (* PERF could read into dd first *)
                 else MUL(dd, dd, cc)) ::
                ASUB(ee, zz, aa) ::
                ADD(aa, aa, bb) ::
                ADD(dd, dd, ee) ::
                (if offset
                 (* PERF precompute nand *)
                 then SUB(dd, dd, ff, hh) :: rd (n - 1)
                 else rd (n - 1))
            in
              MANY (rd 4)
            end
        in
        [
         (* compute actual cksum *)
         LITERAL(bb, 0w1),
         LITERAL(cc, 0w256),
         LITERAL_ADDR(aa, lab_teamstuff, hh),
         
         (* compute W1, W2, W3 *)
         readword false,
         setvar lab_W1 dd,
         readword false,
         setvar lab_W2 dd,
         readword false,
         setvar lab_W3 dd,

         (* read the given checksum: *)  
         (* offset each by 65 = ASCII A *)
         LITERAL(ff, 0w65),
         (* and multiply only by 58, the max range *)
         LITERAL(cc, 0w58),
         readword true,
         setvar lab_Cgiven dd,

         (* compute Ccomputed from W1,W2,W3 *)
         (*   C =  (((W1 * 31337 + W2) * 65521 + W3) mod 2^32)
                      mod (58^4) *)
         getvar cc lab_W1,
         LITERAL(bb, 0w31337),
         MUL(aa, cc, bb),
         getvar cc lab_W2,
         ADD(aa, aa, cc),
         LITERAL(bb, 0w65521),
         MUL(aa, aa, bb),
         getvar cc lab_W3,
         ADD(aa, aa, cc),
         LITERAL(bb, 0w58 * 0w58 * 0w58 * 0w58),
         (* compute aa = aa mod bb *)
         DIV(cc, aa, bb),
         MUL(cc, cc, bb),
         SUB(aa, aa, cc, hh),
         (* aa is actual checksum *)
         setvar lab_Ccomputed aa,

         (* next compare computed with actual *)
         getvar bb lab_Cgiven,

         SUB(aa, aa, bb, hh),
         JNZ(aa, lab_wrongpass, dd, ee, hh),
         
         MANY (emitstring [aa, bb, cc] "decrypting...\n"),

         (* start decrypting *)
         LITERAL_ADDR(aa, lab_decrypt, hh),
         LOADPROG(zz, aa)
             ]
        end),
        (* assumes that lab_key contains the key (16 words).
           assumes that lab_teamstuff contains the team 
           info (16 words).
           decrypts the lab_nwords words in lab_crypted
           into a new array, the pointer to
           which is placed in lab_decrypted. *)
       (lab_decrypt,
        [
         
         (* allocate destination *)
         getvar aa lab_nwords,
         ALLOC(bb, aa),
         setvar lab_decrypted bb,

         (* intialize key.
            for (aa)i = 0 to 255,
            (cc)j = (j + s[i] + k[i]) mod 256
            swap s[i] and s[j] *)
         LITERAL(bb, 0w255),
         LITERAL(aa, 0w0),
         LITERAL(cc, 0w0),

         LITERAL_ADDR(ff, lab_decrypt_keyinit, hh),
         LOADPROG(zz, ff)]),

       (* this loop sets up lab_s, lab_i, lab_j *)
       (lab_decrypt_keyinit,
        [
         (* does (aa)i have any bits above 0xFF set? *)
         NOT(ff, bb),
         AND(ff, aa, ff),
         (* if so, done. *)
         JNZ(ff, lab_decrypt_loop, dd, ee, hh),
         
         (* update j. add s[i] *)
         LITERAL_ADDR(ff, lab_s, hh),
         ADD(ff, ff, aa),
         ASUB(dd, zz, ff),
         ADD(cc, cc, dd),   (* saves dd = s[i] *)
         (* add k[i mod 16]. *)
         LITERAL_ADDR(ff, lab_key, hh),
         LITERAL(ee, 0w15),
         AND(ee, aa, ee),
         ADD(ff, ff, ee),
         ASUB(ff, zz, ff),
         ADD(cc, cc, ff),
         (* mod 256 *)
         AND(cc, cc, bb),
         
         (* now swap s[i] and s[j]. *)
         (* dd = s[i]. get ee: *)
         LITERAL_ADDR(ff, lab_s, hh),
         ADD(ff, ff, cc),
         ASUB(ee, zz, ff),
         (* now ee = s[j]. swap *)
         UPD(zz, ff, dd),

         LITERAL_ADDR(ff, lab_s, hh),
         ADD(ff, ff, aa),
         UPD(zz, ff, ee),
         
         (* increment i. *)
         LITERAL(ff, 0w1),
         ADD(aa, aa, ff),

         (* then loop. *)
         LITERAL_ADDR(ff, lab_decrypt_keyinit, hh),
         LOADPROG(zz, ff)
         ]),

       (lab_decrypt_loop,
        [
         (* Are we done? then end *)
         getvar aa lab_n,
         getvar bb lab_nwords,
         SUB(aa, bb, aa, hh),
         JZ(aa, lab_decrypt_done, cc, ff, hh),


         (* for each word, generate a
            new rc4 word in cc *)
         LITERAL(cc, 0w0),
         CALLWITH(aa, lab_onebyte, ff, hh),
         CALLWITH(aa, lab_onebyte, ff, hh),
         CALLWITH(aa, lab_onebyte, ff, hh),
         CALLWITH(aa, lab_onebyte, ff, hh),

         (* MANY (emitstring [aa, bb] ("****")), *)

         getvar aa lab_n,
         LITERAL_ADDR(bb, lab_crypted, hh),
         (* get next word from input *)
         ADD(bb, aa, bb),
         (* MANY (emitstring [dd] ("||||")),*)
         ASUB(dd, zz, bb),
         (* XOR it with cc *)
         XOR(bb, cc, dd, hh),
         (* store it into same position of output *)
         getvar cc lab_decrypted,
         UPD(cc, aa, bb),
         

         (* increment word *)
         LITERAL(bb, 0w1),
         ADD(aa, bb, aa),
         setvar lab_n aa,

         (* and start again *)
         LITERAL_ADDR(aa, lab_decrypt_loop, hh),
         LOADPROG(zz, aa)]),
       (* return (cc << 8) | next_byte 
          to address in aa. *)
       (lab_onebyte, 
        [
         setvar lab_saveret aa,
         (* shift cc 8 places *)
         LITERAL(bb, W.fromInt 256),
         MUL(cc, bb, cc),
         
         LITERAL(bb, W.fromInt 255),
         getvar dd lab_i,
         getvar ee lab_j,

         (* WRITE(dd), WRITE(ee), *)

         (* (dd) i = i + 1 % 256 *)
         LITERAL(ff, W.fromInt 1),
         ADD(dd, dd, ff),
         AND(dd, dd, bb),

         (* (ee) j = j + s[i] % 256 *)
         LITERAL_ADDR(ff, lab_s, hh),
         ADD(ff, ff, dd),
         ASUB(ff, zz, ff), (* s[i] *)
         ADD(ee, ee, ff),
         AND(ee, ee, bb),

         (* save dd, ee *)
         setvar lab_i dd,
         setvar lab_j ee,

         (* WRITE(dd), WRITE(ee), *)

         (* now we swap the two bytes *)
         LITERAL_ADDR(ff, lab_s, hh),
         ADD(ff, ff, dd),
         ASUB(aa, zz, ff), (* aa = s[i] *)
         
         LITERAL_ADDR(ff, lab_s, hh),
         ADD(ff, ff, ee),
         ASUB(bb, zz, ff), (* bb = s[j] *)
         (* do swaps .. s[j] := aa *)
         (* WRITE(aa), WRITE (bb),  *)

         UPD(zz, ff, aa),
         LITERAL_ADDR(ff, lab_s, hh),
         ADD(ff, ff, dd),
         (* s[i] := bb *)
         UPD(zz, ff, bb),
         
         (* the byte (aa) 
            is s[s[i] + s[j] % 256] *)
         ADD(aa, aa, bb),
         LITERAL(bb, W.fromInt 255),
         AND(aa, aa, bb),
         LITERAL_ADDR(ff, lab_s, hh),
         ADD(ff, ff, aa),
         ASUB(aa, zz, ff),

         (* put it in c's low bits *)
         ADD(cc, cc, aa),

         (* print it out.. *)
         (* WRITE(aa), *)

         (* HALT, *)
         getvar aa lab_saveret,
         LOADPROG(zz, aa)
         ]),

       (lab_decrypt_done,
        [(* MANY(emitstring [aa, bb, cc, dd, ee] "JUMP\n"), *)
         getvar aa lab_decrypted,

         (* check that the first word is good *)
         ASUB(bb, aa, zz),
         LITERAL_ANY(cc, firstword, hh),
         SUB(bb, cc, bb, hh),
         JNZ(bb, lab_wrongpass, dd, ee, ff),

         MANY (emitstring [bb, cc, dd, ee, ff, hh] "ok\n"),

         (* finally, put the team secret in the binary at
            the offset we know. Three words: *)
         getvar bb lab_W1,
         LITERAL(cc, W.fromInt (dynregion + 0)),
         UPD(aa, cc, bb),
         getvar bb lab_W2,
         LITERAL(cc, W.fromInt (dynregion + 1)),
         UPD(aa, cc, bb),
         getvar bb lab_W3,
         LITERAL(cc, W.fromInt (dynregion + 2)),
         UPD(aa, cc, bb),

         (* jump to its beginning. *)
         LOADPROG(aa, zz)]),

       (lab_wrongpass,
        emitstring [bb, cc, dd, ee, ff] "wrong key\n" @ 
        [HALT]),

       (* this must start initialized to 0 *)
       (lab_n, [DATA 0w0]),

       (* PERF various of these labels have nonoverlapping
          lifetimes *)

       (* these needn't be initialized *)
       (lab_decrypted, [BSSDATA]),
       (lab_saveret, [BSSDATA]),

       (lab_W1, [BSSDATA]),
       (lab_W2, [BSSDATA]),
       (lab_W3, [BSSDATA]),
       (lab_Cgiven, [BSSDATA]),
       (lab_Ccomputed, [BSSDATA]),

       (lab_teamstuff, List.tabulate (16, fn n => BSSDATA)),
       (lab_key, List.tabulate (16, fn n => BSSDATA))]

      @
      let
        val s = W8A.tabulate(256, Word8.fromInt)
        val i = 0w0
        val j = 0w0
      in
          [(lab_s, List.tabulate(256,
                                 fn n =>
                                 DATA (W.fromInt (Word8.toInt (Word8Array.sub(s, n)))))),
           (lab_i, [DATA (W.fromInt (Word8.toInt i))]),
           (lab_j, [DATA (W.fromInt (Word8.toInt j))])]
      end
    end
      

  fun rc4_encrypt prog =
    let

      val arc = 
          ARCFOUR.init
          (Word8Vector.tabulate(size ARCFOUR_KEY,
                                fn n => Word8.fromInt 
                                (ord (String.sub(ARCFOUR_KEY, n)))))
          
      (* ENH should discard for extra crypto *)
      (* val () = ARCFOUR.discard arc 1024 *)

      fun to32 w8 = W.fromInt (Word8.toInt w8)
      fun ac () = 
        let
          val b = ARCFOUR.byte arc
        in
          (* print (Word8.toString b ^ " "); *)
          to32 b
        end
      fun arcword () =
              W.orb
              (W.<<(ac (), 0w24),
               W.orb
               (W.<<(ac (), 0w16),
                W.orb
                (W.<<(ac (), 0w8),
                 ac ())))

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
            | _ => raise Decrypt "byte stream not multiple of four"

      val progword = SimpleStream.tolist progword

      val firstword = (case progword of
                         (w :: _) => w
                       | _ => raise Decrypt "empty program!")

      val progword = map (fn w => DATA (W.xorb (w, arcword()))) progword

    in
        (progword, firstword)
    end

  (* Generate a program suitable for assembly that
     does self decryption. The program data are
     given as a byte stream *)
  fun self_decrypt ((prog, labmap), dynlab) =
    let
      (* val _ = print "Encrypting...\n" *)
      val (cdata, firstword) = rc4_encrypt prog
      (* val _ = print " done.\n" *)
    in
        (dc_code { firstword = firstword, 
                   dynregion = 
                   (case V.Map.find (labmap, dynlab) of
                      NONE => 
                        raise Decrypt ("can't wrap because can't " ^
                                       "find dynamic region " ^
                                       V.tostring dynlab)
                    | SOME off => off) } @
         [(lab_crypted, cdata),
          (lab_nwords, [DATA (W.fromInt (length cdata))])],
         lab_prompt)
    end

end
