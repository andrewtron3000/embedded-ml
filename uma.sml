
(* Universal Machine Assembly language *)
structure UMA =
struct

  structure W = Word32
  exception UMA of string

  (* 0 .. 7 *)
  type reg = int

  datatype inst =
    (* A <- B  unless C = 0 *)
    CMOV of reg * reg * reg
    (* replace code with array in A,
       goto address in B *)
  | LOADPROG of reg * reg
  | LITERAL of reg * W.word (* limited to 24 bits? *)
  | HALT
  | READ of reg
  | WRITE of reg
  (* A <- id of new array with B words *)
  | ALLOC of reg * reg
  (* free register *)
  | FREE of reg
  (* A <- B[C] *)
  | ASUB of reg * reg * reg
  (* A[B] <- C *)
  | UPD of reg * reg * reg
  | ADD of reg * reg * reg
  | MUL of reg * reg * reg
  | DIV of reg * reg * reg
  | NAND of reg * reg * reg

  (* advisory ops here. these don't do anything,
     but might make it easier to optimize or
     read the code. *)
  (* ... *)
  | COMMENT of string

  (* debug instructions; the compiler will output these, but they are not part
     of the UM spec.  their encodings ensure that they will have no effect in
     compliant implementations. *)

  (* temporarily stop execution *)
  | BREAK
  (* print out debugging information (a constant and the contents of a
   register *)
  | INFO of W.word * reg
  (* read the time stamp counter into the given register *)
  | RDTSC of reg

  (* macro-ops here. these don't correspond to
     individual instructions, but can be expanded
     in place *)

  (* jump to destination if test reg is not zero
     (zero, less-than-zero respectively). give 
     the test register, the destination, and three
     temporaries *)
  | JNZO of reg * Variable.var * reg * reg * reg option
  | JZO  of reg * Variable.var * reg * reg * reg option
  | JLZO of reg * Variable.var * reg * reg * reg option

  (* CALLWITH (a, v, t1, t2)
     call to a label, storing return address
     (= next instruction) in a *)
  | CALLWITH of reg * Variable.var * reg * reg

   (* If we want to support long jumps (> 300mb),
      supply SOME tmp for the last register. *)
  | LITERAL_ADDRO of reg * Variable.var * reg option

  (* load a 32-bit word *)
  | LITERAL_ANY of reg * W.word * reg
    (* sometimes small enough to use LITERAL.
       sometimes we can LITERAL and then NOT. *)
    (* at worst:
       LITERAL reg, word_hi16
       LITERAL tmp, 65536
       MUL     reg <- tmp * reg
       LITERAL tmp, word_low16
       ADD     reg <- tmp + reg *)

  (* a <- b - c, trashing d *)
  | SUB of reg * reg * reg * reg
    (* NAND a, c, c   (* a = ~c *)
       ADD a, b, a
       LITERAL d, 1
       ADD a, t, a
       *)

  (* a <- b | c, trashing d *)
  | OR of reg * reg * reg * reg
    (* NAND d, b, b
       NAND a, c, c
       NAND a, d, a *)

   (* PERF probably a faster way exists? *)
  (* a <- b ^ c, trashing d *)
  | XOR of reg * reg * reg * reg
    (* OR a, b, c, d
       NAND d, b, c
       NAND a, d, a
       NAND a, a, a *)

    (* a <- ~b *)
  | NOT of reg * reg

  | AND of reg * reg * reg
    (* NAND a, b, c,
       NAND a, a, a *)

    (* a--, trashing b *)
  | DEC of reg * reg
    (* a = b - 1 *)
  | DECTO of reg * reg

  (* other stuff *)
  (* encode the instruction given
     with the wrong endianness. we
     use this for self-checking
     purposes. *)
  | SWAPEND of inst
  (* just give the word directly *)
  | DATA of W.word

  (* address of this label *)
  | DATALAB of Variable.var

   (* reserve one word of space, 
      but it needn't be initialized*)
  | BSSDATA

  (* convenient when calling functions
     that return lists of instructions *)
  | MANY of inst list

   (* no instruction; just marks this label
      for this spot *)
  | LABEL of Variable.var

  (* Nice to open just the instructions sometimes. *)
  structure Inst =
  struct
    datatype inst = datatype inst
      
    fun JLZ_SMALL (a, b, c, d) = JLZO(a, b, c, d, NONE)
    fun JNZ_SMALL (a, b, c, d) = JNZO(a, b, c, d, NONE)
    fun JZ_SMALL (a, b, c, d)  = JZO(a, b, c, d, NONE)
      
    fun JLZ (a, b, c, d, e) = JLZO(a, b, c, d, SOME e)
    fun JNZ (a, b, c, d, e) = JNZO(a, b, c, d, SOME e)
    fun JZ (a, b, c, d, e)  = JZO(a, b, c, d, SOME e)
      
    fun LITERAL_ADDR_SMALL (a, b) = LITERAL_ADDRO (a, b, NONE)
    fun LITERAL_ADDR (a, b, c) = LITERAL_ADDRO (a, b, SOME c)
  end
  open Inst


  fun regword r =
    if r >= 0 andalso r <= 8 
    then W.fromInt r
    else raise UMA ("bad register: " ^ Int.toString r)

  fun triple tag (r1, r2, r3) =
    (* tag words are lg(14)=4 bits.
       each reg is lg(8)=3 bits. *)
    W.orb(W.<<(tag, 0w28),
          W.orb(W.<<(regword r1, 0w6),
                W.orb(W.<<(regword r2, 0w3),
                      regword r3)))

  val mask_z = triple 0w15 (0, 0, 0)
  val mask_s = triple 0w15 (0, 0, 7)
  val mask_d = triple 0w15 (0, 7, 7)
  val mask_t = triple 0w15 (7, 7, 7)
  val mask_a = 0w0 : W.word
  val mask_0 = 0wxFFFFFFFF : W.word

  (* encode an instruction into a 32-bit word *)
  fun encodemask (CMOV r) = (triple 0w0 r, mask_t)
    | encodemask (ASUB r) = (triple 0w1 r, mask_t)
    | encodemask (UPD r)  = (triple 0w2 r, mask_t)
    | encodemask (ADD r)  = (triple 0w3 r, mask_t)
    | encodemask (MUL r)  = (triple 0w4 r, mask_t)
    | encodemask (DIV r)  = (triple 0w5 r, mask_t)
    | encodemask (NAND r) = (triple 0w6 r, mask_t)

    | encodemask HALT                = (triple 0w7 (0, 0, 0), mask_z)
    | encodemask (ALLOC (r1, r2))    = (triple 0w8  (0, r1, r2), mask_d)
    | encodemask (FREE r)            = (triple 0w9  (0, 0, r), mask_s)
    | encodemask (WRITE r)           = (triple 0w10 (0, 0, r), mask_s)
    | encodemask (READ r)            = (triple 0w11 (0, 0, r), mask_s)
    | encodemask (LOADPROG (r1, r2)) = (triple 0w12 (0, r1, r2), mask_d)
    | encodemask (LITERAL (r, w))    =
      (if W.andb(W.<<(0wxFFF, 0w25), w) <> 0w0
       then raise UMA ("literal too large: " ^ W.toString w)
       else W.orb((* tag *)
                  W.<<(0w13, 0w28),
                  (* register *)
                  W.orb(W.<<(regword r, 0w25),
                        (* literal *)
                        w)), mask_0)

    | encodemask (DATA w) = (w, mask_0)
    | encodemask BSSDATA = (0w0, mask_a)

    | encodemask BREAK                = 
        let in
            print ("WARNING: ENCODING NON-STANDARD BREAK\n");
            (W.orb (triple 0w0 (0, 0, 0), W.<< (0w1, 0w24)), mask_0)
        end

    | encodemask (INFO (w, r))        =
        let in
            print ("WARNING: ENCODING NON-STANDARD INFO\n");
            (W.orb (W.orb (triple 0w0 (r, r, r), W.<< (0w2, 0w24)),
                    W.<< (W.andb (w, 0w32767), 0w9)), mask_0)
        end

    | encodemask (RDTSC r) =
        let in
            print ("WARNING: ENCODING NON-STANDARD RDTSC\n");
            (W.orb (triple 0w0 (r, r, r), W.<< (0w3, 0w24)),
             mask_0)
        end

    | encodemask (SWAPEND i) =
      let
        val (w, _) = encodemask i
        val d = W.andb(W.>>(w, 0w24), 0wxFF)
        val c = W.andb(W.>>(w, 0w16), 0wxFF)
        val b = W.andb(W.>>(w, 0w8), 0wxFF)
        val a = W.andb(w, 0wxFF)
      in
          (foldl W.orb 0w0
           [W.<<(a, 0w24),
            W.<<(b, 0w16),
            W.<<(c, 0w8),
            d], mask_0)
      end
    | encodemask _ = raise UMA "unimplemented instruction (maybe macro-op?)"

  and encode i =
      let
          val (word, mask) = encodemask i
      in
          word
      end

end
