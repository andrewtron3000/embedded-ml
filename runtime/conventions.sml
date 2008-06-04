
structure Conventions =
struct

  structure W = Word32
  exception Conventions of string

  val aa = 0 : UMA.reg
  val bb = 1 : UMA.reg
  val cc = 2 : UMA.reg
  val dd = 3 : UMA.reg
  val ee = 4 : UMA.reg
  val ff = 5 : UMA.reg
  val gg = 6 : UMA.reg
  val hh = 7 : UMA.reg

  (* zero reg *)
  val zz = gg

  val SIGN_BIT = Word32.<< (0w1, 0w31)
  val lab_signbit = Variable.namedvar "sign_bit"

  (* everything but the sign bit *)
  val MANTISSA_MASK = Word32.notb(SIGN_BIT)
  val lab_mantissamask = Variable.namedvar "mantissa_mask"

  (* SUSP one of these is the return reg?? *)
  (* registers that the gc check can trash *)
  val gc_trash = [aa, bb, cc, dd]
  (* return address for gc_check; must be one of the trash set *)
  val gc_returnreg = aa
  val gc_notreturnreg = bb

    
  fun emitstring regs s =
    let

        val printing = ref false

        fun shuffle l = ListUtil.sort (fn _ => 
                                     if W.andb(HumlockUtil.randomword (), 0w1) = 0w1
                                     then LESS
                                     else GREATER) l

        val initial = 
          shuffle
          (map (fn r => (r, W.fromInt ~1))
           regs)

        (* best sequence found, best length *)
        val best = ref NONE
        val bestl = ref 999999
        val paths = ref 0

        val MAXSILENT = 10000
        val MAXPATHSTRY = 3000000
        exception Giveup

        fun try _ n insns nil = (paths := !paths + 1;
                                 (if !paths > MAXSILENT andalso not (!printing)
                                  then (print ("emit '" ^ 
                                               String.toString s ^ "'... ");
                                        printing := true)
                                  else ());

                                 (if !paths > MAXPATHSTRY
                                  then raise Giveup
                                  else ());
                                 if n < !bestl
                                 then (best := SOME insns;
                                       bestl := n)
                                 else ())
          | try regs n insns (c :: t) =
          let val h = W.fromInt (ord c)
          in
            (* if it's already there, eagerly apply. *)
            case List.find (fn (_, x) => x = h) regs of
              NONE =>
                (* need to load this constant. are any registers
                   never used again? if so, eagerly re-use them. *)
                ( (* PERF maybe shouldn't be doing O(n) search here
                     inside exponential algorithm *)
                  case ListUtil.extract (fn (_, x) =>
                                         not (List.exists (fn c' =>
                                                           W.fromInt (ord c') = x) t)) regs of
                    SOME ((r, u), others) =>
                      (* this time we'll find it loaded *)
                      let in
                        (*
                        print ("EAGER: " ^ implode [c]^ " (" ^
                               Int.toString r ^ " = " ^ implode [chr (W.toInt u)] ^ 
                               " never used again)\n"); *)
                        try ((r, h) :: others) (n + 1) (UMA.LITERAL (r, h) :: insns) (c :: t)
                      end
                  | NONE =>
                      (* every register could be re-used. try them all. *)
                      let 
                        fun allregs bef nil = ()
                          | allregs bef ((r, u) :: aft) =
                          let in
                            (*
                            print ("try assigning " ^ implode [c] ^ " to " 
                                   ^ Int.toString r ^ "\n"); *)
                            try (((r, h) :: aft) @ bef) (n + 1) 
                                (UMA.LITERAL(r, h) :: insns) (c :: t);
                            allregs ((r, u) :: bef) aft
                          end
                      in
                        allregs nil regs
                      end)

            | SOME (r, _) => 
                     let in
                       (* print "FOUND\n"; *)
                       try regs (n + 1) (UMA.WRITE r :: insns) t
                     end
          end

        val string = explode s
                                  
        fun try_start regs n insns nil =
          (* no shortage of registers. shuffle here since every
             operation is a load into a unique register *)
          try regs n (shuffle insns) string
          | try_start regs n insns (h :: t) =
          (* already loaded? *)
          if List.exists (fn (_, x) => x = W.fromInt (ord h)) regs
          then try_start regs n insns t
          else
            (case List.partition (fn (_, x) => x = W.fromInt ~1) regs of
               (* used up! *)
               (nil, regs) => try regs n (shuffle insns) string
             | ((r, _)::tr, rest) => 
                 try_start ((r, W.fromInt (ord h)) :: tr @ rest) (n + 1)
                 (UMA.LITERAL(r, W.fromInt (ord h)) :: insns) t)

        val () = TextIO.flushOut TextIO.stdOut
        val () = (try_start initial 0 nil string) handle Giveup => ()
    in
      if !printing then print (" done. (" ^ Int.toString (!paths) ^ " paths.)\n") else ();
      (case !best of
         NONE => raise Conventions "couldn't emit??"
       | SOME l => rev l)
    end
    

  fun errormsg s = emitstring [aa, bb, cc, dd, ee, ff, hh] s

end
