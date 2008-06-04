
(* Embed extra junk in the outer layer, in order to distract
   from actually useful things. *)

structure ExtraJunk =
struct

  structure V = Variable
  open Conventions
  open UMA

  val root = (FSUtil.chdir_excursion 
              (CommandLine.name())
              (fn _ =>
               Posix.FileSys.getcwd ()))
    
  (* if running under NJ, default back to current dir, since
     we don't want to root ourselves at the sml/nj binary! *)
  val root = if Option.isSome (StringUtil.find "smlnj" root)
             then Posix.FileSys.getcwd ()
             else root

  fun random_order _ =
      if Word32.andb (HumlockUtil.randomword (), 0w1) = 0w1
      then LESS
      else GREATER
  fun shuffle l = ListUtil.sort random_order l

  val strings =
    [(* illuminati *)
     "fnord",
     (* us *)
     "__CBV__", "CBV", "CBV",
     implode (shuffle (explode "cult of the bound variable")),
     (* halo ARG *)
     "i love bees",
     (* The Beast (AI ARG) *)
     "Evan Chan was murdered",
     (* CIA mind-control project *)
     "MKULTRA",
     (* hitchhiker's guide *)
     "__42__", "42", "42",
     (* lemony snickett *)
     "__VFD__",
     (* dr. who (2005) *)
     "bad wolf",
     (* beatles *)
     implode (rev (explode "paul is dead")),
     (* foucault's pendulum (alchemy, kabbalism, knights templar, etc) *)
     "abracadabra", "abulafia",
     "telluric", "currents", "solomon", "templar",
     "surmount", "welldone", "rakoczi", "tsarogy",
     "raimundus", "lullus",
     (* stephenson (more or less) *)
     "ignoti et quasi occulti",
     "societas_eruditorum",
     "novus ordo seclorum",
     (* dan brown *)
     "so dark the con of man",
     (* lambda calc, ml, etc *)
     "lambda", "lambda", "apply", "eval", "apply", "eval",
     "tycon mismatch",
     (* aliens *)
     "area51", "roswell"
     ]
    
  fun extrify (labs, main) =
      let
          fun stringdata s =
            let
                  val s = Vector.tabulate (size s,
                                           fn c =>
                                           String.sub(s, c))
                      
                  fun to32 (SOME a) = Word32.fromInt (ord a)
                    | to32 NONE = 0w0
                      
                  val prog = SimpleStream.fromvector s
                  fun progword () =
                      case (prog (), prog (), prog (), prog ()) of
                          (NONE, NONE, NONE, NONE) => NONE
                        | (a, b, c, d) =>
                              SOME
                              (DATA (W.orb
                                     (W.<<(to32 a, 0w24),
                                      W.orb
                                      (W.<<(to32 b, 0w16),
                                       W.orb
                                       (W.<<(to32 c, 0w8), to32 d)))))
              in
                  (Variable.namedvar "string",
                   SimpleStream.tolist progword)
            end
                  
          fun datafile f = stringdata (StringUtil.readfile (FSUtil.dirplus root f))
          
          val extras =
              map datafile
              ["runtime/confuse/cbv.gif",
               "runtime/confuse/cbv.raw"] @
              map stringdata strings
              
          val labs = extras @ labs
              
          (* doesn't give a fair shuffle, so repeat
             several times. *)
          val labs = shuffle labs
          val labs = shuffle labs
          val labs = shuffle labs
      in
          
          (labs, main)
      end
  
end
