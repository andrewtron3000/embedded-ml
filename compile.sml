
structure Compile :> COMPILE =
struct

    val self_print = Params.flag false
        (SOME ("-print",
               "Generate wrapper that prints out program instead of running it")) "self_print"

    val self_decompress = Params.flag false
        (SOME ("-sd",
               "Generate self-decompressing code")) "self_decompress"

    val self_decrypt = Params.flag false
        (SOME ("-crypt",
               "Generate self-decrypting code")) "self_decrypt"

    val extra_junk = Params.flag false
        (SOME ("-junk",
               "Add extra junk to outer layer")) "extra_junk"

    val self_checkloadprog = Params.flag false
      (SOME ("-scloadprog",
             "(expensive) self-check for nonzero loadprog")) "scloadprog"

    val obfuscate_outer = Params.flag false
        (SOME ("-obsouter",
               "Fill unused bits with garbage to obfuscate")) "obs_outer"

    val showil = Params.flag false
        (SOME ("-showil",
               "Show internal language AST")) "showil"

    val showcps = Params.flag false
        (SOME ("-showcps", 
               "Show internal CPS after each phase")) "showcps"

    val execcps = Params.flag false
        (SOME ("-execcps",
               "Try interpreting the CPS language at various phases, for debugging")) "execcps"

    val optil = Params.flag true
        (SOME ("-optil", 
               "Optimize the intermediate language")) "optil"

    val optcps = Params.flag true
        (SOME ("-optcps", 
               "Optimize the CPS language")) "optcps"

    val showfinal = Params.flag false
        (SOME ("-showfinal", 
               "Show the final versions of each phase")) "showfinal"

    val writecps = Params.flag true
        (SOME ("-writecps", 
               "Write .cps file")) "writecps"

    val verbose = Params.flag true
        (SOME ("-v",
               "Show progress")) "verbose"

    val target_language = Params.param
                              ""
                              (SOME ("-target-language",
                                     "Language to generate: (c, forth)")) "target_language"
        
    val c_backend = Params.flag false
        (SOME ("-cbackend",
               "generate C output")) "c_backend"

    val c_output_style = Params.param "standalone"
        (SOME ("-c_output_style",
               "Generate \"standalone\" or \"linkable\"")) "c_output_style"

    val cflags = Params.param ""
        (SOME ("-cflags",
               "CFLAGS to add to the makefile (\"\")")) "cflags"

    val ldflags = Params.param ""
        (SOME ("-ldflags",
               "LDFLAGS to add to the makefile (\"\")")) "ldflags"

    val gcctarget = Params.param ""
        (SOME ("-gcctarget",
               "Cross compilation target when using gcc (\"\")")) "gcctarget"

    val fr_backend = Params.flag false
        (SOME ("-frbackend",
               "generate a Forth file")) "frfile"

    val fr_platform = Params.param "gforth"
        (SOME ("-fr_platform",
               "target which forth platform? (gforth) ")) "fr_platform"

    val arduino_backend = Params.flag false
        (SOME ("-arduinobackend",
               "generate Arduino sketch output")) "arduino_backend"

    exception Compile of string

    fun vprint s =
      if !verbose then print s
      else ()

    (* could also turn off CPSOpt.debug *)
    fun quiet () =
        let 
            val debugopt = Params.flag true NONE "debugopt"
        in
            showil := false;
            debugopt := false
        end
    
    (* (* not portable *)
    val pids = (Word.toString
                (Posix.Process.pidToWord
                 (Posix.ProcEnv.getpid ())))
        *)

    val pids = Time.toString (Time.now())

    fun execredir file (prog, args) =
        let 
            (* avoid /tmp symlink races *)
            val _ = (Posix.FileSys.unlink file) handle _ => ()
            val fd = Posix.FileSys.createf 
                (file, Posix.FileSys.O_RDWR,
                 Posix.FileSys.O.flags
                 [Posix.FileSys.O.excl],
                 Posix.FileSys.S.irwxu)
        in
            Posix.IO.dup2 { old = fd, new = Posix.FileSys.stdout };
            (* clear close-on-exec flag, if set *)
            Posix.IO.setfd ( Posix.FileSys.stdout, 
                             Posix.IO.FD.flags nil );
            Posix.Process.exec (prog, prog :: args)         
        end

    fun system (prog, args) =
        let val file = "/tmp/hemtmp_" ^ pids
        in
        (case Posix.Process.fork () of
             NONE => execredir file (prog, args)
           | SOME pid =>
             (case Posix.Process.waitpid (Posix.Process.W_CHILD(pid), nil) of
                  (_, Posix.Process.W_EXITED) => NONE
                | (_, Posix.Process.W_EXITSTATUS w8) => SOME w8
                | _ => 
                      let in
                          (* XXX print contents of 'file', which
                             contains its stdout. *)
                          raise Compile (prog ^ " exited strangely")
                      end))
        end

    fun systeml nil = NONE
      | systeml ((p,a)::t) =
        (case system (p, a) of
             NONE => systeml t
           | fail => fail)

    exception Done of string

    fun getel file =
      let
        fun tokenize s = 
          Parsing.transform Tokenize.token (Pos.markstreamex file s)
          
        fun parseexpression G s = 
          Parsing.transform (Parse.exp G) (tokenize s)
          
        val parsed = Stream.tolist 
          (parseexpression Initfix.initial 
           (StreamUtil.ftostream file))
      in
        case parsed of
          [e] => e
        | nil => raise Compile "Parse error: no expression"
        | _ => raise Compile "Parse error: program must be single expression"
      end handle Parse.Parse s => raise Compile ("Parse error: " ^ s)

    fun getil file =
        let
          val () = vprint "Parsing...\n"
          val e = getel file

          (* wrap with standard declarations *)
          val el = Initial.wrap e
            
          (* rewrite nullary constructors and types *)
          val el = (Nullary.nullary el)
            
          val () = vprint "Elaborating...\n"
          val (il, t) = 
            (Elaborate.elab Initial.initial el)
            handle Pattern.Pattern s =>
              raise Compile ("pattern: " ^ s)
        in
          il
        end

    fun compile fullfile out =
        FSUtil.chdir_excursion fullfile
        (fn file =>
        let
            (* if there's no outfile, base it on the input name *)

            val (base, _) = FSUtil.splitext file
            val out =
                (case out of
                     "" => base ^ (if !c_backend then ".c"
                                   else if !fr_backend then ".fr"
				   else if !arduino_backend then ".ino"
                                   else raise Compile ("Please specify a backend to use."))
                   | s => s)

            val _ = SymbolDB.clear ()

            val inter = getil file

            val opted = 
                if (!optil)
                then
                    let val _ = 
                        if (!showil)
                        then 
                            let in
                                print "il expression:\n";
                                Layout.print (ILPrint.etol inter, print);
                                print "\n\n"
                            end
                        else ()
                    in
                        vprint "Optimizing IL...\n";
                        ILOpt.optimize inter
                    end
                else inter

            val _ = 
                if (!showil) orelse (!showfinal)
                then 
                    let in
                        print "after optimization:\n";
                        Layout.print (ILPrint.etol opted, print);
                        print "\n\n"
                    end
                else ()

            val () = vprint "CPS converting...\n"
            val c = ToCPS.convert 
              (fn v => CPS.Primop(Primop.PHalt, [], nil, nil))
              opted

            val () = 
              if !execcps
              then 
                let in
                  print "\nExecuting first CPS:\n";
                  CPSExec.exec c
                end
              else ()

            val c = if !optcps
                    then let val () = vprint "Optimizing CPS...\n" in
			   CPSOpt.optimize c
			 end
                    else c 

            val _ =
              if !showcps andalso !optcps
              then 
                let in
                  print "\nCPS after optimization:\n";
                  CPSPrint.printe c;
                  print "\n\n"
                end
              else ()

            val () = 
              if !execcps
              then 
                let in
                  print "\nExecuting CPS after optimization:\n";
                  CPSExec.exec c
                end
              else ()
	    (*
            val _ =
              if !writecps
              then CPSPrint.writee (base ^ ".cps") c
              else ()
	     *)
            val () = vprint "Closure converting...\n"
            val closed = Closure.convert c

            val _ =
              if (!showcps)
              then
                let in
                  print "CPS after closure conversion:\n";
                  CPSPrint.printe closed;
                  print "\n\n"
                end
              else ()

            val () = vprint "Alloc converting...\n"
            val alloced = CPSAlloc.convert closed

            val _ =
              if (!showfinal) orelse (!showcps)
              then
                let in
                  print "CPS after alloc conversion:\n";
                  CPSPrint.printe alloced;
                  print "\n\n"
                end
              else ()

            val _ =
              if !writecps
              then CPSPrint.writee (base ^ ".cps") c
              else ()

            val _ = CPSPrint.writee (base ^ ".cpsa") alloced

            val () = vprint "Backend conversion\n"
            val c_asm = if (!c_backend orelse !arduino_backend) then ((vprint "to C...\n") ; (ToC.convert alloced)) else (nil, Variable.newvar ()) 
            val fr_asm = if (!fr_backend) then ((vprint "to Forth...\n") ; (ToForth.convert alloced)) else (nil, Variable.newvar ()) 
            val () = if not (!c_backend) andalso not (!arduino_backend) andalso not (!fr_backend) then (vprint "SKIPPED...\n") else (vprint "")

(* spoons: no runtime for now 
            val () = vprint "Adding runtime...\n"
            (* prog = program with GC attached, as (code, main)
               fh = for humlock (cr0 and stack space)
               dynr = label for start of dynamic region
               *)
            val (prog, fh, dynr) = (Runtime.wrap asm, true, Runtime.lab_dynamic)
*)
	
	    (*
            val (prog, fh, dynr) =
              if !self_print
              then (vprint "Adding self-printing harness...\n";
                    (SelfPrint.self_print 
                     (Assemble.assemble_stream { for_humlock = fh,
                                                 obfuscate = false }
                      (out ^ "_print_prog") (Runtime.cr0 prog),
                      dynr), false,
                     SelfPrint.lab_dynamic))
              else (prog, fh, dynr)

            (* maybe self-decompressing prog as (code, main) *)
            val (prog, fh, dynr) =
              if !self_decompress
              then (vprint "Adding self-decompression...\n";
                    (Decompress.self_decompress 
                     (Assemble.assemble_stream  { for_humlock = fh,
                                                  obfuscate = false }
                     (out ^ "_uncompressed_prog") (Runtime.cr0 prog),
                      dynr), 
                     false,
                     Decompress.lab_dynamic))
              else (prog, fh, dynr)

            val (prog, fh, dynr) =
                if !self_decrypt
                then (vprint "Adding self-decryption...\n";
                      (Decrypt.self_decrypt
                       (Assemble.assemble_stream { for_humlock = fh,
                                                   obfuscate = false }
                       (out ^ "_plain_prog") (Runtime.cr0 prog),
                        dynr),
                       false,
                       Decrypt.lab_dynamic))
                else (prog, fh, dynr)

            val (prog, fh, dynr) =
                if !self_checkloadprog
                then (vprint "Adding loadprog checker...\n";
                      (SCLoadprog.self_check
                       (Assemble.assemble_stream { for_humlock = fh,
                                                   obfuscate = false }
                        (* XXX we should provide our own CR0 here
                           to make sure it doesn't trash registers
                           we need to preserve for self check *)
                       (out ^ "_scload_prog") (SCLoadprog.cr0 prog),
                        dynr),
                       false,
                       SCLoadprog.lab_dynamic))
                else (prog, fh, dynr)

            val (prog, fh) =
                if !extra_junk
                then (ExtraJunk.extrify prog, fh)
                else (prog, fh)

            (* self-check on the very outside *)
            val code = SelfCheck.wrap prog
	     *)
            val () = vprint "Assembling...\n"

          (* SUSP: reading the runtime at application compile time *)
            val runtime_names = map (fn s => Parse.root ^ s)
                                    (if !fr_backend then ["/runtime/runtime-" ^ (!fr_platform) ^ ".fr"]
                                     else [])
            val runtime_files = map TextIO.openIn runtime_names
            val runtime = String.concat (map TextIO.inputAll runtime_files)
            val includemain = if !fr_backend then ((!fr_platform) = "gforth")
                              else if !c_backend then ((!c_output_style) = "standalone") 
                              else false
        in
	    (*
          Assemble.assemble { for_humlock = fh,
                              target = if !h_backend 
                                       then Assemble.TARG_HEADER
                                       else Assemble.TARG_UM,
                              obfuscate = !obfuscate_outer } out code;
	     *)
            (if !c_backend then CPrint.print_basic base c_asm (* (CPrint.print out includemain runtime c_asm (!cflags) (!ldflags) (!gcctarget)) *)
             else if !arduino_backend then (ArduinoPrint.print out includemain runtime c_asm (!cflags) (!ldflags) (!gcctarget))
             else if !fr_backend then (ForthPrint.print includemain out runtime fr_asm)
             else (vprint "Not printing anything.\n"));
            OS.Process.success
        end)
    handle Compile s => fail ("\n\nCompilation failed:\n    " ^ s ^ "\n")
         | Nullary.Nullary s => fail ("\nCouldn't do EL nullary prepass:\n" ^ s ^ "\n")
         | Context.Absent s => fail ("\n\nInternal error: Unbound identifier '" ^
                                      s ^ "'\n")
         | ILAlpha.Alpha s => fail ("\nIL Alpha: " ^ s ^ "\n")
         | CPSAlpha.Alpha s => fail ("\nCPS Alpha: " ^ s ^ "\n")
         | Decrypt.Decrypt s => fail ("\nSelf Decryption: " ^ s ^ "\n")
         | SelfPrint.Print s => fail ("\nSelf Printing: " ^ s ^ "\n")
         | UMA.UMA s => fail ("\nUM Assembly: " ^ s ^ "\n")
         | ToForth.ToForth s => fail ("\nTo Forth: " ^ s ^ "\n")
         | ToC.ToC s => fail ("\nTo C: " ^ s ^ "\n")
         | ContextMap.ContextMap s => fail ("\nContextMap: " ^ s ^ "\n")
         | Decompress.Decompress s => fail ("\nDecompress: " ^ s ^ "\n")
         | Assemble.Assemble s => fail ("\nAssembling: " ^ s ^ "\n")
         | GC.GC s => fail ("\nGC config: " ^ s ^ "\n")
         | ILOpt.ILOpt s => fail ("\nIL Optimizer: " ^ s ^ "\n")
         | ToCPS.CPS s => fail ("\nCPS Conversion: " ^ s ^ "\n")
         | CPSOpt.CPSOpt s => fail ("\nCPS Optimizer: " ^ s ^ "\n")
         | RegHelper.RegHelper (s, _) => fail ("\nReg Assignment: " ^ s ^ "\n")
         | Done s => fail ("\n\nStopped early due to " ^ s ^ " flag.\n")
         | Variable.Variable s => fail ("\n\nBUG: Variables: " ^ s ^ "\n")
         | Header.Header s => fail("\n\nHeader backend: " ^ s ^ "\n")
         | ex => fail ("\n\nUncaught exception: " ^ exnName ex ^ ": " ^
                        exnMessage ex ^ "\n")

    and fail s =
        let in
            print "\nCompilation failed.\n";
            print s;
            OS.Process.failure
        end

    (* For testing from the SML/NJ prompt *)

    fun tokenize s = 
        (map #1 
         (Stream.tolist
          (Parsing.transform Tokenize.token 
           (Pos.markstream 
            (StreamUtil.stostream s)))))

end
