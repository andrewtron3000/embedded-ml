
structure CPrint =
struct

  exception Print of string 
  structure W = Word32

  open C

  fun asm_to_string h =
    case h of 
        CONST i => "0x" ^ (StringUtil.lcase (W.toString i))
      | CAST (t, v) => "( " ^ t ^ " )( " ^ (asm_to_string v) ^ " )"
      | DEBUG s => "" (* s *)
      | COMMENT s => "" (* "fprintf(stderr, \"" ^ s ^ "\\n\")" ^ (asm_to_string SEPARATOR) *)
      | LABEL_REF s => "_" ^ s
      | GETC => "io.getc()"
      | PUTC i => "io.putc(" ^ (asm_to_string i) ^ ")"
      | AVAILC => "availc()"
      | ADDRESS_OF is => "&(" ^ (asm_to_string is) ^ ")"
      | ALLOC_TRACED_STRING (i1, i2) => "alloc_traced_string(" ^
                                        (asm_to_string i1) ^ ", " ^
                                        (asm_to_string i2) ^ ")"
      | ALLOC_TRACED_ARRAY (i1, i2) => "alloc_traced_array(" ^
                                       (asm_to_string i1) ^ ", " ^
                                       (asm_to_string i2) ^ ")"
      | ALLOC_TAGGED (i1, i2) => "alloc_tagged(" ^
                                 (asm_to_string i1) ^ ", " ^
                                 (asm_to_string i2) ^ ")"
      | ALLOC_UNTRACED (i1, i2) => "alloc_untraced(" ^
                                   (asm_to_string i1) ^ ", " ^
                                   (asm_to_string i2) ^ ")"
      | UPDATE_STACK (i, code) => (asm_to_string (STACKVAR i)) ^ " = " ^ (asm_to_string code) ^ (asm_to_string SEPARATOR) 
      | STACKVAR i => "Stackvar(" ^ (asm_to_string (CONST i)) ^ ")"
      | INTVAL code => "Intval(" ^ (asm_to_string code) ^ ")"
      | TUPLEVAR (code, i) => "Tupleval(" ^ (asm_to_string code) ^ ", " ^ (asm_to_string (CONST i)) ^ ")"
      | ARRAYVAR (code, icode) => "Arrayval(" ^ (asm_to_string code) ^ ", " ^ (asm_to_string icode) ^ ")"
      | STORE (target, source) => (asm_to_string target) ^
                                  " = " ^
                                  (asm_to_string source) ^ (asm_to_string SEPARATOR)  (* ^
                                                            "assert (( " ^ (asm_to_string target) ^ " >= &storage[0]) && (" ^ (asm_to_string target) ^ " < &storage[10000]))" ^ (asm_to_string SEPARATOR) *)
      | DEREFERENCE target => "D(" ^ (asm_to_string target) ^ ")"
      | NATIVE_CALL (f, c, a) => f ^ "(0x" ^ (W.toString c) ^ ", " ^ (asm_to_string a) ^ ")"
      | VARIABLE_REF i => "(stackframe + 0x" ^ (W.toString i) ^ ")"
      | CMP_EQ (l, r) => "(" ^ (asm_to_string l) ^ " == " ^ (asm_to_string r) ^ ")"
      | CMP_NEQ (l, r) => "(" ^ (asm_to_string l) ^ " != " ^ (asm_to_string r) ^ ")"
      | CMP_LESSTHAN (l, r) => "( ((int32_t) " ^ (asm_to_string l) ^ ") < ((int32_t) " ^ (asm_to_string r) ^ "))"
      | CMP_LESSTHANEQ (l, r) => "( ((int32_t) " ^ (asm_to_string l) ^ ") <= ((int32_t) " ^ (asm_to_string r) ^ "))"
      | CMP_GREATERTHAN (l, r) => "( ((int32_t) " ^ (asm_to_string l) ^ ") > ((int32_t) " ^ (asm_to_string r) ^ "))"
      | CMP_GREATERTHANEQ (l, r) => "( ((int32_t) " ^ (asm_to_string l) ^ ") >= ((int32_t) " ^ (asm_to_string r) ^ "))"
      | MULTIPLY (l, r) => "(" ^ (asm_to_string l) ^ " * " ^ (asm_to_string r) ^ ")"
      | ADD (l, r) => "(" ^ (asm_to_string l) ^ " + " ^ (asm_to_string r) ^ ")"
      | SUBTRACT (l, r) => "(" ^ (asm_to_string l) ^ " - " ^ (asm_to_string r) ^ ")"
      | DIVIDE (l, r) => "(" ^ (asm_to_string l) ^ " / " ^ (asm_to_string r) ^ ")"
      | SDIVIDE (l, r) => "( ((int32_t) " ^ (asm_to_string l) ^ ") / ((int32_t) " ^ (asm_to_string r) ^ "))"
      | MODULO (l, r) => "(" ^ (asm_to_string l) ^ " % " ^ (asm_to_string r) ^ ")"
      | AND (l, r) => "(" ^ (asm_to_string l) ^ " & " ^ (asm_to_string r) ^ ")"
      | XOR (l, r) => "(" ^ (asm_to_string l) ^ " ^ " ^ (asm_to_string r) ^ ")"
      | OR (l, r) => "(" ^ (asm_to_string l) ^ " | " ^ (asm_to_string r) ^ ")"
      | NOT l => "(~" ^ (asm_to_string l) ^ ")"
      | LSHIFT (l, r) => "(" ^ (asm_to_string l) ^ " << " ^ (asm_to_string r) ^ ")"
      | RSHIFT (l, r) => "(" ^ (asm_to_string l) ^ " >> " ^ (asm_to_string r) ^ ")"
      | LABEL_AS_VALUE l => "(unsigned long) " ^ (asm_to_string l) 
      | TEMP_VARIABLE => "temp" 
      | GOTO_LABEL i => "return " ^ (asm_to_string i) ^ (asm_to_string SEPARATOR)
      | GOTO_ADDRESS i => "return (void (*)())" ^ (asm_to_string i) ^ (asm_to_string SEPARATOR)
      | IF (b, ct, cf) => "if (" ^ (asm_to_string b) ^ ") {" ^ (asm_to_string SEPARATOR) ^
                          (asms_to_string ct) ^ (asm_to_string SEPARATOR) ^ "} else {" ^ (asm_to_string SEPARATOR) ^
                          (asms_to_string cf) ^ (asm_to_string SEPARATOR) ^ "}" ^ (asm_to_string SEPARATOR)
      | SWITCH (b, ics, def) => 
        let
           fun alternative(i, cs) = "case " ^ (asm_to_string i) ^ ": " ^ (asm_to_string SEPARATOR) ^
                                    (asms_to_string cs) ^ "break" ^ (asm_to_string SEPARATOR)
           val alternatives = foldr (op ^) "" (map alternative ics)
           val default = "default: " ^ (asm_to_string SEPARATOR) ^ (asms_to_string def) ^ 
                         "break" ^ (asm_to_string SEPARATOR)
        in
           ("switch(" ^ (asm_to_string b) ^ ") " ^
            "{ " ^ alternatives ^
            default ^ " }")
        end
      | NEW_TAG_REF => "newtag"
      | EXCEPTION_HANDLER_REF => "exception_handler"
      | SEPARATOR => ";\n"
      | SET (d, t, b) => "efficient_set(" ^
                         (asm_to_string d) ^ ", " ^
                         "(unsigned long) " ^ (asm_to_string t) ^ ", " ^
                         (asm_to_string b) ^ ")"
      | COPY (d, s, b) => "efficient_copy(" ^
                          (asm_to_string d) ^ ", " ^
                          (asm_to_string s) ^ ", " ^
                          (asm_to_string b) ^ ")" ^ (asm_to_string SEPARATOR)
      | HALT => "return (0)" ^ (asm_to_string SEPARATOR)

  and asms_to_string a_s = foldr (op ^) "" (map asm_to_string a_s) 

  fun generateLabel l = "_" ^ (Variable.tostring l)

  fun print_function f (l, asm) =
    let
       val v = generateLabel l
    in
       TextIO.output (f, ("void * " ^ v ^ "()\n{\n"));
       TextIO.output (f, asms_to_string asm);
       TextIO.output (f, "}\n\n")
    end

  fun print_label_h f bs = 
    let 
       fun out (lab, asm) = 
         let 
            val v = generateLabel lab
         in
            TextIO.output (f, ("extern void *"  ^ v ^ "();\n"))
         end
    in
       app out bs
    end
        
  fun print_header_file f bs appname = 
    ( TextIO.output (f, "#ifndef __" ^ appname ^ "_H__\n");
      TextIO.output (f, "#define __" ^ appname ^ "_H__\n");
      TextIO.output (f, "\n");
      TextIO.output (f, "#ifdef __cplusplus\n");
      TextIO.output (f, "extern \"C\" {\n");
      TextIO.output (f, "#endif\n");
      TextIO.output (f, "\n");
      TextIO.output (f, "typedef struct {\n");
      TextIO.output (f, "   unsigned long (*availc)();\n");
      TextIO.output (f, "   unsigned long (*getc)();\n");
      TextIO.output (f, "   unsigned long (*putc)(unsigned long);\n");
      TextIO.output (f, "} IO_functions_type;\n");
      TextIO.output (f, "\n");
      TextIO.output (f, "void initializeIO(IO_functions_type *io_fns);\n");
      TextIO.output (f, "\n");
      print_label_h f bs;
      TextIO.output (f, "\n");
      TextIO.output (f, "#ifdef __cplusplus\n");
      TextIO.output (f, "}\n");
      TextIO.output (f, "#endif\n");
      TextIO.output (f, "\n");
      TextIO.output (f, "#endif /*__" ^ appname ^ "_H__*/\n");
      TextIO.flushOut f;
      TextIO.closeOut f )

  fun print_io_fns f =
      ( TextIO.output (f, "IO_functions_type io;\n");
        TextIO.output (f, "\n");
        TextIO.output (f, "void initializeIO(IO_functions_type *io_fns) {\n");
        TextIO.output (f, "   io.availc = io_fns->availc;\n");
        TextIO.output (f, "   io.getc = io_fns->getc;\n");
        TextIO.output (f, "   io.putc = io_fns->putc;\n");
        TextIO.output (f, "}\n");
        TextIO.output (f, "\n") )
        
  fun copy_file (infilename, outfilename) =
    let
       val f = TextIO.openIn infilename
       val s = TextIO.inputAll f
       val () = TextIO.closeIn f
       val ofile = TextIO.openOut outfilename
    in
       TextIO.output(ofile, s);
       TextIO.flushOut ofile;
       TextIO.closeOut ofile
    end
        
  fun print_basic appname (blocks, lab) =
    let
       val f = TextIO.openOut (appname ^ ".c")
       val f_h = TextIO.openOut (appname ^ ".h")
    in
       TextIO.output (f, "#include \"runtime-c.h\"\n");
       TextIO.output (f, "#include \"" ^ appname ^ ".h\"" ^ "\n\n");
       print_io_fns f;
       app (print_function f) blocks;
       TextIO.flushOut f;
       TextIO.closeOut f;
       print_header_file f_h blocks appname;
       app copy_file [( (Parse.root ^ "/runtime/runtime-c.h"), ("runtime-c.h") ),
                      ( (Parse.root ^ "/runtime/runtime-c.c"), ("runtime-c.c") ) ]
    end
        
  fun print out includemain runtime (blocks, lab) cflags ldflags target =
      let
          val path = "./output/"
          val () = OS.FileSys.mkDir path

          fun generateMainFile includemain =
              let
                  val entry = if includemain then "main" else "mlmain"
                  val file = TextIO.openOut (path ^ entry ^ ".c")
              in
                  TextIO.output (file, "#include \"labels.h\"\n");
                  TextIO.output (file, "int " ^ entry ^ "(int argc, char **argv)\n{\n");
                  TextIO.output (file, "   return engine();\n");
                  TextIO.output (file, "}\n");
                  TextIO.flushOut file;
                  TextIO.closeOut file
              end

                                 
        fun printblock (l, asm) = 
            let 
                val v = generateLabel l
                val f = TextIO.openOut (path ^ v ^ ".c")
            in
                TextIO.output (f, ("#include \"runtime-c.h\"\n"));
                TextIO.output (f, ("#include \"labels.h\"\n"));
                print_function f (l, asm);
                TextIO.flushOut f;
                TextIO.closeOut f
            end

        fun generateRuntimeFiles () = 
          map copy_file [( (Parse.root ^ "/runtime/runtime-c.h"), (path ^ "runtime-c.h") ),
                         ( (Parse.root ^ "/runtime/runtime-c.c"), (path ^ "runtime-c.c") ) ]

        fun generateLabelHeaderFile bs = 
            let 
               val f = TextIO.openOut (path ^ "labels.h");
            in
               print_label_h f bs;
               TextIO.flushOut f;
               TextIO.closeOut f
            end

        fun generateMakefile bs target = 
            let 
                fun genObjStr (lab, asm) = 
                    let 
                        val v = generateLabel lab
                    in
                        v ^ " "
                    end
                val os = foldr (op^) "" (map genObjStr bs)
		val target = if target = "" then target else (target ^ "-")
                val hdr = ( "CC := " ^ target ^ "gcc\n" ^
                            "CFLAGS := " ^ cflags ^ "\n" ^
                            "LDFLAGS := " ^ ldflags ^ "\n" ^
                            "AR := " ^ target ^ "ar\n" ^
                            "ARFLAGS := rS\n" ^
                            "RM := rm -f \n" ^
                            "RANLIB := " ^ target ^ "ranlib\n" ^
                            "OBJECTS := runtime-c main " ^ os ^ "\n" )
                val f = TextIO.openOut (path ^ "makefile");
                val () = TextIO.output (f, hdr);
                val () = TextIO.output (f, ("a.out: libarchive.a \n"));
                val () = TextIO.output (f, ("\t$(CC) $(CFLAGS) -L. -larchive $(LDFLAGS) -o $@ \n"));
                val () = TextIO.output (f, ("libarchive.a: \n"));
                val () = TextIO.output (f, ("\tmake -j 2 -f makefile.obj \n"));
                val () = TextIO.output (f, ("\tfind . -print | grep \"\\.o\" | xargs $(AR) $(ARFLAGS) libarchive.a\n"));
                val () = TextIO.output (f, ("\t$(RANLIB) libarchive.a\n"));
                val () = TextIO.flushOut f;
                val () = TextIO.closeOut f;
                val f = TextIO.openOut (path ^ "makefile.obj");
                val () = TextIO.output (f, hdr);
                val () = TextIO.output (f, ("all : $(addsuffix .o,$(OBJECTS)) ;\n"));
                val () = TextIO.output (f, ("\n"));
                val () = TextIO.output (f, ("%.o: %.c\n"));
                val () = TextIO.output (f, ("\t$(CC) $(CFLAGS) -c $^ -o $@\n"));
                val () = TextIO.flushOut f;
                val () = TextIO.closeOut f;
            in
                ()
            end

        val ordered_blocks = blocks (* rev(blocks) *)

      in
        generateRuntimeFiles ();
        generateLabelHeaderFile ordered_blocks;
        if includemain then generateMakefile ordered_blocks target else ();
        app printblock ordered_blocks;
        generateMainFile (includemain)
      end

end
