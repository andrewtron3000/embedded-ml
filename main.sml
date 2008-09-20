structure QuietDownNJ = struct end

val outf = Params.param ""
    (SOME ("-o",
           "Name of bytecode output (relative to input file dir)")) "outf"

val _ =
    case Params.docommandline () of
        [input] => OS.Process.exit(Compile.compile input (!outf))
      | _ =>
            let in
                print "Usage: mlc file.uml\n\n";
                print (Params.usage ())
            end
