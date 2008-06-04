
(* Interface to the Hemlock compiler. *)

signature COMPILE =
sig

    (* compile source progname
       
       takes source file and produces binary. 
       progname should not be a path, rather, a base filename.
       this name is used for the name of the cordcode (prog.tar.gz)
       and client (prog)
       *)
    val compile : string -> string -> OS.Process.status


    (* ok to ignore these, which are for testing *)
    val tokenize : string -> Tokens.token list

    val getel : string -> EL.exp

end
