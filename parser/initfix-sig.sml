
(* initial fixity for identifiers *)

signature INITFIX =
sig
   
    val initial : (string * (int * Parsing.associativity)) list
end