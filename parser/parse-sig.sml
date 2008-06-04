
signature PARSE =
sig
    exception Parse of string
    
    (* root of the humlock build directory *)
    val root : string

    (* expression parser *)
    val exp : (string * (int * Parsing.associativity)) list -> 
                   (EL.exp_ * Pos.pos,Tokens.token) Parsing.parser

end
