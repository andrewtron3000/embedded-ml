
signature TOKENIZE =
sig
    (* Parser for tokens *)
    val token : (Tokens.token * Pos.pos,char) Parsing.parser

end