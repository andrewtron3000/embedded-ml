
structure Initfix :> INITFIX =
struct

    val initial =
        [("<", (2, Parsing.Non)),
         ("<=", (2, Parsing.Non)),
         (">", (2, Parsing.Non)),
         (">=", (2, Parsing.Non)),
         (* array bounds check *)
         ("chk", (2, Parsing.Non)),

         ("<>", (2, Parsing.Non)),

         ("::", (9, Parsing.Right)),
         ("^", (3, Parsing.Left)),

         (":=", (1, Parsing.Non)),

         ("+", (4, Parsing.Left)),
         ("-", (4, Parsing.Left)),
         ("*", (5, Parsing.Left)),

         (* ?? *)
         ("andb", (5, Parsing.Left)),
         ("orb",  (5, Parsing.Left)),
         ("xorb", (5, Parsing.Left)),
         ("shl",   (5, Parsing.Non)),
         ("shr",   (5, Parsing.Non))
	 ]

(*         ("div", (5, Parsing.Left)),
           ("mod", (5, Parsing.Left))] *)

end
