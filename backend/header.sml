
structure Header =
struct

  val unique = Params.param "um"
    (SOME ("-fn",
           "For the C++ Header backend, give the function name to generate")) "fn"

  exception Header of string

  structure W = Word32

  fun write outfile prog =
    let
      
      val unique = !unique
      val UNIQUE = CharVector.map Char.toUpper unique

      fun to32 w8 = W.fromInt (Word8.toInt w8)

      fun progword () =
          case (prog (), prog (), prog (), prog ()) of
              (SOME a, SOME b, SOME c, SOME d) =>
                  SOME
                  (W.orb
                   (W.<<(to32 a, 0w24),
                    W.orb
                    (W.<<(to32 b, 0w16),
                     W.orb
                     (W.<<(to32 c, 0w8), to32 d))))
                      
            | (NONE,   NONE,   NONE,   NONE) => NONE
            | _ => raise Header "byte stream not multiple of four"

      val f = TextIO.openOut outfile

      fun save s =
        TextIO.output(f, s);

      val ct = ref 0;

    in
      save("#ifndef __" ^ UNIQUE ^ "_H\n" ^
            "#define __" ^ UNIQUE ^ "_H\n" ^
            " /* generated file! do not edit */\n" ^
            "typedef unsigned int uint;\n\n" ^ 
            "uint " ^ unique ^ "_data[] = {\n");

      SimpleStream.app (fn w =>
                        (save ("0x" ^ Word32.toString w ^ ", ");
                         ct := !ct + 1;
                         if !ct mod 8 = 0 then save "\n"
                         else ())) progword;
             
      save (" };\n");

      save ("\n\n /* UM interpreter */\n\n");

      save ("static uint * " ^ unique ^ "_ulloc(uint size) {\n");
      save "  uint * r = (uint*)calloc((1 + size), sizeof(uint));\n";
      save "  *r = size;\n";
      save "  return (r + 1);\n";
      save "}\n";
      save "\n";
      save ("static void " ^ unique ^ "_ufree(uint * p) {\n");
      save "  free(p - 1);\n";
      save "}\n";
      save "\n";
      save ("string " ^ unique ^ " (string input) {\n");
      save "  string output;\n";
      save "  uint inidx = 0;\n";
      save "  uint ip = 0; uint reg[8] = {0,0,0,0,0,0,0,0};\n";
      save "\n";
      save (" uint * zero = " ^ unique ^ "_ulloc(" ^ Int.toString(!ct) ^ ");\n");
      save "  /* initialize */\n";
      save "  for (unsigned int i = 0; i < zero[-1]; i ++) {\n";
      save ("    zero[i] = " ^ unique ^ "_data[i];\n");
      save "  }\n";
      save "\n";
      save "#  define arr(m) (m?(uint*)m:zero)\n";
      save "\n";
      save "#  define c w & 7\n";
      save "#  define b (w >> 3) & 7\n";
      save "#  define a (w >> 6) & 7\n";
      save "\n";
      save "  /* spin cycle */\n";
      save "  for(;;) {\n";
      save "    uint w = zero[ip++];\n";
      save "\n";
      save "    /*\n";
      save "    int c = w & 7;\n";
      save "    int b = (w >> 3) & 7;\n";
      save "    int a = (w >> 6) & 7;\n";
      save "    */\n";
      save "\n";
      save "    switch(w >> 28) {\n";
      save "    case 0: if (reg[c]) reg[a] = reg[b]; break;\n";
      save "    case 1: reg[a] = arr(reg[b])[reg[c]]; break;\n";
      save "    case 2: arr(reg[a])[reg[b]] = reg[c]; break;\n";
      save "    case 3: reg[a] = reg[b] + reg[c]; break; \n";
      save "    case 4: reg[a] = reg[b] * reg[c]; break;\n";
      save "    case 5: reg[a] = reg[b] / reg[c]; break;\n";
      save "    case 6: reg[a] = ~(reg[b] & reg[c]); break;\n";
      save "        /* XXX space leak: should free the mem we\n";
      save "           allocated (but didn't free) */\n";
      save "    case 7: return output;\n";
      save ("    case 8: reg[b] = (uint)" ^ unique ^ "_ulloc(reg[c]); break;\n");
      save ("    case 9: " ^ unique ^ "_ufree((uint*)reg[c]); break;\n");
      save "    case 10: output += (char)reg[c]; break;\n";
      save "    case 11: if (inidx == input.length ()) reg[c] = 0xFFFFFFFF;\n";
      save "             else reg[c] = (uint)input[inidx++]; break;\n";
      save "    case 12:\n";
      save "      if (reg[b]) {\n";
      save ("        " ^ unique ^ "_ufree(zero);\n");
      save "        int size = ((uint*)reg[b])[-1];\n";
      save ("        zero = " ^ unique ^ "_ulloc(size);\n");
      save "        memcpy(zero, (uint*)reg[b], size * 4);\n";
      save "      }\n";
      save "      ip = reg[c]; \n";
      save "      break;\n";
      save "    case 13: reg[7 & (w >> 25)] = w & 0177777777; break;\n";
      save "    }\n";
      save "  }\n";
      save "}\n";
      save "\n";
      save "      \n";
      save "\n";

      save ("#endif\n");
      TextIO.closeOut f
    end
  

end
