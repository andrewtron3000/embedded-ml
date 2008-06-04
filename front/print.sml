
(* XXX this is crap. use layout *)

structure ELPrint =
struct

  local open EL
      fun nspaces j = StringUtil.tabulate j #" "
      val itos = Int.toString
  in

      fun ptos (PVar s) = s
        | ptos PWild = "_"
        | ptos (PAs (s, p)) = "(" ^ s ^  " as " ^ ptos p ^ ")"
        | ptos (PRecord spl) = 
          "{" ^ StringUtil.delimit ", " (map (fn (s, p) =>
                                              s ^ " = " ^
                                              ptos p) spl) ^ "}"
        | ptos (PConstrain (p, t)) = ptos p ^ " : t" (* XXX *)
        | ptos (PConstant _) = "constant" (* XXX *)
        | ptos (PApp (s, SOME p)) = "(" ^ s ^ " " ^ ptos p ^ ")"
        | ptos (PApp (s, NONE)) = "(" ^ s ^ " (-none-))"
        | ptos (PWhen (e, p)) = ptos p ^ " when (" ^ etosi 0 e ^ ")"

      and etosi i (e, _) =
          (case e of
               Var s => s
             | Let (d, e) => "let\n" ^ nspaces (i + 3) ^ 
                             dtosi (i + 3) d ^ "\n" ^
                             nspaces i ^ "in\n" ^ nspaces (i + 3) ^ 
                             etosi (i + 3) e ^ "\n" ^ nspaces i ^ "end"
             | (Constant (CInt i)) => "0x" ^ Word32.toString i
             | (Constant (CChar c)) => "CHR '" ^ implode[c] ^ "'"
             | _ => "??")

      and dtosi i (d, _) =
          (case d of
               Do e => "do " ^ etosi (i + 3) e
             | Val (tv, p, e) => 
                   "val " ^
                   (case tv of
                        nil => ""
                      | [t] => t ^ " "
                      | _ => "(" ^ StringUtil.delimit ", " tv ^ ")") ^
                   ptos p ^ " = " ^ etosi (i + 10) e
             | _ => "?d?")

  end

end
