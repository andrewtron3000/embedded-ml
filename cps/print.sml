(* "pretty" printer for CPS terms. *)

(* Layout is probably not necessary here since CPS terms are mostly
   "linear" *)

(* XXX however, this is slow because, for one thing, I use ^ all the time
   instead of concat. porting to layout might make sense in that light. *)

structure CPSPrint :> CPSPRINT =
struct

    structure V = Variable
    open CPS
    open Primop

   fun nspaces j = StringUtil.tabulate j #" "

    fun vtos (Var v) = V.tostring v
      | vtos (Label l) = "*" ^ V.tostring l
      | vtos (Int i) = "0x" ^ Word32.toString i
      (* | vtos (String s) = "\"" ^ String.toString s ^ "\"" *)

    fun ttos INT = "INT"
      | ttos (STRING s) = ("STRING \"" ^ String.toString s ^ "\"")
      | ttos CODE = "CODE"
      | ttos REF = "REF"
      (* int * t. Used to tag object language sums. *)
      | ttos (INT_T n) = "INT_" ^ Int.toString n ^ "_T"
      (* tuple of the supplied length. Limited to ??? *)
      | ttos (TUPLE i) = "TUPLE_" ^ Int.toString i

    fun etosil i (App (v, vl)) = 
           nspaces i ^ 
           vtos v ^ " [" ^ (StringUtil.delimit ", " (map vtos vl)) ^ "]" :: nil
      | etosil i (Alloc (t, vas, v, rest)) =
           nspaces i ^ 
           V.tostring v ^ " = (" ^ ttos t ^ " | " ^ 
               (StringUtil.delimit ", " (map vtos vas)) ^ ")"
           :: etosil i rest
      | etosil i (Project (j, vl, vr, rest)) =
           nspaces i ^ V.tostring vr ^ " = #" ^ Int.toString j ^ 
           " " ^ vtos vl
           :: etosil i rest
      | etosil i (Fix ([], rest)) =
           nspaces i ^ "fix (no functions)" ::
           etosil i rest
      | etosil i (Fix (vael, rest)) =
           List.concat
           (ListUtil.mapi
            (fn ((v, a, e), n) =>
             (nspaces i ^ (if n = 0 then "fix" else "and") ^ " " ^
              V.tostring v ^ "(" ^ StringUtil.delimit ", "
              (map V.tostring a) ^ ") as") ::
             etosil (i + 4) e) vael)
           @ etosil i rest
      | etosil i (Primop (PBind, [vl], [vr], [c])) =
           nspaces i ^
           V.tostring vr ^ " = " ^ vtos vl ::
           etosil i c
      | etosil i (Primop (po, vls, vrs, cs)) =
           nspaces i ^
           (if length vrs > 0 
            then if length vrs = 1 then V.tostring (hd vrs) ^ " = "
                 else "(" ^ StringUtil.delimit ", " (map V.tostring vrs) 
                    ^ ") = "
            else "") ^ Primop.tostring po ^ " (" ^
                StringUtil.delimit ", " (map vtos vls) ^ ")" ::

                List.concat
                (if length cs <> 1 
                 then ListUtil.mapi (fn (c, n) =>
                                     if n > 0 then
                                        nspaces (i + 2) ^ "or" ::
                                        etosil (i + 4) c
                                     else etosil (i + 4) c) cs
                 else [etosil i (hd cs)])
      | etosil i (Deferred os) =
           (case Util.Oneshot.deref (os()) of
                NONE => [nspaces i ^ "(XXX DEFERRED)"]
              | SOME e => etosil i e)
      | etosil i (Sumswitch(va, n, vr, icl, def)) = 
          (nspaces i ^
             "sumswitch[" ^ Int.toString n ^ "] " ^ 
                V.tostring vr ^ " = " ^ vtos va ^ " of") ::
                List.concat
                 (map (fn (j, c) =>
                       nspaces (i + 2) ^ Int.toString j ^ " =>" ::
                       etosil (i + 8) c) icl) @
             (nspaces (i + 2) ^ "_ =>" :: etosil (i + 8) def)
      | etosil _ _ = "XXX intswitch" :: nil

    fun etosi i e = String.concat (map (fn s => s ^ "\n") (etosil i e))

    fun printe e = app (fn s => print (s ^ "\n")) (etosil 0 e)
    fun writee fname e =
      let
        val l = etosil 0 e
        val f = TextIO.openOut fname
      in
        app (fn s => TextIO.output(f, s ^ "\n")) l;
        TextIO.closeOut f
      end

end