
structure Primop =
struct

    datatype compare =
        PEq
      | PNeq
        (* Signed comparison *)
      | PLess
      | PLesseq
      | PGreater
      | PGreatereq
      (* array bounds check *)
      | PBChk

    (* things of int * int -> _ *)
    datatype binop =
        PTimes
      | PPlus
      | PMinus
      | PDiv
      | PSDiv
      | PAndb
      | PXorb
      | POrb
      | PShl
      | PShr
      | PCmp of compare

    datatype port = Console | Serial0

    datatype primop =
      (* primitive arithmetic stuff *)
        B of binop
      | PNeg

      (* string concatenation: string * string -> string *)
(*      | PConcat *)

      (* non-int compares *)
      | PEqs

      | PNotb

      (* is it zero? *)
      | PNull (* two branches: yes-null/not-null *)

      (* references: can compile these as 1-length arrays *)
      | PSet
      | PGet
      | PRef

      (* arrays and vectors use these *)
      | PArray
      | PArray0
      | PSub
      | PUpdate
      | PArraylength

      | PJointext

      | PPrint

      (* no-op, just bind *)
      | PBind

      (* no-op; just remove variables from the current
        scope. *)
      | PKill

      (* I/O for both UM (console) and SCIP (console + serial) *)
      | PPutc of port
      | PGetc of port
      | PAvail of port (* non-blocking, avail bytes to read *)

      (* I/O for SCIP only *)
      | PSetStatus (* eight LEDs on SCIP *)
      | PSetTestpoint (* 8 pin testpoint on external (B1) connector of SCIP *)
      | PSetCounter (* four hexadecimal digits on SCIP *)

      (* Concurrency support in SCIP only *)
      | PFromSeconds (* convert from seconds to clock ticks *)
      | PSleep

      (* generate an exception tag, sequentially *)
      | PNewtag
      (* store and retrieve exception handler fn *)
      | PSethandler
      | PGethandler

      (* no run-time effect; just produces compiletime
         warning if not dead *)
      | PCompileWarn of string

      (* read from dynamic region *)
      | PDynamic

      (* invoke a native function with the given name *)
      | PNative of string

      (* Done *)
      | PHalt

      (* DEBUGGING! *)
      | PShowval

    type cpsprimop = primop

    (* nb: all operations are UNSIGNED, including comparisons *)
    (* XXX this should probably be factored out *)
    fun tostring (B PTimes) = "Times"
      | tostring (B PPlus) = "Plus"
      | tostring (B PMinus) = "Minus"
      | tostring (B PDiv) = "Div"
      | tostring (B PSDiv) = "SDiv"
      | tostring (B (PCmp PEq)) = "Eq"
      | tostring (B (PCmp PNeq)) = "Neq"
      | tostring (B (PCmp PLess)) = "Less"
      | tostring (B (PCmp PLesseq)) = "Lesseq"
      | tostring (B (PCmp PGreater)) = "Greater"
      | tostring (B (PCmp PGreatereq)) = "Greatereq"
      | tostring (B (PCmp PBChk)) = "BChk"

      | tostring (PNull) = "Null"

      | tostring (B PAndb) = "Andb"
      | tostring (B PXorb) = "Xorb"
      | tostring (B POrb) =  "Orb"
      | tostring (B PShl) = "Shl"
      | tostring (B PShr) = "Shr"

      | tostring PNotb = "Notb"

      | tostring PEqs = "Eqs"

      | tostring PPrint = "Print"

      | tostring PNeg = "Neg"

      | tostring PSet = "Set"
      | tostring PGet = "Get"
      | tostring PRef = "Ref"

(*      | tostring PConcat = "Concat" *)

      | tostring PArray = "Array"
      | tostring PArray0 = "Array0"
      | tostring PSub = "Sub"
      | tostring PUpdate = "Update"
      | tostring PArraylength = "Arraylength"
      | tostring PJointext = "Jointext"

      | tostring PBind = "Bind"
      | tostring PKill = "Kill"
      | tostring PNewtag = "Newtag"
      | tostring PGethandler = "Gethandler"
      | tostring PSethandler = "Sethandler"

      | tostring (PCompileWarn s) = "Warn(" ^ s ^ ")"
      | tostring PDynamic = "Dynamic"

      | tostring (PPutc Console) = "Putc"
      | tostring (PGetc Console) = "Getc"
      | tostring (PAvail Console) = "AvailC0"

      | tostring (PPutc Serial0) = "PutcS0"
      | tostring (PGetc Serial0) = "GetcS0"
      | tostring (PAvail Serial0) = "AvailS0"

      | tostring PSetStatus = "SetStatus"
      | tostring PSetTestpoint = "SetTestpoint"
      | tostring PSetCounter = "SetCounter"

      | tostring PFromSeconds = "FromSeconds"
      | tostring PSleep = "Sleep"

      | tostring (PNative f) = "Native(" ^ f ^ ")"

      | tostring PHalt = "Halt"

      | tostring PShowval = "(DEBUG:ShowVal)"

end
