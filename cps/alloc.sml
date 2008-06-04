
(* possible addition: rewrite an alloc of an empty
   tuple to just reuse a value that we've allocated
   recently. (We should restrict this to small stuff
   like integers to not keep large data live for gc.) 
   Empty tuples won't ever be destructed, so that's 
   safe. *)

structure CPSAlloc :> CPSALLOC =
struct

    exception CPSAlloc of string
    open CPS
    open Primop
    structure V = Variable

    structure Constant : ORD_KEY =
    struct
        type ord_key = value
            
        fun compare (Int a, Int b) = Word32.compare (a, b)
          | compare (Label a, Label b) = V.compare (a, b)
          (* | compare (String a, String b) = String.compare (a, b) *)
          | compare (Var _, _) = 
            raise CPSAlloc "BUG: vars not constant (l)"
          | compare (_, Var _) = 
                raise CPSAlloc "BUG: vars not constant (r)"
          | compare (Int _, _) = GREATER
          | compare (_, Int _) = LESS
(*
          | compare (Label _, _) = GREATER
          | compare (_, Label _) = LESS
*)
    end

    structure D = SplayMapFn(Constant)

    fun gettag (Var _) = raise CPSAlloc "impossible"
      | gettag (Int _) = INT
      (* | gettag (String _) = STRING *)
      | gettag (Label _) = CODE

    fun rewrite d cexp =
        let

            (* allocate the value va, if necessary, and pass a
               variable holding it (and a new dictionary) to j. *)
            fun one d va j =
                case va of
                     Var v => j d v
                   | _ => 
                     (case D.find (d, va) of
                          (* already allocated *)
                          SOME v => j d v
                          (* need to allocate small value *)
                        | _ => let val nv = V.namedvar "alloc"
                                   val d = D.insert(d, va, nv)
                               in
                                   Alloc(gettag va, [va],
                                         nv, j d nv)
                               end)
            and list d (l : value list) j =
                let fun g d (va :: rest) acc = 
                           one d va (fn d => 
                                     fn v => g d rest (v::acc))
                      | g d nil acc = j d (rev acc)
                in g d l nil
                end
        in 

            case cexp of
                Project (i, vl, va, cexp) =>
                    (* projection should never have a constant *)
                    Project(i, vl, va, rewrite d cexp)
              | Fix (vvcl, cexp) => 
                    Fix(map (fn (va, vl, ce) => 
                             (va, vl, rewrite d ce)) vvcl,
                        rewrite d cexp)
              | App (va, vl) =>
                    (* leave the value in function position untouched *)
                    list d vl (fn _ => fn vs => App(va, map Var vs))

              (* don't touch small allocations. *)
              (* XXX could put them in the dictionary.
                 optimizer should have ensured that vas is not a 
                 variable itself *)
              | Alloc (INT, vas, v, ce) => Alloc(INT, vas, v, rewrite d ce)
              | Alloc (STRING s, vas, v, ce) => 
                    Alloc(STRING s, vas, v, rewrite d ce)
              | Alloc (CODE, vas, v, ce) => 
                    Alloc(CODE, vas, v, rewrite d ce)

              | Alloc (tag, vas, v, ce) =>
                    list d vas (fn d => fn vass => 
                                Alloc(tag, map Var vass, v, rewrite d ce))

              | Intswitch (va, icl, ce) =>
                    (* constant folding should have already taken care
                       of anything clever that we could do here... *)
                    one d va (fn d => fn v => 
                              Intswitch(Var v,
                                        map (fn (i,c) => 
                                             (i, rewrite d c)) icl,
                                        rewrite d ce))
              | Sumswitch (va, num, vr, icl, ce) =>
                    one d va (fn d => fn v => 
                              Sumswitch(Var v, num, vr,
                                        map (fn (i,c) => 
                                             (i, rewrite d c)) icl,
                                        rewrite d ce))
              (* we allow some operands to primops to be literal
                 integers. *)
              | Primop (PSub, [varr, Int idx], vs, [c]) => 
                    Primop(PSub, [varr, Int idx], vs, [rewrite d c])

              | Primop (PPutc p, [Int ch], vs, [c]) =>
                    Primop(PPutc p, [Int ch], vs, [rewrite d c])

              (* although this should have been optimized out... *)
              | Primop (PNotb, [Int i], vs, [c]) =>
                    Primop(PNotb, [Int i], vs, [rewrite d c])

              | Primop (B bop, [va, vb], vs, c) => 
                    Primop(B bop, [va, vb], vs, map (rewrite d) c)

              | Primop (PArray, [Int sz, va], vs, [c]) => 
                    one d va (fn d => fn var =>
                              Primop (PArray, [Int sz, Var var],
                                      vs, [rewrite d c]))

              | Primop (PUpdate, [varr, Int idx, obj], vs, [c]) =>
                    (* Assume varr is not small (by typing);
                       don't allocate the idx, but do allocate
                       the object since it's escaping. *)
                    one d obj (fn d => fn var =>
                               Primop (PUpdate, [varr, Int idx, Var var],
                                       vs, [rewrite d c]))
              (* general primop *)
              | Primop (po, vas, vrs, ces) =>
                    list d vas (fn d => fn vs => 
                                Primop(po, map Var vs, vrs,
                                       map (rewrite d) ces))

              | Deferred ceo => (case Util.Oneshot.deref (ceo ()) of
                                     NONE => raise CPSAlloc "unset oneshot"
                                   | SOME ce => rewrite d ce)
        end

    fun convert ce = rewrite D.empty ce

end
