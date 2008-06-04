
(* rewrites a CPS term so that every variable binding is unique. *)

structure CPSAlpha =
struct

  exception Alpha of string
  
  open CPS

  structure V = Variable
  structure VM = Variable.Map
  
  (* G is a set of bound variables.
       if we see one of these rebound, we need to rename it.
     R is map of variables to variables.
       if we see one of these (used) in the domain, we replace
       with R(v). *)
  fun alpha G R c =
    let
      fun rename v =
        case VM.find (R, v) of
          SOME v' => v'
        | NONE => v

      fun value (Var v) = Var (rename v)
        | value (Label l) = raise Alpha "alpha post closure-conversion??"
        | value (Int i) = Int i

      fun dobinds (G, R, acc, nil) = (G, R, rev acc)
        | dobinds (G, R, acc, h :: t) =
        (case VM.find (G, h) of
           NONE => dobinds (VM.insert(G, h, ()), R, h :: acc, t)
         | SOME () => 
             (* shadowing! *)
             let 
               val h' = V.alphavary h
             in
               (* print ("alpha/SHADOWED: " ^ V.tostring h ^ " -> " ^ V.tostring h' ^ "\n"); *)
               dobinds (VM.insert(G, h', ()),
                        VM.insert(R, h, h'),
                        h' :: acc,
                        t)
             end)

      (* normal case *)
      fun alph vas binds k =
        let
          val vas = map value vas
          val (G, R, binds) = dobinds (G, R, nil, binds)
          val re = alpha G R
        in
          k (vas, binds, re)
        end

    in
      case c of
        App(va, vl) => App (value va, map value vl)
      | Alloc (t, vas, v, ce) => 
          alph vas [v] (fn (vas, [v], re) =>
                        Alloc(t, vas, v, re ce))
      | Project(i, va, v, ce) => 
          alph [va] [v] (fn ([va], [v], re) =>
                         Project(i, va, v, re ce))

      | Fix (vael, ce) =>
          let
            val (G, R, fs) = dobinds (G, R, nil, map #1 vael)
            fun onefun (f, (_, args, e)) =
              let
                val (G, R, args) = dobinds (G, R, nil, args)
              in
                (f, args, alpha G R e)
              end
          in
            Fix(map onefun (ListPair.zip (fs, vael)), alpha G R ce)
          end

      | Intswitch (va, iel, def) =>
          alph [va] []
             (fn ([va], [], re) =>
              Intswitch(va, ListUtil.mapsecond re iel, re def))

      | Sumswitch (va, n, v, icl, def) => 
          alph [va] [v]
             (fn ([va], [v], re) =>
              Sumswitch(va, n, v, ListUtil.mapsecond re icl, re def))

      | Primop (po, vas, vs, ces) => 
          alph vas vs
             (fn (vas, vs, re) =>
              Primop(po, vas, vs, map re ces))

      | Deferred osf => 
             let val os = osf ()
             in
               case Util.Oneshot.deref os of
                 NONE => (* (Util.Oneshot.wrap (alpha G R) os;
                             Deferred (fn () => os)) *)
                         raise Alpha "oneshot unset in alpha conversion"
               | SOME ce => alpha G R ce
             end
    end


  fun alphavary ce = alpha VM.empty VM.empty ce

end