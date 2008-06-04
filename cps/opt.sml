
(* XXX to add:

   - source of bugs: if the same variable is bound in the same
     scope, the renamer gets confused, here. Whenever something
     is bound, we should take it out of the substitution, (or
     just raise an error if it is in the substitution).. for
     now I am trying to avoid duplicate vars. (See alphavary)

   - constant folding for conditionals, (sumcase!)
     etc.

   - argument flattening. This is very common. Look to see if a
     non-escaping function is always called directly with a tuple. If
     so, rewrite call sites to send the tuple components instead, and
     make the function prologue rebuild the tuple inside. (Usually
     the function just projects out the fields, so then those would
     become known projections and the allocation dead.) The closure
     converter has to be fixed to handle >2 arguments if this happens,
     but that's not too hard. Long argument lists can make things worse
     later, but lots of allocations do, too.
     
   - re-alloc of a tuple in the same scope. This would be best suited
     to post closure-conversion. There's no need to allocate, say,
     unit, several times in the same scope. (This probably should be
     limited to a narrow scope so that we don't add unit as a free
     variable of almost every function.)

   - cse?

   - More agressive inlining. (Use "size" of body:
                               For instance, if a body is just
                               an application, inlining will
                               almost certainly be a win
                               (unless the argument list is
                               a gillion arguments long))
     ... allow inlining without deleting the definition, ie,
     for datatype constructors that also escape.

   - Constant argument removal. If a function is always called with
     the same argument (this is common with continuation arguments),
     erase it. The hard thing about this is respecting the scope of
     that variable (the function might not be in its scope, though if
     all call sites are, then the function could be moved within the
     scope of the var, too -- and, especially, the variable may look
     the same in both calls but actually be two different vars.)

*)
structure CPSOpt :> CPSOPT =
struct

  val debugopt = Params.flag false
      (SOME ("-debugopt", 
             "Debug the CPS optimizer")) "debugcpsopt"

  val execcps = Params.flag false
      (SOME ("-execcps",
             "Try interpreting the CPS language at various phases, for debugging")) "execcps"

  (* PERF these should all be ON when not bug-hunting! *)

  val cps_inline = Params.flag true
      (SOME ("-cpsinline",
             "Inline in the CPS phase")) "cpsinline"
      
  val cps_drop = Params.flag true
      (SOME ("-cpsdrop",
             "Drop unused args in CPS phase")) "cpsdrop"

  val cps_halt = Params.flag true
      (SOME ("-cpshalt",
             "Optimize HALT in CPS phase")) "cpshalt"

  val cps_eta = Params.flag true
      (SOME ("-cpseta",
             "Optimize ETA in CPS phase")) "cpseta"

  val cps_deadalloc = ref true
  val cps_subst = ref true

  datatype score = 
    Infinite
  | Finite of int

  infix ++
  fun Infinite ++ n = Infinite
    | (Finite x) ++ n = Finite(x + n)

  infix +-+
  fun Infinite +-+ _ = Infinite
    | _ +-+ Infinite = Infinite
    | (Finite x) +-+ (Finite y) = Finite (x + y)

  fun scoretos Infinite = "infinite"
    | scoretos (Finite n) = Int.toString n

  fun debugdo f = if !debugopt then f () else ()
  fun dprint s = if !debugopt then print ("[CPSOPT] " ^ s ^ "\n") else ()
  fun dprintd f = if !debugopt then print ("[CPSOPT] " ^ f() ^ "\n") else ()

  (* bottom-up optimizations.
      * dead var elimination
      * simple eta-reductions
      * constant folding
      * constant propagation
      * unused argument elimination
      * beta reductions of functions called exactly once
        (ie, inlining)

     (DVE) dead var elimination. If at a binding site we see that a
     variable is unused, and the operation producing the binding is
     not effectful, remove the binding.

     (ETA) When we see a function declaration, if the body of the function
     is of the form App(f, args) where args is the same as the list of
     arguments to our function, then erase that function and replace it
     with f everywhere. (Some side conditions apply.)

     (INLINE) If at a function declaration we see that a function is
     called directly, and its number of free occurrences is exactly
     1, and it is not recursive, we can inline the application of
     that function. We can also inline if we have other reason to
     suspect that this will help us without hurting code size. We
     carry out the inlining by passing Inline f to the function
     returned with the call site.

     (CF) Constant Folding. If the expression supports constant
     folding, check to see if the subexpressions are constants, and
     reduce. This applies to some primops as well as intcase. In
     intcase, if the case object is a variable and that variable
     appears inside any of the arms, we can substitute the actual
     known integer value of that variable. To support constant
     folding in CPS form, we also do constant propagation. 

     (CP) When we see a binding where the right-hand-side is a simple
     constant, we replace every occurrence of that variable with the
     constant and erase the binding.

     (DROP) Drop unused arguments to functions if we know all of
     the call sites. This does work even on recursive calls to
     itself, but not transitively--if two functions call each
     other passing an unused arg back and forth, it will be
     retained.

     (AFLAT) Flatten arguments to functions if called with
     tuples and we know all the call sites.

     (KNOWN) When we create a tuple, record its fields so that
     projections from it (while it is in scope) can be done
     statically.

     (INJ) When we create an injection, record its tag and
     contents so that case analyses on it (while it is in
     scope) can be done statically.

     (SPLIT) A fairly complex inlining scenario. A function is a
     "splitter(x, n)" if x is an argument to the function, and the
     body immediately sumcase-analyzes that argument with 'n'
     different branches (not including the default). If a
     splitter(x,n) function does not escape, and is called in k
     places, where for each call site the actual parameter in position
     'x' is a known-inj with tag Ti, and each Ti is distinct, 
     and k <= n + 1, then we can conservatively inline the function.
     This does not cause expansion of code, because the (INJ) optimization
     will reduce each sumcase to a different branch from the original
     function body.
     This optimization is intended to specifically catch the
     case where we write       if e1 < e2 then e3 else e4
     in the source language.

     (PULL) attempts to pull out Fix expressions to their maximal
     scope. This transformation is only applied if the function is
     non-escaping, or else we may move closure creations into traces
     where they would not otherwise be executed. (That would not be
     conservative, though it is not clear whether it would be an
     overall win or not.)

     *)

  exception CPSOpt of string
  structure V = Variable
  structure O = Util.Oneshot
  structure OU = CPSOptUtil
  open CPS
  open Primop

  val itos = Int.toString

  fun chd s nil = raise CPSOpt s
    | chd _ (h::t) = h

  (* (INLINE) (DROP) (AFLAT)
     during analysis,
     each function application site will be
     replaced with a rewriting function. 
     The callsite can be told to do one of
     the following three actions: *)
  datatype rewrite_call = 
      Inline of V.var * V.var list * cexp
    | Remargs of int list
    | Flatten of flat list
    | Don't

  (* (AFLAT)
     when flattening, we describe each 
     argument position with a 'flat'. 
     
     If an argument is AsIs, that means
     to do nothing. If it is AsTuple ln
     then the argument should be flattened
     to n consecutive arguments. *)
  and flat = AsIs | AsTuple of int

  (* knowledge about the value of a variable.
     used for (KNOWN) and (INJ) and (AFLAT) *)
  datatype knowledge =
      Ktuple of value list
    | Kinj of int * value option
(*
    | Kstring of string
*)

  (* information about call-sites *)
  type callsite =
    { (* function to rewrite call site (must be called) *)
      rewrite : rewrite_call -> unit,
      (* (SPLIT); what is known about the actual arguments
         at this call site. *)
      callwith : knowledge option list }

  (* augment a set of 'seen' variables.
     increment the ones that are already there,
     add the ones that weren't. *)
  fun augset s vl =
      let
          fun one (Var v, s) =
              (let val (ss, n) = V.Map.remove (s, v)
               in V.Map.insert (ss, v, n + 1)
               end handle LibBase.NotFound => V.Map.insert(s, v, 1))
            | one (_, s) = s
      in
          foldl one s vl
      end

  fun subtract s vl =
      let
          fun one (v, s) = let val (ss, _) = V.Map.remove (s, v)
                           in ss
                           end handle LibBase.NotFound => s
      in
          foldl one s vl
      end


  fun dropi dr a =
      let
          fun go nil _ = nil
            | go (h::t) n = if List.exists (fn x => x = n) dr 
                            then go t (n + 1) 
                            else h :: go t (n + 1)
      in
          go a 0
      end

  (* union two fv maps *)
  fun union s1 s2 = V.Map.unionWith op+ (s1, s2)

  fun unionc s1 s2 = V.Map.unionWith op@ (s1, s2)

  fun addcall s f g =
      let val (ss, l) = V.Map.remove (s, f)
      in V.Map.insert (ss, f, g :: l)
      end handle LibBase.NotFound => V.Map.insert (s, f, [g])

  fun takecalls s vl =
      foldl (fn (x, (s, l)) => 
             let val (ss, i) = V.Map.remove (s, x)
             in (ss, (x,i)::l)
             end handle LibBase.NotFound => (s, l)) (s, nil) vl

  fun getvar (Var v) = v
    | getvar _ = raise CPSOpt "expected var, got somethin' else"

  fun increment why 0 (a, b, c, d) = (a, b, c, d)
    | increment why n (a, b, c, d : score) =
    let in
      dprintd (fn () => "score +" ^ Int.toString n ^ " -> " ^ scoretos (d ++ n) ^ 
               " for " ^ why ^ "\n");
      (a, b, c, d ++ n)
    end

  fun increments why (Finite 0) (a, b, c, d) = (a, b, c, d)
    | increments why n (a, b, c, d) =
    let in
(*      dprintd (fn () => "score +" ^ Int.toString n ^ " -> " ^ Int.toString (d + n) ^ 
               " for " ^ why ^ "\n"); *)
      (a, b, c, d +-+ n)
    end

  fun fvtos s =
      V.Map.foldli (fn (v, n, s) => 
                    s ^ " " ^ V.tostring v ^ ":" ^ itos n) "" s

  (* once tried to do eta reduction, but that didn't work because
     all call sites (ie, the App) are deferred on the way back. *)
  fun makefix (vael, e) = (Fix(vael, e), Finite 0)

  (* every time I inline a function, I should alphavary its
     arguments so that I don't get duplicate variables in the
     same scope. *)
  (* FIXME: needs to alpha vary any bindings within the body, too. *)
  fun alphavary (args, body) =
      let
          val l = map (fn a => (a, V.alphavary a)) args
          val subst = foldl (fn ((a, aa), m) =>
                             V.Map.insert (m, a, Var aa))
                            V.Map.empty l

          val nbody = CPSSubst.alphavary subst body
      in
(* (* broken: forces unset oneshot - print should handle exns? *)
          debugdo (fn () =>
                   (print "alphavary: \n";
                    print (CPSPrint.etosi 3 body);
                    print "to:\n";
                    print (CPSPrint.etosi 3 nbody)));
*)                    

          debugdo (fn () =>
                   app (fn (old, new) =>
                        print ("  " ^ V.tostring old ^ " -> " ^
                               V.tostring new ^ "\n")) l);

          (map #2 l, nbody)
      end

  (* used in order to write the result of primop translations briefly *)
(*
 (* don't use this, since it doesn't correctly track
    bound vars, etc. if k does something *)
  fun ethrough s kn n vs e k =
      let val (ee, fv, ca, m) = bottomup s kn e
      in (k ee, subtract fv [vs], ca, m + n)
      end
*)

  fun einstead s kn n e =
      let val (ee, fv, ca, m) = bottomup s kn e
      in (ee, fv, ca, m ++ n)
      end

  (* should do 'noeffect' dropping here *)
  and doprimop s kn (Primop(po, vas, vrs, rests)) =
      let
          fun dosub (va as (Var v)) = 
              (case V.Map.find (s, v) of
                   NONE => va
                 | SOME n => n)
            | dosub va = va

          val nvas = map dosub vas

          val used = augset V.Map.empty nvas

          val (rests, fv, calls, amount) =
              foldr (fn ((e,f,c,n), (es,fs,cs,nn)) =>
                     (e :: es, union f fs, unionc c cs, n +-+ nn)) 
              (nil, used, 
               V.Map.empty, Finite 0) 
              (map (bottomup s kn) rests)

      in
          dprintd (fn () => "Primop(" ^ Primop.tostring po ^ ")...");
          dprintd (fn () => "Free vars: " ^ fvtos fv);
          (case (OU.noeffect po, rests) of
             (true, [cont]) =>
                 (* if it doesn't bind any variables we need... *)
                 if List.all (fn v => not (Option.isSome 
                                           (V.Map.find (fv, v)))) vrs
                 then (* erase *) increment "noeffect erase" 
                                   (5 * (1 + length nvas))
                                  (cont, fv, calls, amount)
                 else (Primop(po, nvas, vrs, rests),
                       subtract fv vrs, calls, amount)
           | (true, _) => raise 
                 CPSOpt ("if primop is noeffect (" ^ Primop.tostring po ^ 
                         "), it must have exactly 1 cont")
           | _ => (Primop(po, nvas, vrs, rests), 
                   subtract fv vrs, calls, amount))
      end
    | doprimop _ _ _ = raise CPSOpt "doprimop: impossible"

  (* the main optimization pass. 

     bottomup subst known e

     subst is a substitution to apply, known is a map of bound
     tuples and injections, and e is the expression to simplify.

      - a new simplified and substituted expression
      - its set of free variables (mapped to the number of times used)
      - a list of functions called directly with the information on
          each call site.
      - "amount" of simplification done. If this is a positive
          integer, then the program has made progress towards an
          irreducible state. We use this to decide whether to do
          another optimization pass.

          *)
  and bottomup (s : value V.Map.map) (kn : knowledge V.Map.map) 
               (e : CPS.cexp) : 
               (* optimized expression *)
               cexp * 
               (* number of free vars *)
               int V.Map.map *
               (* direct calls for a function *)
               callsite list V.Map.map *
               (* simplification amount *)
               score

               =
    let 
      fun dosub (va as (Var v)) = 
        (case V.Map.find (s, v) of
             NONE => va
           | SOME n => n)
          | dosub va = va
      fun dofun vael rest =
        let
          fun folder ((v, a, e), (l, amt)) = 
              let val (ee, fv, ca, nn) = bottomup s kn e
              in ((v, a, ee, fv, ca) :: l, nn +-+ amt)
              end

          val (fs, done) = foldr folder (nil, Finite 0) vael
          (* names of fns defined in this fix *)
          val friends = map #1 fs

          val _ = dprintd (fn () => "Friends are: " ^ 
                           StringUtil.delimit ", "
                           (map V.tostring friends))

          (* separate the mutually recursive components
             into different fixes. XXX This could be a
             bit better by doing a topological sort of
             dependencies -- right now, if f calls g
             then they will be in the same fix, even if
             g doesn't call f back. The rest of the code
             is already set to handle this, just give it
             a list of (lists of actually
             mutually-recursive functions) in dependency
             order. *)

          (* start with a list of singletons, and union
             any lists that refer to each other. Should
             use maps and a union/find structure for
             this; right now it's quadratic in the worst
             case. But who has hundreds of
             mutually-recursively defined functions? *)

          val uf = map ListUtil.list friends

(*          val _ = dprint ("initial length uf = " ^ itos (length uf)); *)

          (* Get the set in which f resides (and the remainder). 
             It must be in some set. *)
          fun get l f =
              case List.partition (fn m => 
                                   List.exists (fn x => 
                                                V.eq(x, f)) m) l of
                  ([a], l) => (a, l)
                | _ => raise CPSOpt "get: impossible!"

          (* In the partition ll, union the sets containing f and g *)
          fun bundleunion ll f g =
              let
                  val _ = dprintd (fn () => "union " ^ V.tostring f ^ 
                                   " and " ^ V.tostring g)
                  val (fs, newll) = get ll f
              in
                  if List.exists (fn z => V.eq(z, g)) fs 
                  then ll (* already unioned *)
                  else let val (gs, newerll) = get newll g
                       in (fs @ gs) :: newerll
                       end
              end

          fun coalesce nil l = l
            | coalesce ((h, _, _, fv, _)::t) l =
              (* for each free variable in h that's in friends,
                 union those two sets. *)
              let 
                  (* friends free in body *)
                  val freefr = 
                      List.filter (fn (x,_) => List.exists 
                                    (fn y => V.eq(x, y)) friends)
                                  (V.Map.listItemsi fv)

                  val _ = dprintd (fn () => "Free friends are: " ^ 
                                   StringUtil.delimit ", "
                                   (map V.tostring friends))
              in
                  coalesce t
                    (foldl (fn ((g,_), u) => bundleunion u h g) l freefr)
              end

          val uf = coalesce fs uf

(*          val _ = dprint ("coalesced uf: " ^ itos (length uf))*)
          val uf = map (map (fn f => 
                             chd "uf" (List.filter (fn (g, _, _, _, _) => 
                                                    V.eq(g, f)) fs))) uf

          (* recurse on rest *)
          val (reste, fvs, calls, amount) = bottomup s kn rest

          (* increase done if we split up any functions *)
          val done = amount +-+ done ++ ((length uf - 1) * 10)
        in
          null uf andalso raise CPSOpt "bug: uf was empty";
(*          dprint ("length uf = " ^ itos (length uf)); *)

          if length uf > 1 
          (* I only want to do inlining and dead code
             elimination when I'm looking at an
             individual bundle (It is a pain to look for
             calls and appearances in later bundles). So
             if I've split these up, because they don't
             call each other, I'll wait for the next
             pass to do anything. But I still need to
             call all of the functions for the call
             sites. *)
          then 
              let

                  val allcalls = 
                      foldl (fn (fs,cs) => foldl (fn ((_, _, _, _, a), b) => 
                                                  unionc a b) cs fs) calls uf
                  val allfvs = 
                      foldl (fn (fs,vs) => 
                             foldl (fn ((_, args, _, a, _), b) => 
                                    union (subtract a args) b) vs fs) fvs uf

                  (* if a call is to one of these functions, say 
                     Don't inline and subtract from set. Note these 
                     call sites might be in the function bodies! *)

                  val (newcalls, these) = takecalls allcalls friends
                  val _ = app (fn (_, fl) => 
                               app (fn {rewrite, ... } => rewrite Don't) fl) 
                              these

                  (* if a free variable is one of these functions, 
                     don't make free. *)

                  val newfv = subtract allfvs friends

                  (* make nested fix *)
                  fun donest nil = reste
                    | donest (fs::r) =
                      Fix(map (fn (a, b, c, _, _) => (a, b, c)) fs, donest r)
              in
                  (donest uf, newfv, newcalls, done)
              end
          else 
            (* there's just one bundle now. We'll only inline
               it if there is a single function in that bundle,
               however. *)
              let val fns = chd "fns" uf
              in 
                  (* check if this is inlinable *)
                  if length fns = 1 
                      andalso 
                      let val (f, args, e, fv, cl) = chd "fns2" fns

                          val noccs = (case V.Map.find(fvs, f) of
                                            SOME n => n
                                          | NONE => 0)

                          val thiscalls = (case V.Map.find(calls, f) of
                                            SOME l => l
                                          | NONE => nil)

                          val ncalls = length thiscalls

                          (* (SPLIT) *)
                          exception No
                          val cansplit =
                            (case e of
                               Sumswitch(Var x, num, _, iel, _) =>
                                 (* must be an argument *)
                                 (case ListUtil.position
                                    (fn y => V.eq (x, y)) args of
                                    NONE => false
                                  | SOME pos =>
                                      let
                                      (* knowledge for split var *)
                                        fun kfsv {rewrite=_, callwith=l} =
                                          (case List.nth (l, pos) of
                                             SOME (Kinj (i, _)) => i
                                           | _ => raise No)
                                             handle Subscript =>
                                               raise CPSOpt
                                                 ("bad cps: " ^
                                                  "call site didn't have " ^
                                                  "enough args")
                                        
                                        val kvs = map kfsv thiscalls


                                        (* XXX. This condition is not
                                           required for safety, just to 
                                           ensure that inlining doesn't
                                           increase the size of the program.
                                           
                                           One thing we could do if a branch
                                           is repeated is to hoist that
                                           branch as a function itself. It's
                                           not clear if this ever actually
                                           happens, however. *)
                                        fun disjoint l =
                                          let
                                            val l =
                                              ListUtil.sort Int.compare l
                                          in
                                            ListUtil.alladjacent op<> l
                                          end

                                      in
                                        (* must be no more call sites
                                           than branches *)
                                        length iel + 1 >= ncalls
                                        andalso disjoint kvs
                                      end)
                             | _ => false) handle No => false
                            


                          val recursive = 
                            isSome (V.Map.find(fv, f))
                      in  
                        debugdo (fn () =>
                          if cansplit andalso (ncalls = noccs)
                             andalso (not recursive)
                          then dprint ("CAN SPLIT " ^ V.tostring f ^ "!"
                                       ^ "(ncalls = " ^ Int.toString
                                       (length thiscalls) ^ ")\n")
                          else ());

                          (* we might use a gentler set of criteria here; 
                             this only allows inlining if there is exactly 
                             1 call. *)
                          
                          debugdo (fn () =>
                                   (dprint (V.tostring f ^ 
                                            " is a candidate for inlining...");
                                    dprint ("     FV inside: " ^ fvtos fv);
                                    dprint ("     FV after: " ^ fvtos fvs)));

                          (* if it's recursive, inlining is not
                             conservative (and this code might not
                             do it right) *)
                          !cps_inline
                          andalso
                          not recursive
                          (* must not escape -- all occurrences are
                             accounted for by call sites *)
                          andalso ncalls = noccs
                          andalso 
                          (* exactly one direct call *)
                          (ncalls = 1
                           orelse
                           (* (SPLIT) *)
                           cansplit
                           orelse
                           (* looks like ctor *)
                           false
                           (* XXX non-conservative and wrong -- duplicating
                           code without alpha-varying is incorrect... some
                           things look like constructors, but aren't!
                           (case e of
                              Alloc(INT_T _, _, _, ce) =>
                                (* this is not necessarily conservative...
                                   but the bad cases are much rarer
                                   than what we miss by not inlining! *)
                                (case ce of
                                     Deferred _ => true
                                   | App _ => true
                                   | _ => false)
                            | _ => false)
                           *))
                      end
                  then 
                      let (* if inlined, we know it has this form *)
                          val (f, a, e, fv, cl) = chd "fns3" fns

                          (* wrap the body in a Deferred. This will make
                             alpha renaming lazy when it reaches this point.
                             In the case of (SPLIT), this is an important
                             optimization; otherwise the program size
                             can grow exponentially as sharing is destroyed
                             during the alpha-vary pass. *)

                          (* XXX this doesn't work, because we end up forcing
                             the function in each place before we get to the
                             embedded sumswitch. for now, only running alphavary
                             before the first optimization pass. *)
                          (* val e = Deferred (fn () => Util.Oneshot.init e) *)

                          (* do inlining *)
                          val _ = 
                              case V.Map.find (calls, f) of
                                  SOME cs => 
                                      app (fn {rewrite=c, ...} =>
                                           c (Inline (f, a, e))) cs
                                | NONE => () (* raise CPSOpt "inlining???" *)

                          (* better remove the call(s) we inlined at *)
                          val (calls, these) = takecalls calls [f] 
                      (* XXX could use 'these' instead of searching
                         map above *)
                      in
                          (* still need to add its free variables and
                             calls, which now appear at the inlining
                             site(s). Set amount + 1000, because we
                             definitely want to do another pass to
                             clean up the inlining sites. *)

                          (reste, union fvs (subtract fv a), 
                           unionc calls cl, if length these > 0
                                            then Infinite
                                            else amount)
                      end
                  else
                   (* can't inline. If none of the functions are
                      ever called (we don't care if they call each
                      other), then we can erase this binding,
                      otherwise we're done! *)

                   let
                     val appears = 
                         foldl (fn ((f, _, _, _, _), appeared) =>
                                appeared orelse 
                                isSome(V.Map.find(fvs, f))) false fns
                   in
                     if appears
                     then
                       let
                           (* this is similar to the n-ary case above. *)
                           val allcalls = 
                               foldl (fn ((_, _, _, _, a), b) => 
                                      unionc a b) calls fns

                           val allfvs = 
                               foldl (fn ((_, args, _, a, _), b) => 
                                      union (subtract a args) b) fvs fns

                           (* If all the call sites are known, we
                              can do stuff like (DROP) and (AFLAT). *)
                           (* (DROP) *)
                           fun dropargs (all as (f, a, body, fv, ca), 
                                         (ac, l, amt)) =
                             if (* is every argument free in body? *)
                                List.all (fn v => 
                                          isSome (V.Map.find (fv, v))) a 
                                (* or is cps_drop disabled? *)
                                orelse not (!cps_drop)
                             then (ac, all :: l, amt)
                             else 
                                let
                                    (* not all used. *)
                                    val _ = dprintd 
                                        (fn () => V.tostring f ^ 
                                         " is a candidate for " ^
                                         "argument modification")

                                    val (restcalls, calls) = takecalls ac [f]

                                    val calls = 
                                        getOpt(ListUtil.Alist.find 
                                                  V.eq calls f, 
                                               nil)

                                    val used = getOpt(V.Map.find (allfvs, f), 0)
                                in
                                 case Int.compare(length calls, used) of
                                     GREATER => raise CPSOpt 
                                         "impossible: more calls than used"

                                   (* some unknown *)
                                   | LESS => (ac, all :: l, amt)

                                   | EQUAL =>
                                      (* all known callsites -- do it! *)
                                      (* start by trying to drop unused arguments. *)
                                      let
                                          (* generate list of unused args. *)
                                          fun gu nil _ = nil
                                            | gu (h::t) n =
                                              if isSome (V.Map.find (fv, h))
                                              then gu t (n + 1)
                                              else n :: gu t (n + 1)
                                          val drops = gu a 0
                                      in
                                          dprintd (fn () => "Removing args from " ^
                                                  V.tostring f 
                                                  ^ "!");

                                          app (fn {rewrite=j, callwith} => 
                                               j (Remargs drops)) calls;

                                          (restcalls, 
                                           (f, dropi drops a, 
                                            body, fv, ca)::l, 
                                           amt ++ 100)
                                      end
                                end

                           exception ChangeOurMinds

                           (* (AFLAT) *)
                           fun flatargs (all as (f, a, body, fv, ca), 
                                         (ac, l, amt)) =
                             let
                               val (restcalls, calls) = takecalls ac [f]
                                 
                               val calls = 
                                 getOpt(ListUtil.Alist.find 
                                        V.eq calls f, 
                                        nil)
                                 
                               val used = getOpt(V.Map.find (allfvs, f), 0)
                             in
                               case Int.compare(length calls, used) of
                                 GREATER => raise CPSOpt 
                                   "impossible: more calls than used/aflat"
                                   
                               (* some unknown *)
                               | LESS => (ac, all :: l, amt)
                                   
                               | EQUAL =>
                                   (* all known callsites -- do it! *)
                                   (* this pass is driven by calls (where we
                                      know whether things are records), not
                                      the function itself (where we might
                                      have some information, like projections,
                                      but no way to know the length of records). *)
                                   let
                                     val _ = calls : {callwith : knowledge option list,
                                                      rewrite : rewrite_call -> unit} list

                                     (* this part is kind of like pattern compilation.
                                        first form a matrix

                                        arg11 ... arg1n  => rewrite_1
                                               .
                                               :
                                        argm1 ... argmn  => rewrite_m

                                        where each arg has type knowledge option.

                                        *)
                                     val (matrix, rws) = 
                                       (* also ensures matrix is not ragged *)
                                       ListPair.unzip (map (fn {callwith, rewrite} => (callwith, rewrite)) calls)

                                     val matrix = ListUtil.transpose matrix
                                       
                                     (* if ALL arguments in a column are (SOME
                                        (Ktuple t)) then we can flatten that
                                        column. (Note that we can't flatten
                                        unless we know them ALL--the CPS
                                        language sometimes represents a datatype
                                        (say, list) as both () and (a,b).. so
                                        this would be unsafe.
                                        
                                        Note that because knowledge is not
                                        recursive, once we have split a column
                                        once, we will have to wait for the next
                                        pass of optimization to do further
                                        flattening on that column.

                                        we do this by mapping each column to a 
                                        "flat" datatype. *)

                                     fun docol l =
                                       (* find out if we know anything... *)
                                       (case l of
                                          SOME(Ktuple t) :: rest => 
                                            if List.all (fn (SOME(Ktuple t')) => length t = length t'
                                                          | _ => false) rest
                                            then AsTuple (length t)
                                            else AsIs
                                        | _ => AsIs)

                                     val flats = map docol matrix

                                   in
                                     (* are we flattening anything? *)
                                     if List.exists (fn (AsTuple _) => true | _ => false) flats
                                     then
                                       let
                                         val () = dprintd 
                                           (fn () => "flattening args for " ^ V.tostring f ^
                                            "(old args: " ^ StringUtil.delimit ", " (map V.tostring a) ^
                                            ")")

                                         val flatarg = Flatten flats
                                         val () = app (fn r => r flatarg) rws

                                         (* should this be fatal? *)
                                         val () = if length flats <> length a
                                                  then raise ChangeOurMinds
                                                  else ()
                                                    
                                         val fa = ListPair.zip (flats, a)
                                                    
                                         fun mkargsbody (nil, a_acc, bod) = (rev a_acc, bod)
                                           | mkargsbody ((AsIs, a) :: rest, a_acc, bod) =
                                           mkargsbody (rest, a :: a_acc, bod)
                                           | mkargsbody ((AsTuple n, a) :: rest, a_acc, bod) =
                                           let
                                             val components =
                                               List.tabulate (n, fn x => V.namedvar ("flat_" ^
                                                                                     Int.toString x))

                                             val bod =
                                               Alloc(TUPLE n, map Var components, a, bod)
                                           in
                                             mkargsbody(rest, rev components @ a_acc, bod)
                                           end

                                         val (args, body) = mkargsbody (fa, nil, body)
                                       in
                                         
                                         (* calls to other functions *)
                                         (restcalls, 
                                          (* this new function *)
                                          (f, 
                                           args, 
                                           body, 
                                           fv, ca)::l, 
                                          (* better run again, since we didn't actually
                                             erase the known tuple allocations or allocation
                                             within the function *)
                                          Infinite)
                                       end
                                     else (ac, all :: l, amt)
                                   end
                             end handle ChangeOurMinds => (ac, all :: l, amt)

                           val (newcalls, fs, adrop) = 
                               foldr dropargs (allcalls, nil, Finite 0) fns

                           val (newcalls, fs, aflat) =
                               foldr flatargs (newcalls, nil, Finite 0) fs

                           (* send Don't to anything left *)
                           val (newcalls, these) = takecalls newcalls friends

                           val _ = 
                               app (fn (_, fl) => 
                                    app (fn {rewrite, ...} => 
                                         rewrite Don't) fl) these

                           (* if a free variable is one of these functions,
                              don't make free. *)

                           val newfv = subtract allfvs friends

                           val (ee, d) = makefix 
                               (map (fn (a, b, c, _, _) => (a, b, c)) fs, 
                                reste)
                       in
                         increments "drop/flat/d" (d +-+ adrop +-+ aflat)
                           (ee, newfv, newcalls, done)
                       end

                     (* really easy -- the variables aren't free, and we 
                        don't need to instantiate the call sites (if
                        there are any, they're in the function
                        bodies), so just return the continuation! *)

                     else increment "unused-fun" 50 (reste, fvs, calls, amount)
                   end

              end
        end
             : cexp * 
               (* number of free vars *)
               int V.Map.map *
               (* direct calls for a function *)
               callsite list V.Map.map *
               (* simplification amount *)
               score


      fun powtwo 0w0 = 0w1
        | powtwo n = 0w2 * powtwo (n - 0w1)

    in
     case e of
       Deferred os => 
          (case O.deref (os()) of
               NONE => 
                   raise CPSOpt "Found unset Deferred expression in Optimize!"
             | SOME e => bottomup s kn e)
      (* XXX if I want to run this on closure-converted code, need to look for
         App (Label v, vl) as well. *)

     | App (v, vl) =>
          let
              val v = dosub v
              val vl = map dosub vl
              val fv = augset V.Map.empty (v :: vl)
              val os = O.oneshot () : cexp O.oneshot

              fun rewrite_site (Inline (f, args, body)) =
                  (* just create the bindings; the next
                     pass will take care of substitution and
                     further simplification. *)
                  let
                      val (args, body) = alphavary (args, body)

                      fun re nil nil = body
                        | re (formal::rest) (actual::more) =
                          Primop(PBind, [actual], [formal], [re rest more])
                        | re _ _ =
                          raise CPSOpt ("when inlining " ^ V.tostring 
                                        f ^ ", function called " ^
                                        "on the wrong number of args")
                  in
                      dprintd (fn () => "Inlining " ^ V.tostring f ^ "!");
                      O.set (os, re args vl)
                  end

                | rewrite_site Don't = O.set (os, App(v, vl))

                  (* (DROP) *)
                | rewrite_site (Remargs dr) = O.set (os, App(v, dropi dr vl))

                  (* (AFLAT) *)
                | rewrite_site (Flatten fl) = 
                  let
                    val () = dprintd (fn () => ("doing flattening for call to " ^
                                                CPSPrint.vtos v ^ "..."))

                    (* compute new vl. *)
                    fun mkargs (nil, nil, acc, k) = (rev acc, k)
                      | mkargs (AsIs :: rest, h :: t, acc, k) = mkargs (rest, t, h :: acc, k)
                      | mkargs (AsTuple n :: rest, Var h :: t, acc, k) = 
                      (* need to generate n arguments, each of which is a projection from h *)
                      let val nsvars = List.tabulate(n, fn x =>
                                                     (x, V.namedvar (V.basename h ^
                                                                     "#" ^ Int.toString x)))
                      in
                        (* PERF: should use known info here instead of waiting for two more passes... *)
                        mkargs (rest, t, rev (map (Var o #2) nsvars) @ acc,
                                foldl (fn ((num, var), k) =>
                                       fn e => Project(num, Var h, var, k e)) k nsvars)
                      end
                      | mkargs (_ :: _, nil, _, _) = raise CPSOpt "more flatargs than real args"
                      | mkargs (nil, _ :: _, _, _) = raise CPSOpt "more real args than flatargs" 
                      | mkargs _ = raise CPSOpt "bad cps; flattening but not var arg"

                    val (args, k) = mkargs(fl, vl, nil, fn x => x)
                  in
                    dprintd (fn () => ("Args were: " ^ StringUtil.delimit ", " (map CPSPrint.vtos vl)));
                    dprintd (fn () => ("Args  are: " ^ StringUtil.delimit ", " (map CPSPrint.vtos args)));
                    O.set (os, k (App(v, args)))
                  end

              (* (SPLIT): record knowledge about the actual arguments *)
              (* n.b. could easily be recording more information here,
                 in order to do constant argument removal, etc. *)
              fun onearg (Var x) = V.Map.find (kn, x)
                | onearg _ = NONE

              val calls = addcall V.Map.empty (getvar v) 
                { rewrite = rewrite_site, callwith = map onearg vl }
          in
              (Deferred (fn () => os), fv, calls, Finite 0)
          end
    (* rewrite bindings with substitutions *)
    | p as Primop(PBind, [va], [vr], [rest]) =>
          if !cps_subst
          then
            increment "bind" 5 (bottomup (V.Map.insert (s, vr, dosub va)) kn rest)
          else doprimop s kn p

    (* since we track known tuples (and NULL is a tuple) we can sometimes
       reduce these cases *)
    | p as Primop(PNull, [va], [], [z, nz]) =>
          (case dosub va of
             Var target => 
               (case V.Map.find (kn, target) of
                  NONE => doprimop s kn p
                (* (KNOWN) nullcase on known tuple: can do dead code elim *)
                | SOME (Ktuple nil) => increment "null" 100 (bottomup s kn z)
                | SOME (Ktuple (_ :: _)) => increment "null" 100 (bottomup s kn nz)
                | SOME _ => raise CPSOpt "bad cps: pnull on known non-tuple/null")
           | _ => doprimop s kn p)

    | Primop(PNull, _, _, _) => raise CPSOpt "bad cps: pnull"

    (* can't really optimize halt, but we know it 
       doesn't use its argument, and doesn't return. so empty these. *)
    | p as Primop(PHalt, args, results, rests) => 
       if !cps_halt
       then
             increment "phalt" (length args + length results + (length rests * 100))
          (Primop(PHalt, [], [], []), V.Map.empty, V.Map.empty, Finite 0)
       else doprimop s kn p

(*
    (* can do a lot of optimization here when strings are known. *)
    | Primop(PJointext, args, [v], [rest]) =>
         let

           datatype str =
             STR of string * V.var
           | VAR of V.var
           | NEWSTR of string           

           val p = ref 0

           fun joinflatten nil = nil
             | joinflatten (STR ("", _) :: rest) =
             joinflatten (p := !p + 10; rest)
             | joinflatten (STR (s, _) :: STR (ss, _) :: rest) =
             joinflatten (p := !p + 40; NEWSTR (s ^ ss) :: rest)
             (* newstr can show up on the left *)
             | joinflatten (NEWSTR s, :: STR (ss, _) :: rest) =
             joinflatten (p := !p + 40; NEWSTR (s ^ ss) :: rest)

             | joinflatten (e :: rest) =
             e :: joinflatten rest

           fun onearg (Var v) =
             (case V.Map.find (kn, v) of
                SOME (Kstring s) => STR (s, v)
              | SOME _ => raise CPSOpt "jointext known non-string"
              | NONE => VAR v)
             | onearg _ = raise CPSOpt "jointext non-var!"
                
           val args = map onearg args

           val args = joinflatten args

           fun genterm (acc, nil) =
             Primop(PJointext, acc, 
         in
           
         end
*)

    (* similarly, certain primops return unit, which is almost never
       used. Separate the allocation of unit out so it can usually
       be erased. *)
    | Primop(PPutc p, args, [v], [rest]) => 
          increment "putc" 5 (bottomup s kn (Primop(PPutc p, args, [], [Alloc(TUPLE 0, [], v, rest)])))
    | Primop(PSethandler, args, [v], [rest]) => 
          increment "sethandler" 5 (bottomup s kn (Primop(PSethandler, args, [], [Alloc(TUPLE 0, [], v, rest)])))
    | Primop(PUpdate, args, [v], [rest]) => 
          increment "update" 5 (bottomup s kn (Primop(PUpdate, args, [], [Alloc(TUPLE 0, [], v, rest)])))
    | Primop(PSet, args, [v], [rest]) => 
          increment "set" 5 (bottomup s kn (Primop(PSet, args, [], [Alloc(TUPLE 0, [], v, rest)])))

    | Primop(PCompileWarn ws, args, [v], [rest]) => 
          increment "compilewarn" 5 (bottomup s kn (Primop(PCompileWarn ws, args, [],
                                             [Alloc(TUPLE 0, [], v, rest)])))

    (* various arithmetic identities *)
    | Primop(B PDiv, [Int 0w0, _], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [Int 0w0], [vr], [rest]))
    | Primop(B PSDiv, [Int 0w0, _], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [Int 0w0], [vr], [rest]))
    | Primop(B PTimes, [Int 0w0, _], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [Int 0w0], [vr], [rest]))
    | Primop(B PTimes, [_, Int 0w0], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [Int 0w0], [vr], [rest]))
    | Primop(B PPlus, [va, Int 0w0], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [va], [vr], [rest]))
    | Primop(B PPlus, [Int 0w0, va], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [va], [vr], [rest]))
    | Primop(B PTimes, [va, Int 0w1], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [va], [vr], [rest]))
    | Primop(B PTimes, [Int 0w1, va], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [va], [vr], [rest]))
    | Primop(B PMinus, [va, Int 0w0], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [va], [vr], [rest]))

    (* nb. can overflow, but we are using words *)
    | p as Primop(PNeg, [Int a], [vr], [rest]) =>
          einstead s kn 10 (Primop(PBind, [Int (~a)], [vr], [rest]))

    (* unlike many architectures, UM is better at mul/div than shifting *)
    | p as Primop(B PShr, [va, Int factor], [vr], [rest]) =>
          einstead s kn 10 (Primop(B PDiv, [va, Int (powtwo (Word32.andb(factor, 0w31)))], [vr], [rest]))

    | p as Primop(B PShl, [va, Int factor], [vr], [rest]) =>
          einstead s kn 10 (Primop(B PTimes, [va, Int (powtwo (Word32.andb(factor, 0w31)))], [vr], [rest]))

    | p as Primop(B po, [Int a, Int b], [vr], [rest]) =>
    (* two constants: should be an arithmetic primop *)
          (case OU.evalints po a b of
               SOME va => einstead s kn 10 (Primop(PBind, [va], [vr], [rest]))
             | NONE => doprimop s kn p)

    | p as Primop(B (PCmp po), [Int a, Int b], [], [tt, ff]) => 
    (* two continuations: a comparison operation *)
          let val (ee, fv, ca, m) = if OU.evalcmp po a b
                                    then bottomup s kn tt
                                    else bottomup s kn ff
          in increment "known_cmp" 100 (ee, fv, ca, m)
          end

    (* same vars are always equal *)
    | p as Primop(B (PCmp PEq), [Var a, Var b], [], [tt, _]) =>
          if V.eq (a, b)
          then einstead s kn 100 tt
          else doprimop s kn p

    (* and never inequal *)
    | p as Primop(B (PCmp PNeq), [Var a, Var b], [], [_, ff]) => 
          if V.eq (a, b)
          then einstead s kn 100 ff
          else doprimop s kn p

    (* General case *)
    | p as Primop _ => doprimop s kn p
    (* The only thing we do for allocations is erase them (they are 
       not effectful) if the result is never used. It might be profitable
       to attempt to do the following further optimizations:
        - reuse older records in scope with exactly the same data
        XXX add: if a record is created as the eta-expansion of another
        record, just substitute. Ie:
        r = (TUPLE_3 | #0 s, #1 s, #2 s)
          (and length of s = 3)

       ... all of these seem to work better in a top-down pass.

       *)
    (* (CP) don't allocate ints. The alloc phase
       later will create them at the last minute, if needed. *)
    | Alloc(INT, [Int i], v, rest) => 
          increment "alloc-int" 50 (bottomup s kn
                        (Primop(PBind, [Int i], [v], [rest])))

    | Alloc(tag, vas, v, rest) => 
          let
              val nvas = map dosub vas
              (* (KNOWN) (INJ)
                 add this tuple as known while it is 
                 in scope *)
              val newkn = 
                  (case (tag, nvas) of
                       (TUPLE _, _) => V.Map.insert (kn, v, Ktuple nvas)
                     | (INT_T i, [nva]) => V.Map.insert (kn, v, Kinj (i, SOME nva))
                     | (INT_T i, nil) => V.Map.insert (kn, v, Kinj (i, NONE))
                     | (INT_T _, _) => raise CPSOpt "bad cps: alloc int_t multiple"
                     | _ => kn)
              val (re, fv, calls, a) = bottomup s newkn rest
          in
            (* dead? erase it. *)
            if !cps_deadalloc 
               andalso
               (case V.Map.find (fv, v) of
                  NONE => true
                | _ => false)
            then increment "alloc/dead" 50 (re, fv, calls, a)
            else (Alloc(tag, nvas, v, re), 
                  augset (subtract fv [v]) nvas, calls, a)
          end

    (* Erase unused projections and carry out projections from
       known tuples. *)
    | Project(i, va, vr, rest) => 
        let
          fun regular nva =
              let
                  val (re, fv, calls, a) = bottomup s kn rest
              in
                  case V.Map.find (fv, vr) of
                      (* dead? erase it. *)
                      NONE => increment "project/dead" 50 (re, fv, calls, a)
                    | _ => (Project(i, nva, vr, re), 
                            augset (subtract fv [vr]) [nva], 
                            calls, a)
              end
        in
          case dosub va of
            Var target => 
              (case V.Map.find (kn, target) of
                   NONE => regular (Var target)
                 (* (KNOWN) projection from known tuple *)
                 (* XXX note: it is possible for us to generate code like this: 
                    x = Null
                    if Null x 
                    then ... don't use x ...
                    else ... #1/2 x ...

                    from the SUMREP_OPTION optimization in tocps. The #1/2 is
                    unreachable, but this code will be confused when it sees
                    it. for now we rely on the primop optimization for PNull
                    to reduce this before we ever encounter the situation. *)
                 | SOME (Ktuple vals) =>
                     (case (SOME (List.nth (vals, i))) handle _ => NONE of
                        SOME va =>
                          let in
                            dprintd (fn () => V.tostring vr ^ " is # from known tuple");
                            increment "known-proj" 50
                            (bottomup s kn 
                             (Primop(PBind, [va], [vr], [rest])))
                          end
                      | NONE =>
                          let in
                            
                            print ("WARNING: projection index " ^ Int.toString i ^
                                   " too high for " ^ V.tostring target ^ " = " ^
                                   "(" ^ StringUtil.delimit ", " (map CPSPrint.vtos vals) ^
                                   ")\n");
                            print "(code crashes along this branch; replacing with HALT.)\n";
                            raise CPSOpt "just kidding, this is fatal";
                            increment "XXXX" 50
                            (bottomup s kn
                             (Primop(PHalt, [], [], [])))
                          end)

                 | SOME _ => 
                   raise CPSOpt ("bad cps: projection from " ^
                                 V.tostring target
                                 ^ " known value non-tuple"))
          | _ => raise CPSOpt 
                 "Bad CPS: projection from non-variable"
        end

    | Sumswitch (va, n, v, iel, def) =>
        (case dosub va of
           nva as (Var target) => 
             (case V.Map.find (kn, target) of
                NONE =>
                  let 
                    (* PERF (KNOWN): within each branch, could
                       know the case object variable is Kinj(branch, v) *)
                    (* val _ = print "unknown sum\n" *)
                    fun folder ((i, e), (l, fvs, calls, a)) =
                      let val (ee, fvv, cc, aa) = bottomup s kn e
                      in ((i,ee)::l, union fvs (subtract fvv [v]), 
                          unionc calls cc, aa +-+ a)
                      end
                    val (ndef, dfv, dc, da) = bottomup s kn def
                    val (niel, fvs, cls, amt) = 
                      foldr folder (nil, subtract dfv [v], dc, da) iel
                  in (Sumswitch (nva, n, v, niel, ndef), augset fvs [nva], cls, amt)
                  end
              | SOME (Kinj (i, vao)) => 
                  (* (INJ) analysis of known injection. we can reduce
                     in place. *)
                  let 
                    (* if not found, then use default *)
                    fun findb nil = def
                      | findb ((i', e) :: t) = 
                      if i = i'
                      then e
                      else findb t

                    val branch = findb iel
                  in
                    dprintd (fn () => "KNOWN CASE: " ^ V.tostring target ^ " = " ^
                            Int.toString i ^ "\n");
                    dprintd (fn () => V.tostring v ^ " case analysis of known inj");
                    (* potentially drops a lot of code, so should
                       definitely run again *)
                    increment "known inj" 1000
                    (bottomup s kn 
                     (* carrier or not carrier? 
                        if not carrier, we get NULL
                        *)
                     (case vao of
                        SOME va => Primop(PBind, [va], [v], [branch])
                      | NONE => Alloc(TUPLE 0, [], v, branch)))
                  end

              | SOME _ => raise CPSOpt ("bad cps: case analysis of " ^
                                        V.tostring target 
                                        ^ " known value non-tuple"))
         | _ => raise CPSOpt ("bad cps: sumcase analysis of non-var"))

    | Intswitch (va, iel, def) =>
          let
              val nva = dosub va
          in
              case nva of
                  (* do constant folding *)
                  Int i =>
                      (case ListUtil.Alist.find op= iel i of
                           (* not in list, use default *)
                           NONE => increment "intswitch-def" 50 (bottomup s kn def)
                           (* in list, use it. *)
                         | SOME e => increment "intswitch" 50 (bottomup s kn e))
                | Var v =>
                      (* can't fold this one. But if we enter any
                         of the arms, then we can substitute that
                         integer for this variable within the body.
                         This can cause nested switches to be folded
                         away. *)
                      let
                          fun folder ((i, e), (l, fvs, calls, a)) =
                              let val (ee, fvv, cc, aa) = 
                                  bottomup (V.Map.insert (s, v, Int i)) kn e
                              in ((i,ee)::l, union fvs fvv, 
                                  unionc calls cc, a +-+ aa)
                              end

                          val (ndef, dfv, dc, da : score) = bottomup s kn def
                          val (niel, fvs, cls, amt) = 
                              foldr folder (nil, dfv, dc, da) iel
                      in
                          (Intswitch (nva, niel, ndef), 
                           augset fvs [nva], cls, amt)
                      end
                | _ => raise CPSOpt "Bad CPS: intswitch on non-var/int"
          end
    | Fix (nil, rest) =>
          increment "empty fix" 5 (bottomup s kn rest)

    (* (ETA) Eta-reduce a common but specific form:
       Fix f (args) as
         g (args)            where f =/= g
         (side condition f \notin args not necessary because
          the formal args shadow f.)
         *)

    (* at this stage, all applications should be with Vars *)
    | Fix (vael as [(f,formal,App(Var g,actual))], rest) => 
          if !cps_eta andalso
             (not (V.eq (f, g))
              andalso ListUtil.all2 
                     (fn (fo, Var ac) => V.eq(fo,ac)
                        | _ => false) formal actual)
          then (* treat it as substitution f = g *)
              let in
                  dprintd (fn () => "Eta reduced " ^ V.tostring f ^
                           " = " ^ V.tostring g);
                  increment "fix/eta" 150 (bottomup 
                                           (V.Map.insert 
                                            (s, f, dosub (Var g))) kn rest)
              end
          else dofun vael rest
    | Fix (vael, rest) =>
          dofun vael rest
    end

  (* (PULL) Tired of being heroic, this is now a separate pass.

     pull takes an expression and returns
     (exp, funs, fvs)
     such that

       foldr Fix exp (map (list o #f) funs)

     is an equivalent program to the argument. fvs is the set of free
     variables of the expression.

     In this new expression, functions are hoisted to their maximal scope.

     XXX should only hoist for functions that are non-escaping!
     
     *)
  local
    (* does a free variable occur only in Direct calls (ie, App(v, ...)),
       or does it escape at least once? *)
    datatype occurrence = Direct | Escapes

    infix \\
    fun s \\ v = subtract s v

    fun fvals nil = V.Map.empty
      (* might overwrite Direct with Escapes; ok *)
      | fvals (Var v :: rest) = V.Map.insert(fvals rest, v, Escapes)
      | fvals (_ :: rest) = fvals rest

    fun occup (Escapes, _) = Escapes
      | occup (_, Escapes) = Escapes
      | occup (Direct, Direct) = Direct

    fun union s1 s2 = V.Map.unionWith occup (s1, s2)
    fun unionall l = foldl (Util.uncurry union) V.Map.empty l

    fun pull argexp : (cexp * { f  : var * var list * cexp,
                                fv : occurrence V.Map.map } list
                       * occurrence V.Map.map) =
      let

        (* the core of the algorithm. when we're about
           to bind variables vs, wrap e with 
           any function declarations from the
           list fs that have any variable in vs free. 
           returns the wrapped e, and the potentially
           reduced list of hoistable functions, and
           the almost certainly modified set of free
           variables *)
        (* wlog we can do this one variable
           at a time, in any order *)
        fun maybedump nil fv fs e = (e, fs, fv)
          | maybedump (v :: vs) free fs e =
          let
            (* find a function that needs v *)
            fun findy (free, acc, nil, e) =
              (* no more functions need it *)
              maybedump vs (free \\ [v]) (rev acc) e
              | findy (free, acc, {f, fv} :: t, e) =
              if isSome (V.Map.find (fv, v))
              then 
                (* needed!

                   since we will bind this
                   function, check for other
                   functions in t that need it. *)
                let
                  val () =
                    dprintd (fn () =>
                             "(pull) dumping " ^ V.tostring (#1 f)
                             ^ " because saw freevar " ^
                             V.tostring v ^ " (fvs: " ^
                                 StringUtil.delimit ", " 
                                 (map (V.tostring o #1) (V.Map.listItemsi fv)) ^
                                  ")\n")
                  val (e', t', free') = maybedump [#1 f] free t e
                in
                  (* other functions in t' may need
                     this var as well; keep going.
                     
                     the new freevars for e includes
                     the free variables for the function
                     we just dumped (fv). However, we remove
                     the function itself from that, since we
                     are binding it now. *)
                  findy ((union fv free') \\ [#1 f], acc, t', Fix ([f], e'))
                end
              else findy (free, {f=f,fv=fv} :: acc, t, e)
          in
            findy (free, nil, fs, e)
          end

        fun simple vals_used vars_bound exp_inside k =
          let
            val (exp, fs, fv) = pull exp_inside
            val (exp, fs, fv) = maybedump vars_bound fv fs exp
          in
            (k exp, fs, union (fvals vals_used) fv)
          end

      in
        case argexp of
          Deferred osf =>
            (case Util.Oneshot.deref (osf ()) of
               NONE => raise CPSOpt "pull: unset oneshot"
             | SOME e => pull e)
        | Alloc (t, vas, v, ce) => 
           simple vas [v] ce (fn x => Alloc(t, vas, v, x))
        | Project (i, va, v, ce) => 
           simple [va] [v] ce (fn x => Project(i, va, v, x))
        | App (Var v, vas) => (App(Var v, vas), nil, union (V.Map.insert(V.Map.empty, v, Direct)) (fvals vas))
        | App (_, _) => raise CPSOpt "pull: bad cps; app of non-var"

        (* this is where we start hoisting *)
        | Fix ([(f, args, body)], ce) =>
           let
             val (ce, fs, fv) = pull ce
             val (body, fsbod, fvbod) = pull body
             val (body, fsbod, fvbod) = maybedump (f :: args) fvbod fsbod body
           in
             dprintd (fn () => ("(pull) picked up " ^ V.tostring f));
             (* functions used in body first, then this function, then rest *)
             (ce, fsbod @ [{ f = (f, args, body), fv = fvbod }] @ fs, fv)
           end

        | Fix (fs, ce) =>
           let
             val (ce, fsrest, fvrest) = pull ce
             val thesefuns = map #1 fs
             val (ce, fsrest, fvrest) = maybedump thesefuns fvrest fsrest ce
             fun onefun (name, args, bod) =
               let
                 val (bod, fsbod, fvbod) = pull bod
                 (* all functions are bound, plus these args *)
                 val (bod, fsbod, fvbod) = maybedump (thesefuns @ args) fvbod fsbod bod
               in
                 ((name, args, bod), fsbod, fvbod)
               end
             val (fs, fsinfs, fvinfs) = ListUtil.unzip3 (map onefun fs)
           in
             dprintd (fn () => ("(pull) bypassing mutual: " ^
                                StringUtil.delimit ", " (map (V.tostring o #1) fs)));
             (* functions first, then rest *)
             (Fix(fs, ce), List.concat fsinfs @ fsrest,
              unionall (fvrest :: fvinfs))
           end

        (* binds nothing; just collect *)
       | Intswitch (va, iel, def) =>
           let
             val (def, fsdef, fvdef) = pull def
             fun onearm (i, e) =
               let
                 val (e, fs, fv) = pull e
               in
                 ((i, e), fs, fv)
               end
             val (iel, fsarms, fvarms) = ListUtil.unzip3 (map onearm iel)
           in
             (Intswitch (va, iel, def),
              List.concat fsarms @ fsdef,
              union (fvals [va]) (unionall (fvdef :: fvarms)))
           end
       | Sumswitch (va, i, v, iel, def) =>
           let
             val (def, fsdef, fvdef) = pull def
             val (def, fsdef, fvdef) = maybedump [v] fvdef fsdef def

             fun onearm (i, e) =
               let
                 val (e, fs, fv) = pull e
                 val (e, fs, fv) = maybedump [v] fv fs e
               in
                 ((i, e), fs, fv)
               end
             val (iel, fsarms, fvarms) = ListUtil.unzip3 (map onearm iel)
           in
             (Sumswitch (va, i, v, iel, def),
              List.concat fsarms @ fsdef,
              union (fvals [va]) (unionall (fvdef :: fvarms)))
           end
       | Primop (po, vas, vs, ces) =>
           let
             fun onerest ce =
               let
                 val (ce, fs, fv) = pull ce
                 val (ce, fs, fv) = maybedump vs fv fs ce
               in
                 (ce, fs, fv)
               end
             val (ces, fss, fvs) = ListUtil.unzip3 (map onerest ces)
           in
             (Primop(po, vas, vs, ces), List.concat fss, 
              union (fvals vas) (unionall fvs))
           end

      end
  in
    fun pullpass e =
      let
        val (e, fs, fv) = pull e
        fun list x = [x]

        val () =
          debugdo (fn () =>
                   let in
                     print "(pull) Toplevel closed functions:\n";
                     app (fn { f = (f, _, _), fv } => 
                          print ("  " ^ V.tostring f ^ " (fv: " ^
                                 StringUtil.delimit ", " 
                                 (map (V.tostring o #1) (V.Map.listItemsi fv)) ^
                                  ")\n")) fs
                   end)

        (* all functions here must be closed, then *)
        val fnames = map (#1 o #f) fs
        val allfv = union fv (unionall (map #fv fs))
        val allfv = subtract allfv fnames

        val finalexp = foldr Fix e (map (list o #f) fs)
      in

(*
        debugdo (fn () =>
                 let in
                   print "after pulling:\n==============================\n";
                   CPSPrint.printe finalexp;
                   print "==============================\n"
                 end);
*)
        if V.Map.numItems allfv > 0
        then 
          let in
            print "\n\n *** OOPS: ***\n----------------\n";
            CPSPrint.printe finalexp;
            print "\n----------------\n";
            V.Map.appi (fn (v, status) =>
                        print ("Still free: " ^ V.tostring v ^ "\n")) allfv;
            raise CPSOpt "code not closed after pullpass"
          end
        else
          finalexp
      end
  end (* local *)

  (* for testing from interactive loop *)
  fun botpass x =
      let val (ne, _, calls, amt : score) = bottomup V.Map.empty V.Map.empty x
      in 
          V.Map.app (fn j => app (fn {rewrite, ...} => rewrite Don't) j) calls;
          print ("Did " ^ scoretos amt ^ " units of optimization:\n");
          CPSPrint.printe ne;
          (* XXX hack:
             should just be ne -- this prohibits 
             printing at top level in NJ *)
          Deferred(fn () => Util.Oneshot.init ne)
      end

  (* We generate call sites when we see any application, even if that
     application was a function parameter or something. So we may not
     have told every call site to be not inlined. Any remaining ones
     should be not inlined right here: *)
  fun repbottom n x =
      let 
        val (ne, _, calls, amt : score) = bottomup V.Map.empty V.Map.empty x
      in
          dprint ("  ** PASS SCORE : " ^ scoretos amt ^ " **");
          V.Map.app (fn j => app (fn {rewrite, ...} => rewrite Don't) j) calls;

          debugdo (fn () =>
                   CPSPrint.printe ne);

          (* "TEST EXECUTE..." *)
          (* CPSExec.exec ne; *)

          print ("(" ^ scoretos amt ^ ") ");

          (case amt of
             Infinite => repbottom (n - 1) ne
           | Finite x =>
               if n > 0 orelse x > 50 
               then pullbottom (n - 1) ne
               else (print "\n"; ne))
      end

  and pullbottom n e =
    let
      (* XXX do pulling *)
      val e = pullpass e
    in
      repbottom n e
    end

  (* always do at least 3 (?) passes *)
  fun optimize x = 
    let 
        (* needs to be alpha-renamed, first *)
        val x = CPSAlpha.alphavary x
        (* XXX maybe should alphavary after each pass? *)
    in
(*      CPSExec.exec x; *)
      pullbottom 3 x
    end
end

