
structure Pattern :> PATTERN =
struct

  val debugpat = Params.flag false
      (SOME ("-debugpat", 
             "Debug the pattern compiler")) "debugpat"
      
  fun debugdo f = if !debugpat then f () else ()
  fun dprint s = if !debugpat then print ("[PATCMP] " ^ s ^ "\n") else ()

  infixr 9 `
  fun a ` b = a b

  exception Pattern of string

  open ElabUtil
  structure E = EL
  structure I = IL
  structure C = Context
  structure V = Variable

  (* This loosely follows the scheme used in the TILT compiler, with
     many simplifications. *)

  (* we have a matrix of patterns like this 

     obj1    obj2    obj3  ...
     pat11   pat12   pat13 ...   => e1
     pat21   pat22   pat23 ...   => e2

     _       _       _     ...   => def ()

     Columns can be rearranged at will.
     XXX this leads to difficult semantics if When patterns have
     effects. We should avoid rearranging columns; this doesn't
     really affect the strategy we use, just complicates its
     implementation slightly.

     First we "clean" each column, which means removing outer-level
     "as" patterns, type constraints, and variable bindings. Type
     constraints are removed through unification with the
     corresponding case object. 'as' patterns are removed by binding
     the variable to the case object within the corresponding
     expression. variables are just "x as _" so those are treated
     the same way.

     If the matrix has zero height, then the result is the default.
     Otherwise, if the matrix has zero width, then the result is the
     expression e1. (If the height is > 1, then we also signal a
     redundant match error if this is a user pattern.)

     Otherwise, the task is to move some column to the front and
     'split' on it. A column has a "shape" that can be deduced by the
     patterns that appear in it. The way we split depends on this
     type.

     (Cwild) A column that is just wildcards is simple; we just
     discard the column entirely. We do these first.

     (Crec) A column that contains any record is irrefutable. We
     choose to split such columns as soon as possible, since those
     just become projections and then new columns. (These columns
     must be cleaned before they are thrust back into the mix).

     (Sum) A column that contains only application patterns is
     handled with an unroll and sumcase. The same default is
     used for all subarms, which means duplicated code unless
     the sum is 1-ary.

     (Sumwild n) A column with some number of sum patterns and then a
     non-sum. n is the 0-based index of the first non-sum, and is
     always > 0.

     (When) A column that starts with a When clause.

     (Constants are treated as sums, except that a series of
     constant patterns can never be exhaustive, and we generate
     a sequence of integer tests rather than a sumcase. PERF--generate
     intcase instead.)

     This code is pretty hard to read because I often need to switch
     views of the matrix between row-major and column-major. Sorry
     about that.

     *)
  datatype shape = 
      Crec 
    | Cconst 
    | Csum 
    | Cwild 
    | Csw of int 
    | Ccw of int
    | Cwhen
    (* a pattern that has wildcards and then
       an refutable pattern *)
    | Cavoid

  fun shtos Crec = "rec"
    | shtos Cconst = "const"
    | shtos Csum = "sum"
    | shtos Cwild = "wild"
    | shtos (Csw i) = "csw-" ^ Int.toString i
    | shtos (Ccw i) = "ccw-" ^ Int.toString i
    | shtos Cwhen = "when"
    | shtos Cavoid = "avoid"

  fun elabmatrix 
         user 
         (elab : Context.context -> EL.exp -> IL.exp * IL.typ)
         (elabt : Context.context -> Pos.pos -> EL.typ -> IL.typ)
         ctx (loc : Pos.pos) obs columns es def =
   let
    fun % e_ = (e_, loc)

    (* mark a clean column with its sort *)
    fun markcolumn c =
        let
            fun allwild nil = Cwild
              | allwild (E.PWild :: rest) = allwild rest
              | allwild (E.PRecord _ :: _) = Crec
              | allwild (E.PConstant _ :: _) = Cavoid
              | allwild (E.PApp _ :: _) = Cavoid
              | allwild (E.PWhen _ :: _) = Cavoid
              | allwild _ = raise Pattern "bug: allwild/pattern not clean"
            and allsum nil _ = Csum
              | allsum (E.PWild :: _) n = Csw n
              | allsum (E.PApp _ :: rest) n = allsum rest (n + 1)
              | allsum _ _ = 
                raise Pattern "pattern column does not agree (sum)"
            and allconst nil _ = Cconst
              | allconst (E.PWild :: _) n = Ccw n
              | allconst (E.PConstant _ :: rest) n = allconst rest (n + 1)
              | allconst _ _ =
                raise Pattern "pattern column does not agree (const)"
        in
           ((case c of
                (* doesn't matter what we pick *)
                nil => Cwild
              | E.PWild :: _ => allwild c
              | E.PApp _ :: _ => allsum c 0
              | E.PConstant _ :: _ => allconst c 0
              | E.PRecord _ :: _ => Crec
              | E.PWhen _ :: _ => Cwhen
              | _ => raise Pattern "bug: pat not clean"),
             c)
        end

    (* preference for split column. This is mostly a
       heuristic, but we assume for instance that we
       will always avoid picking Cavoid columns,
       since they are impossible to split on. *)
    fun preference (Cavoid, Cavoid) = EQUAL
      | preference (Cavoid, _) = GREATER
      | preference (_, Cavoid) = LESS
      | preference (Cwhen, Cwhen) = EQUAL
      | preference (Cwhen, _) = LESS
      | preference (_, Cwhen) = GREATER
      | preference (Cwild, Cwild) = EQUAL
      | preference (Cwild, _) = LESS
      | preference (_, Cwild) = GREATER
      | preference (Crec, Crec) = EQUAL
      | preference (Crec, _) = LESS
      | preference (_, Crec) = GREATER
      | preference (Cconst, Cconst) = EQUAL
      | preference (Cconst, _) = LESS
      | preference (_, Cconst) = GREATER
      | preference (Csum, Csum) = EQUAL
      | preference (Csum, _) = LESS
      | preference (_, Csum) = GREATER
        (* between a ccw and a csw, we need to prefer
           the one with the latest wildcard before
           comparing the constructors. *)
      | preference (a, b) =
        let 
            fun geti (Csw x) = x
              | geti (Ccw x) = x
              | geti _ = 
                raise Pattern "bug: unhandled pattern kind in preference"
            val aw = geti a
            val bw = geti b
        in
            case Int.compare (aw, bw) of
                LESS => GREATER
              | GREATER => LESS
              | EQUAL =>
                    (case (a, b) of
                         (Csw a, Csw b) => Int.compare (b, a)
                       | (Csw _, _) => LESS
                       | (_, Csw _) => GREATER
                       | (Ccw a, Ccw b) => Int.compare (b, a)
                       | _ => raise Pattern "impossible")
        end

    (* elm  marked-columns-and-objects  exps  default *)
    fun elm ctx (_ : ((shape * E.pat list) * string) list)
                (nil : E.exp list) def = 
        elab ctx ` def ()
      | elm ctx nil (h::t) _ =
        if List.null t orelse not user
        then elab ctx h
        else raise Pattern "redundant match"
      | elm ctx columns exps def =
        (* pick a column to split on *)
        let

            (* when code is going to be duplicated,
               hoist it out into a function and 
               generate calls to that, instead 

               calls the continuation k with
               a new context and the name of the
               hoisted function.
               *)
            fun hoist k ctx (ile, ilt) =
                let
                    val nfs = newstr "hoist"
                    val nfv = V.namedvar nfs

                    val ignored = V.namedvar "ignored"

                    val nctx =
                        C.bindv ctx nfs (I.Mono (I.Arrow(false, 
                                                         [I.TRec nil], 
                                                         ilt))) nfv

                    val (ke, kt) = k nctx nfs
                in
                    (* would like to use nil arg list here rather
                       than single arg of unit, but need to be able
                       to call from the EL *)
                    (I.Let
                     (I.Fix(I.Mono
                            [{ name = nfv,
                               arg = [ignored],
                               dom = [I.TRec nil],
                               cod = ilt,
                               body = ile, 
                               inline = false,
                               recu = false,
                               total = false }]),
                      ke), kt)
                end

            (* handles splitting of constant patterns.
               this is simpler than sum patterns, but
               we're still trying to be a little clever. 

               Here's what we're compiling:

               obj       .             
                          .
               .           .        =>  .
               .            r       =>  .
               .             e      =>  .
               pats           s     =>  exps
               .               t    =>  .
               .                .   =>  .
               .                 .  =>  .
                                 _  =>  def

               ... where pats are all assumed to be
                   compatible constants.
               *)
            fun doconst ctx (pats : E.pat list) 
                            (obj : string) 
                            (rest : (E.pat list * string) list) 
                            (def : I.exp * I.typ)
                            (exps : E.exp list) =
              let
                  (* continuation for hoisting. this just gets the
                     name of a hoisted function that implements the
                     behavior of 'def' (without duplicating code) *)
                fun k ctx hoisted =
                  let
                      (* new default executes hoisted code *)
                      fun doconst_default () = 
                          % ` E.App (% ` E.Var hoisted, % ` E.Record nil)
                      
                      val (defe, deft) = elab ctx ` doconst_default ()

                      (* must be status Normal *)
                      val (objt, objv) = 
                          case C.var ctx obj of
                              (I.Mono tt, vv, Normal) => (tt, vv)
                            | _ => raise Pattern 
                                  "case object is poly or constructor (?)"

                      (* strip "Constant" constructor *)
                      val consts = map 
                          (fn E.PConstant c => c
                            | _ => raise Pattern 
                           "bug: expected constant in doconst") pats


                      (* used later to reattach objects *)
                      val robjs = map (fn (_, ob) => ob) rest
                                                    
                      (* transpose the remaining columns, so that we can
                         collect up rows in the next pass. If there are
                         no more columns, make a list of the correct
                         length, but with each row being nil. 
                         
                         when splitting, we'll probably change the shapes
                         of the other columns, so lose that info. *)

                      val rest =
                          case rest of
                              nil => map (fn _ => nil) consts
                            | _ => ListUtil.transpose ` map #1 rest

                      val _ =
                          (length rest = length consts)
                          andalso length consts = length exps
                          orelse raise Pattern "length invt violated"

                      (* now working with rows. *)
                      val rows = 
                          ListPair.zip (consts, ListPair.zip(exps, rest))

                      (* this expects to find mono-shape patterns,
                         so if it doesn't, fail early *)
                      fun compare (E.CInt i, E.CInt j) = Word32.compare(i, j)
                        | compare (E.CInt _, _) =
                              raise Pattern "(un)expected int pattern"
                        | compare (_, E.CInt _) =
                              raise Pattern "(un)expected int pattern"
                        | compare (E.CString s, E.CString t) =
                              String.compare (s, t)
                        | compare (E.CString _, _) =
                              raise Pattern "(un)expected string pattern"
                        | compare (_, E.CString _) =
                              raise Pattern "(un)expected string pattern"
                        | compare (E.CChar c, E.CChar d) =
                                  Char.compare(c, d)

                      val sorted =
                          ListUtil.stablesort 
                          (ListUtil.byfirst compare) rows

                      fun part ((const, dat)::t) =
                          let
                              (* scoop up any of the same const.
                                 they must appear immediately after,
                                 since the list is sorted *)

                              fun more acc (ll as ((cst, da)::t)) =
                                  (case compare(cst, const) of
                                       EQUAL => more (da::acc) t
                                     | _ => (rev acc, ll))
                                | more acc nil = (rev acc, nil)

                              val (these, those) = more [dat] t
                          in
                              (const, these) :: part those
                          end
                        | part nil = nil

                      (* up to here could probably have been
                         shared with dosum code *)
                      val clustered = part sorted
                          : (E.constant * (E.exp * E.pat list) list) list
                          
                      (* clustered has this form: 
                     
                         [(Const n,
                          [(exp, row), ...] (for each beginning with n))]
                         
                         ... where the exp/row list is never nil.

                         This would be the place to implement better
                         strategies like binary search or jump tables. *)

                      (* Generalized over ints and chars.
                         pass the expression to check against,
                         the primop that performs such equality,
                         and the il type *)
                      fun smallconst eq_exp peq ilt matches rest =
                        let

                          (* this must be an XXXint pattern, then *)
                          val () = unify ctx loc "const int pattern"
                                         ilt objt

                          fun kk ctx failure = 
                           let 
                            val (failc, failt) = 
                                elab ctx (% ` E.App (% ` E.Var failure, 
                                                     % ` E.Record nil))
                                
                           in
                           (I.Sumcase
                            (unroll loc `
                             elabt ctx loc `
                             E.TVar Initial.boolname,

                             (* unroll (obj = eq_exp) *)
                             I.Unroll `
                             I.Primapp
                              (peq,
                               [I.Var objv, eq_exp], 
                               nil),

                             V.namedvar "unused",
                             [(Initial.truename,
                               (* equal. match any associated
                                  row, then ok. otherwise,
                                  failure continuation. *)
                               let
                                   val (exps, rows) = ListPair.unzip matches
                                   val cols =
                                       ListPair.zip
                                       (map markcolumn `
                                        ListUtil.transpose rows,
                                        robjs)
                                      : ((shape * E.pat list) * string) list
                                   
                                   (* in here, we know the object can't
                                      match anything from "rest" (the
                                      failure continuation); though
                                      it's safe to continue with those,
                                      it's a waste. Instead proceed to
                                      the original default. *)

                                   val (ee, tt) =
                                       elm ctx cols exps doconst_default
                               in
                                   unify ctx loc
                                     "ccw cont / default" deft tt;
                                   ee
                               end)],

                             (* if not equal, invoke
                                failure continuation*)
                             failc),
                            deft)
                           end

                        in
                            hoist kk ctx ` gen rest
                        end


                      and gen ((E.CInt n, matches)::rest) =
                          smallconst (I.Int n) 
                                     (Primop.B ` Primop.PCmp Primop.PEq) 
                                     Initial.ilint matches rest
                          
                        (* in the IL, chars are compiled as ints. *)
                        | gen ((E.CChar c, matches)::rest) = 
                          smallconst (I.Int (Word32.fromInt (ord c)))
                                     (Primop.B ` Primop.PCmp Primop.PEq)
                                     Initial.ilchar matches rest

                        | gen (_ :: _) =
                        raise Pattern 
                            "string constant patterns unimplemented"

                      (* no clauses? use real default *)
                      | gen nil = elab ctx ` doconst_default ()

                  in
                      gen clustered
                  end
              in
                  hoist k ctx def
              end


            (* Handle splitting of app patterns.
               app patterns can be datatype constructors
               or exn constructors, so this code is
               pretty tough. (spec is as above for doconst)

               This is called by csum and csw. *)
            fun dosum ctx pats obj rest def exps =
             let
               (* given the hoisted default, generate the case *)
               fun k nctx hoisted =
                let
                  val () = dprint ("split on sum col " ^ obj)
                    (* new default executes hoisted code *)
                    fun ndef () = (% ` E.App (% ` E.Var hoisted, 
                                              % ` E.Record nil))

                    val _ = length pats = length exps
                        orelse raise Pattern 
                                 "different number of exps/pats in dosum"

                    (* used later to reattach objects *)
                    val robjs = map (fn (_, ob) => ob) rest

                    (* zip up expressions with this column *)
                    val col = ListPair.zip (pats, exps)

                    (* as in doconst *)

                    val rest =
                        case rest of
                            nil => map (fn _ => nil) col
                          | _ => ListUtil.transpose `
                                  map #1 rest


                    (* make a list of lists--each sublist contains
                       only projections from the same sum label,
                       preserving their original order *)

                    fun part nil nil acc = ListUtil.mapsecond rev ` rev acc
                      | part ((E.PApp (lab, p), e)::crest) (row::rrest) acc =
                        part crest rrest
                        (case ListUtil.Alist.find op= acc lab of
                             NONE => (lab, [(p, e, row)]) :: acc
                           | SOME b => 
                                 ListUtil.Alist.update op= acc lab 
                                 ((p,e,row)::b))
                      | part nil (_::_) _ =
                             raise Pattern "bug: col is shorter than rest"
                      | part (_::_) nil _ =
                             raise Pattern "bug: rest is shorter than col"
                      | part _ _ _ = 
                             raise Pattern 
                          ("impossible: non-app in sum col " ^
                           "or mismatched col/rest")

                    val parted = part col rest nil

                    (* parted:
                       list of
                          label * list of 
                                 inner pattern (option), expression, rest of row *)

(*
                    val _ =
                        (print "Parted:\n";
                         app (fn (l, perl) =>
                              print (l ^ ": " ^
                                     StringUtil.delimit "," 
                                         (map (ELPrint.ptos o #1) perl)
                                      ^ "\n")) parted)
*)

                    (* can't fail -- we know there is at least one pat *)
                    val (l, _) = hd parted

                in
                    (* look at the first to decide if we're looking 
                       at datatype constructors or exn (tagtype)
                       constructors. Sadly, there is a lot of
                       duplicated code in these two arms, but it makes
                       it way too hairy to put them together. *)

                    case C.var nctx l of
                      (I.Mono (I.Arrow (_, _, cod as I.TVar _)), _, 
                       I.Tagger _) =>
                       (* ****** Exception Constructor **** *)
                       let
                           val parted = ListUtil.mapsecond
                                             (map (fn (SOME p, e, r) => (p, e, r)
                                                   | _ => raise Pattern
                                                      "exn papp not nullary")) parted
                           val rett = new_evar ()

                           val insidee = newstr "intag"
                           val insidev = V.namedvar insidee

                           (* XXX this could be factored out with 
                              onelab below *)
                           fun onelab (l, perl) =
                             case C.var nctx l of
                               (I.Mono (I.Arrow (_, [ruledom], rulecod)), 
                                _, I.Tagger vtag) =>
                                 let
                                     (* objty = rulecod *)

                                     (* every arm will have access to 
                                        the tagged innards *)
                                     val nctx = C.bindv nctx insidee 
                                                  (I.Mono ruledom) insidev 

                                     val _ = unify nctx loc "tagcase codomain" 
                                                      cod rulecod

                                     val (ocol, oe, rest) = 
                                         ListUtil.unzip3 perl

                                     (* clean new column. *)
                                     val (ncol, ne) =
                                         clean nctx loc elabt insidee
                                             ruledom ocol oe

                                     val rest = ListUtil.transpose rest
                                     val _ = length rest = length robjs
                                             orelse null rest
                                             orelse
                                             raise Pattern 
                                                 "rest/robjs mismatch"

                                     val rest = ListPair.zip (rest, robjs)

                                     val cols =
                                         (markcolumn ncol, insidee)::
                                         ListUtil.mapfirst markcolumn rest

                                     val _ = dprint ("exncol: " ^ l ^ ":")

                                     (* elaborate the inside *)

                                     val (re, rt) = elm nctx cols ne ndef
                                   in
                                       unify ctx loc 
                                          "tag pattern return" rt rett;
                                       (vtag, re)
                                   end
                             | _ => raise Pattern 
                                     ("expected tagtype constructor: " ^ l)

                         val (thearms : (I.var * I.exp) list) = 
                             map onelab parted

                         val (de, dt) = elab nctx ` ndef ()

                         val (opt, objv, _) = C.var nctx obj

                       in
                           (* unify object with codomain of constructors. *)
                           unify nctx loc "tagcase arg" (evarize opt) cod;
                           unify nctx loc "tagcase default" rett dt; 

                           (I.Tagcase (cod, 
                                       I.Var objv,
                                       insidev,
                                       thearms,
                                       de),
                            rett)

                       end

                    | (pt, _, I.Constructor) =>
                       (* ****** Datatype Constructor ***** *)
                       let
                         val cod =
                           (case evarize pt of
                              I.Arrow(_, _, cod) => cod
                            | (tt as (I.Mu _)) => tt
                            | _ => raise Pattern "bug:can't get codomain from first app pattern")

                         val rett = new_evar ()

                         (* Sumcase binds one variable for all 
                            arms + default *)
                         val insides = newstr "insum"
                         val insidev = V.namedvar insides

                         (* for each entry in the parted list,
                            do type checking and then elaborate
                            the subpatterns. *)
                         fun onelab (l, perl) =
                           (case (C.var nctx l, new_evar ()) of
                             ((pt, _, I.Constructor), domvar) =>
                               (case evarize pt of
                                  (* nullary constructor *)
                                  (tt as (I.Mu _)) =>
                                   (case ElabUtil.unroll loc tt of
                                      I.Sum ltl =>
                                        let
                                          (* sanity check *)
                                          val () =
                                            (case ListUtil.Alist.find op= ltl l of
                                               NONE => raise Pattern "non-carrier label not in sum??"
                                             | SOME (IL.Carrier _) => 
                                                 raise Pattern "non-carrier label has carried type?"
                                             | SOME IL.NonCarrier => ())

                                          val _ = unify nctx loc "(nullary)sum codomain" cod tt

                                          (* don't bind var; it would be nonsensical here. *)

                                          (* also there is no column *)
                                          val (_, oe, rest) = ListUtil.unzip3 perl

                                          (* XXX need wildcard column? *)
                                          val rest = ListUtil.transpose rest
                                          val _ = length rest = length robjs
                                            orelse null rest
                                            orelse
                                            raise Pattern 
                                              "(nullary) rest/robjs mismatch"

                                          val rest = ListPair.zip (rest, robjs)
                                            
                                          val cols = ListUtil.mapfirst markcolumn rest
                                            
                                          val () = dprint ("sumcol: " ^ l ^ ":")
                                            
                                          (* elaborate the inside *)
                                          val (re, rt) = elm nctx cols oe ndef
                                        in
                                          unify ctx loc 
                                             "(nullary) sum pattern return" rt rett;
                                          (l, re)
                                        end
                                    | _ => raise Pattern "(nullary-pat)mu didn't contain sum")
                                  (* carrier constructor *)
                                | (I.Arrow(_, ruledom, rulecod)) =>
                                   (case ElabUtil.unroll loc rulecod of
                                      (I.Sum ltl) =>
                                        (case ListUtil.Alist.find op= ltl l of
                                           NONE => raise Pattern "label not in sum??"
                                         | SOME IL.NonCarrier => 
                                             raise Pattern "bug:non-carriers shouldn't have arrow type"
                                         | SOME (IL.Carrier { carried = objty, ...}) =>
                                           let

                                               val ruledom = 
                                                 case ruledom of
                                                   [rd] => rd
                                                 | _ => raise Pattern "carrier not unary arrow"

                                               val nctx = C.bindv nctx insides 
                                                             (I.Mono objty) insidev 


                                             (* XXX this is pretty suspect, 
                                                since domvar is fresh and I
                                                never use it again ... *)

                                               val _ = unify nctx loc "sum domain" 
                                                              domvar ruledom

                                               val _ = unify nctx loc "sum codomain" 
                                                              cod rulecod

                                               val (ocol, oe, rest) =
                                                   ListUtil.unzip3 perl

                                               val ocol =
                                                 map (fn (SOME p) => p
                                                           | NONE => 
                                                      raise Pattern 
                                                        "nullary papp in carrier case")
                                                 ocol

                                               (* clean new column. *)
                                               val (ncol, ne) =
                                                   clean nctx loc elabt insides
                                                          ruledom ocol oe

                                               val rest = ListUtil.transpose rest
                                               val _ = length rest = length robjs
                                                       orelse null rest
                                                       orelse
                                                       raise Pattern 
                                                           "rest/robjs mismatch"

                                               val rest = ListPair.zip (rest, robjs)

                                               val cols =
                                                   (markcolumn ncol, insides)::
                                                   ListUtil.mapfirst markcolumn rest

                                               val () = dprint ("sumcol: " ^ l ^ ":")

                                               (* elaborate the inside *)
                                               val (re, rt) = elm nctx cols ne ndef
                                           in
                                               unify ctx loc 
                                                  "sum pattern return" rt rett;
                                               (l, re)
                                           end)
                                    | _ => raise Pattern "mu bod not sum?")
                               | tt => raise Pattern
                                      ("bug: ctor was not mu or arrow type: "
                                       ^ (Layout.tostring (ILPrint.ttol tt))))
                           | _ => raise Pattern
                               ("non-constructor in app pattern " ^ l))

                         val (thearms : (I.label * I.exp) list) = 
                             map onelab parted

                         val (st, insum) = 
                             (case cod of
                                  I.Mu(n, mubod) =>
                                      (case List.nth (mubod, n) of
                                           (_, sum as I.Sum insum) => 
                                               (sum, insum)
                                         | _ => raise Pattern
                                               "mu body not sum??")
                                | _ => raise Pattern
                                           "ctor cod not mu??")

                         val (de, dt) = elab nctx ` ndef ()

                         val (opt, objv, _) = C.var nctx obj
                             
                         val nsum = length insum

                       in
                           (* unify object with codomain of constructors. *)
                           unify nctx loc "sum arg" (evarize opt) cod;
                           unify nctx loc "sum default" rett dt; 

                           (* if exhaustive, lose default and
                              replace with final branch *)
                           (if length thearms = nsum
                            then I.Sumcase (st,
                                            I.Unroll ` I.Var objv,
                                            insidev,
                                            List.take (thearms, nsum - 1),
                                            #2 ` List.last thearms)
                                
                            else I.Sumcase (st, 
                                            I.Unroll ` I.Var objv,
                                            insidev,
                                            thearms,
                                            de),
                            rett)

                       end
                    | _ => raise Pattern 
                               ("Non-constructor in app pattern " ^ l)
                end
             in
                 (* def will potentially be duplicated. hoist it. *)
                 hoist k ctx def
             end

            (* PERF - this doesn't really need to be a sort, it just
               needs to bring the lowest to the front *)
            val columns = ListUtil.sort (ListUtil.byfirst `
                                         ListUtil.byfirst preference) columns

        in

            (* debugging -- show progress *)
            debugdo 
            (fn () =>
             let 
                 val ty = map (fn ((t, _), _) => shtos t) columns
                 val obs = map (fn (_, ob) => ob) columns
                 val cols = ListUtil.transpose (map (fn ((_, c), _) => 
                                                     map ELPrint.ptos c) 
                                                columns
                                                
                                                @ [map (fn _ => "=") exps]
                                                @ [map (ELPrint.etosi 0) exps])
             in
                 print "\n\npattern matrix:\n";
                 print `
                 StringUtil.table 78 (ty::obs::cols)
             end);

            (* each column is ((kind, arms), objvar) *)
            (case columns of
                 nil => raise Pattern "impossible"
               | ((Cwild, _), obj) :: rest => 
                     let in
                         dprint ("Split on wild col: " ^ obj);
                         elm ctx rest exps def
                     end
               | ((Csum, apps), obj) :: rest =>
                     dosum ctx apps obj (map (fn ((_, pats), ob) => 
                                              (pats, ob)) rest) 
                           (elab ctx ` def ()) exps
               | ((Cwhen, E.PWhen(exp, punder) :: col), obj) :: rest =>
                     (* do these just one at a time. 

                        we have:

                        case v1        ...     vn of
                          p when e     ...     p1n => e1
                             :         .        :
                           (col)        .       :
                             :           .      :
                            pm1        ...     pmn => em
                                                _  => def

                        we generate:

                        let val v1' = YES (e v1) handle Match => NO
                        in (case v1'      ...   vn   of
                                YES p     ...   p1n  => e1
                                                  _  => 

                           (case v1       ...   vn   of
                                 p21      ...   p2n  => e2
                                  :       .      :
                                (col)      .     :
                                  :         .    :
                                 pm1      ...   pmn  => em
                                                  _  => def))
                        end
                        *)
                   let

                     val () = dprint ("Split on when col: " ^ obj)
                       
                     (* throw away shapes, then break up rest into cols and
                        objects *)
                     val rest = map (fn ((shape, col), ob) => (col, ob)) rest
                     val (rcols, robs) = ListPair.unzip rest

                     (* PERF should not be carrier so we can apply sumrep optimization *)
                     val yes = HumlockUtil.newstr "Yes"
                     val no  = HumlockUtil.newstr "No"

                     (* new object for this column turns the exception
                        matching into a datatype wrap *)
                     val nobj =
                       E.Handle(% ` E.App(% ` E.Var yes,
                                          % ` E.App(exp,
                                                    % ` E.Var obj)),
                                [(E.PApp (Initial.matchname, SOME ` E.PRecord nil),
                                  % ` E.App(% ` E.Var no,
                                            % ` E.Record nil))])

                     val objects_a = % nobj :: map (% o E.Var) robs
                     val objects_b = map (% o E.Var) (obj :: robs)
                     val cols_a = [E.PApp(yes, SOME punder)] :: 
                                    map (fn x => [hd x]) rcols
                     val cols_b = col :: map tl rcols

                     val prows_a = ListUtil.transpose cols_a
                     val prows_b = ListUtil.transpose cols_b

                     val exps_a = [hd exps]
                     val exps_b =  tl exps

                     val _ = length prows_a = length exps_a
                             orelse raise Pattern "bug: |prows_a| <> |exps|"
                     val _ = length prows_b = length exps_b
                             orelse raise Pattern "bug: |prows_b| <> |exps|"

                     val rows_a = ListPair.zip (prows_a, exps_a)
                     val rows_b = ListPair.zip (prows_b, exps_b)
                       
                     val () = dprint "prows_a:"
                     val () = app (fn r => dprint ("  " ^ StringUtil.delimit "   "
                                                   (map ELPrint.ptos r))) prows_a
                     val () = dprint "prows_b:"
                     val () = app (fn r => dprint ("  " ^ StringUtil.delimit "   "
                                                   (map ELPrint.ptos r))) prows_b

                     val () = dprint "exps_a:"
                     val () = app (fn e => dprint ("  " ^ ELPrint.etosi 2 e)) exps_a
                     val () = dprint "exps_b:"
                     val () = app (fn e => dprint ("  " ^ ELPrint.etosi 2 e)) exps_b


                     (* let datatype ('a, 'b) whentag = 
                            Yes of 'a | No of 'b *)
                     val we = E.Let(% ` E.Datatype (["a"],
                                    [(HumlockUtil.newstr "whentag",
                                      [(yes, SOME (E.TVar "a")),
                                       (no,  SOME (E.TRec nil))])]),
                                    % `
                                    E.Case (objects_a, rows_a, 
                                            SOME 
                                            (fn () =>
                                             % ` E.Case (objects_b,
                                                         rows_b,
                                                         SOME def))))
                   in
                     elab ctx (% we)
                   end

               | ((Cwhen, _), _) :: _ =>
                     raise Pattern "bug: shape is 'when' but not when pat"

               | ((Cavoid, _), _) :: _ =>
                     raise Pattern "bug: best column was 'avoid' !"

               | ((Csw 0, _), _) :: _ =>
                     raise Pattern "bug: best column was sumwild 0 !"
               | (all as ((Csw n, someapps), obj) :: rest) => 
                     (* dosum on the first n rows, but send along
                        a default that does the remaining matrix. *)
                     let
                         val (mtx, objs) = 
                             ListPair.unzip `
                             map (fn ((_, col), obj) => (col, obj)) all

                         val mtx = ListUtil.transpose mtx

                         (* these are all already clean *)
                         val (top, bot) = ListUtil.cleave n mtx
                         val (tope, bote) = ListUtil.cleave n exps

                         local
                             val cols = map markcolumn `
                                          ListUtil.transpose bot
                             val cols = ListPair.zip (cols, objs)
                         in
                             val ndef =
                                 elm ctx cols bote def
                         end

                         val tcs = ListUtil.transpose top
                     in
                         dosum ctx (hd tcs) obj (ListPair.zip 
                                                 (tl tcs, tl objs))
                               ndef tope
                     end

               (* constant case is much like sum case *)
               | ((Cconst, consts), obj) :: rest =>
                     doconst ctx consts obj (map (fn ((_, pats), ob) => 
                                                  (pats, ob)) rest) 
                             (elab ctx ` def ()) exps
               | ((Ccw 0, _), _) :: _ =>
                     raise Pattern "bug: best column was constwild 0 !"
               | (all as ((Ccw n, consts : E.pat list), obj) :: rest) => 
                     (* case analyze first n rows, but send along
                        a default that does the remaining matrix. *)
                     let
                         val (mtx, objs) = 
                             ListPair.unzip `
                             map (fn ((_, col), obj) => (col, obj)) all

                         val mtx = ListUtil.transpose mtx

                         (* these are all already clean.

                            top is what we'll split on.
                            bot is what's left. *)
                         val (top, bot) = ListUtil.cleave n mtx
                         val (tope, bote) = ListUtil.cleave n exps

                         (* match the bottom. this becomes our 
                            new default. *)
                         local
                             val cols = map markcolumn `
                                          ListUtil.transpose bot
                             val cols = ListPair.zip (cols, objs)
                         in
                             val newdef = elm ctx cols bote def
                         end

                         val tcs = ListUtil.transpose top
                     in
                         doconst ctx (hd tcs) obj (ListPair.zip 
                                                   (tl tcs, tl objs))
                                 newdef tope
                     end

               | ((Crec, recs), obj) :: rest =>
                 let
                     val _ = dprint ("Split on rec col: " ^ obj);

                     (* split record. we should only see wild
                        and rec patterns of the appropriate type. 
                        return label -> new column alist

                        *)
                     fun split (p, acc) =
                       (case (p, acc) of
                          (E.PWild, Util.B l) => Util.B (E.PWild :: l)
                        | (E.PWild, Util.A lcl) =>
                            (* cons pwild onto all new columns *)
                            Util.A ` map (fn (l, pl) => 
                                          (l, E.PWild :: pl))
                                         lcl
                        | (E.PRecord spl, Util.A lcl) =>
                            let val spl =
                                ListUtil.sort (ListUtil.byfirst
                                               HumlockUtil.labelcompare) spl
                            in
                                ListUtil.all2 
                                   (ListUtil.byfirst op=) spl lcl
                                   orelse raise 
                                   Pattern "patterns don't have same labels";

                                Util.A `
                                ListPair.map
                                   (fn ((_, p), (l, pl)) =>
                                    (l, p :: pl)) (spl, lcl)
                            end
                        | (E.PRecord spl, Util.B l) =>
                            let
                                val spl =
                                ListUtil.sort (ListUtil.byfirst
                                               HumlockUtil.labelcompare) spl
                            in
                                Util.A `
                                map (fn (lab, p) => (lab, p :: l)) spl
                            end
                        | _ => raise Pattern "patterns don't agree (record)")

                     (* can't fail -- must have at least one PRecord *)
                     val newcols = 
                         case foldr split (Util.B nil) recs of
                             Util.A x => x
                           | _ => raise Pattern "impossible"

                     val newcols =
                         map (fn (l, col) => (l, col, new_evar ())) newcols

                     val (obje, objt) = elab ctx (E.Var obj, loc)

                     fun recurse nil (nctx, ncols, nes) =
                         elm nctx ncols nes def
                       | recurse ((l, col, t)::rest) (c, cols, oes) =
                         let
                             val ss = newstr ("pat_" ^ l)
                             val v = V.namedvar ss
                             val nc = C.bindv c ss (I.Mono t) v

                             (* clean column *)
                             val (col, nes) = 
                                 clean nc loc elabt ss t col oes

                             val (ee, tt) = 
                                 recurse rest 
                                    (nc, (markcolumn col, ss) :: cols, nes)
                         in
                             (I.Let(I.Val(I.Mono
                                         (v, t, I.Proj (l, objt, obje))),
                                   ee),
                              tt)
                         end


                 in 
                     (* unify object with record type *)
                     unify ctx loc "record pattern" objt 
                         ` I.TRec ` map (fn (l, _, t) => (l, t)) newcols;

                     recurse newcols (ctx, rest, exps)
                 end)
        (*        | ((sh,_),_)::_ => raise Pattern 
                 ("unimplemented pattern shape " ^ shtos sh)) *)
        end
   in
       elm ctx (ListPair.zip(map markcolumn columns, obs)) es def
   end



  (* user : bool  --  whether to disallow redundant patterns (because the
                      pattern was supplied by a user rather than the 
                      compiler)

     elab         --  Elaborate.elab
     elabt        --  Elaborate.elabt
     ctx          --  The current context
     objs         --  List of case objects   (case obj of ...)
     tv           --  List of types (evars) corresponding to case objects
     clauses      --  list of clauses (pat list * exp): one for each object
     def          --  unit -> EL.exp function that produces the default
                      (and perhaps a warning message)

     *)

  (* XXX I don't check for duplicate variables in an 'as' pattern, or
     indeed anywhere else. Probably the right thing to do is to
     check elsewhere that each pattern line is affine.

     This doesn't make the language unsound (the program is still
     typechecked), but it can produce really unpredictable results as
     far as the programmer is concerned, since columns may be
     rearranged and minor changes to the program can cause different
     variables to be shadowed in a pattern row. *)


  and elaborate user elab elabt (ctx : C.context) loc
                  (obs : string list, 
                   m   : (E.pat list * E.exp) list,
                   def : unit -> E.exp) =
      let

          val _ = debugdo
          (fn () =>
           let in
               print "Pattern compiler!\n";
               print ("obs: [" ^ StringUtil.delimit ", " obs ^ "]\n");
               print ("m: [" ^ StringUtil.delimit ", "
                      (map (fn (pl, e) => 
                            "([" ^ StringUtil.delimit ", "
                            (map ELPrint.ptos pl) ^ "], " ^
                            ELPrint.etosi 5 e ^ ")") m)  ^ "]\n")
           end);
          
          val columns = ListUtil.transpose (map #1 m)
              handle ListUtil.ListUtil => 
                  raise Pattern "bug: ragged matrix??"

          val columns =
            (* we want a column for every object, even if there
               are *no* rows. *)
            (case columns of
               nil => (map (fn _ => nil) obs)
             | c => c)

          val es = map #2 m

          val tvs = map (fn ob => 
                         let val (_, tt) = elab ctx (E.Var ob, loc)
                         in tt
                         end) obs

          val (columns, es) = 
              (ListUtil.foldl3
               (fn ((pl, tv, ob), (cols, es)) => 
                let val  (ccs, ees) = clean ctx loc elabt ob tv pl es
                in (ccs :: cols, ees)
                end) (nil, es) columns tvs obs)
              handle ListUtil.ListUtil => 
                  raise Pattern "bug: exps/tvs/pl mismatch"

          (* because foldl above reversed it *)
          val columns = rev columns

      in
          length tvs = length columns
             orelse raise Pattern "wrong number of pattern columns for args";

          elabmatrix user elab elabt ctx loc obs columns es def

      end

  (* clean one column corresponding to the arg a,
     the type (for unification) of that arg tv,
     the list of patterns comprising the column pl,
     and list of expressions the same length as the column es.

     Return the new column and the new list of expressions.
     *)
  and clean ctx loc elabt a tv pl (es : EL.exp list) 
        : (EL.pat list * EL.exp list) =
      let
          fun one (p, e) =
              (case p of
                   E.PAs (s, pp) =>
                       let in
                           dprint ("cleaning " ^ s ^ "...\n");
                           one (pp, (E.Let((E.Val(nil, E.PVar s, 
                                                  (E.Var a, loc)), 
                                        loc), e), loc))
                       end
                 | E.PVar s => one (E.PAs (s, E.PWild), e)
                 | E.PConstrain (pp, tt) =>
                       let val t = elabt ctx loc tt
                       in unify ctx loc "pattern constraint" tv t;
                          one (pp, e)
                       end
                 | _ => (p, e))
      in
          ListPair.unzip `
          ListPair.map one (pl, es)
      end

end