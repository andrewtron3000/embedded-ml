(* GC that doesn't do gc, in order to make code simpler for debugging *)

structure GCMinimal =
struct

  structure U = UMA
  type reg = U.reg
  open U.Inst
  structure V = Variable

  exception GC of string

  datatype shape =
           ARRAY_TRACED   (* length field n, then n traced pointers *)
         | UNTRACED       (* raw data *)
         | TAGGED_POINTER (* untraced word then traced pointer *)

  structure W = Word32

  open Conventions

  val gc_data_size = 7

  (* NB ALL of these offsets MUST be small enough to be loaded as a literal in
   a single instruction. *)

  val gc_header_size = 2

  (* labels, some global, some private to GC *)
  val gc_init = V.namedvar "GC_MIN_INIT"
  val gc_end = V.namedvar "GC_MIN_END"

  fun gc_check roots_length = [COMMENT "(minimal) GC check"]
  fun gc_alloc_common t1 t2 res_reg sh =
      (* res_reg currently contains the computed size for the new object *)
      [ALLOC (res_reg, res_reg)]

  fun gc_alloc t1 t2 res_reg size_reg sh =
      [
       COMMENT ("(minimal) Alloc (size in " ^ Int.toString size_reg ^ ")"),
       (* compute size; allocate space for object + header *)
       LITERAL (res_reg, W.fromInt gc_header_size),
       ADD (res_reg, size_reg, res_reg)
      ]
      @ gc_alloc_common t1 t2 res_reg sh

  fun gc_alloc_static t1 t2 res_reg size sh =
      [
       COMMENT ("(minimal) Alloc (static size " ^ Int.toString size ^ ")"),
       LITERAL (res_reg, W.fromInt (size + gc_header_size))
      ]
      @ gc_alloc_common t1 t2 res_reg sh

  fun gc_code roots_start mainlab = 
      [
       (gc_init,
        [
         LITERAL_ADDR(bb, mainlab, aa),
         LOADPROG (zz, bb)
        ]),
       (gc_end,
        [
         LOADPROG (zz, gc_returnreg)
        ])
      ]

end
