structure GC :> GC =
struct

  structure U = UMA
  type reg = U.reg
  open U.Inst
  structure V = Variable

  (* ENH make it a command-line param *)
  val debug = Params.flag true
        (SOME ("-gcdebug",
               "Inlcude GC debug info")) "gcdebug"

  fun rdtsc x = if !debug then RDTSC x
                else MANY []

  fun info x = if !debug then INFO x
               else MANY []

  val paranoid = Params.flag false
        (SOME ("-gcparanoid", 
               "Run GC in every prologue")) "gcparanoid"
 
  (* used for compile-time configuration errors and assertions *)
  exception GC of string

  datatype shape =
           ARRAY_TRACED   (* length field n, then n traced pointers *)
         | UNTRACED       (* raw data *)
         | TAGGED_POINTER (* untraced word then traced pointer *)

  structure W = Word32

  open Conventions

  (* INFO 

     0x1000: number of roots at the start of collection

     0x1040: number of ticks since last collection
     0x1041: number of ticks during tracing
     0x1042: number of ticks during sweeping

     0x1080: number of live objects at the end of collection
     0x1081: number of dead objects at the end of collection (stats only)
     0x1082: number of allocated objects (stats only)

     0x1100: new function call threshold
     0x1101: alternative (and unused) function call threshold

   *)

  val wtoi = W.fromInt

  (* initial and minimum value for determining the length of the interval
   between collections *)
  val min_function_call_threshold = wtoi 100
  (* maximum value for determining the length of the interval between 
   collections *)
  val max_function_call_threshold = wtoi 0x8000000

  val target_ratio = Params.param (Int.toString 5)
        (SOME ("-gcratio",
               "GC target ratio")) "gcratio"

(*
  T_k = max (T_k-1 * (L_k / L_k_1), T_min)

  T_k = T_1 (L_k / L_1) = (T_1 / L_1) L_k

  Let R = (T_1 / L_1) be a config parameter

  at end of cycle compute MAX_VAL / R
    if L > (MAX_VAL / R) then let T = MAX_VAL
    otherwise T = R * L 
*)

  (* gather additional (and non-essential) statistics at a non-trivial cost *)
  val statistics = false

  (* number of words at the beginning of every heap object reserved for the 
   collector *)
  val gc_header_size = 2

  (* largest array size that is special cased in the tracing code *)
  val max_fixed_array_size = 0w2 : W.word

  (* NB ALL of these offsets MUST be small enough to be loaded as a literal in
   a single instruction. *)
  (* val next_unfree_offset = 0w0 : W.word *)
  val flags_offset = 0w1 : W.word

  val tagged_pointer_offset = wtoi (gc_header_size + 1)
  val array_length_offset = wtoi (gc_header_size)

  val tagged_pointer_bit = W.<< (0w1, 0w1)
  val array_traced_bit = W.<< (0w1, 0w2)
  val any_traced_bits = W.orb (tagged_pointer_bit, array_traced_bit)

  (* We set mark_bit = flags_offset to enable several optimizations below *)
  (* val mark_bit = flags_offset *)
  val non_mark_bits = any_traced_bits

  val stack_next_offset = 0w1 : W.word

  fun shape_to_flags UNTRACED = raise GC "shouldn't call shape_to_flags for UNTRACED"
    | shape_to_flags TAGGED_POINTER = tagged_pointer_bit
    | shape_to_flags ARRAY_TRACED = array_traced_bit

  (* labels, some global, some private to GC *)
  val gc_init = V.namedvar "gc_init"
  val gc_end = V.namedvar "gc_end"

  val lab_data = V.namedvar "gc_data"
  (* data labels *)
  val lab_unfree_list = V.namedvar "gc_unfree_list"
  val lab_counter = V.namedvar "gc_counter"
  val lab_last_counter = V.namedvar "gc_last_counter"
  val lab_last_heap_size = V.namedvar "gc_last_heap_size"
  val lab_return_addr = V.namedvar "gc_return_addr"
  val lab_first_saved_reg = V.namedvar "gc_first_saved_reg"
  val lab_second_saved_reg = V.namedvar "gc_second_saved_reg"
  val lab_last_saved_reg = V.namedvar "gc_last_saved_reg"
  val lab_live_object_count = V.namedvar "gc_live_object_count"
  val lab_spilled_text = V.namedvar "gc_spilled_text"
  val lab_timestamp = V.namedvar "gc_timestamp"
  val lab_dead_object_count = V.namedvar "gc_dead_object_count"
  val lab_alloc_count = V.namedvar "gc_alloc_count"

  val lab_collect = V.namedvar "gc_collect"

  val lab_maybe_mark_root = V.namedvar "gc_maybe_mark_root"
  val lab_mark_root = V.namedvar "gc_mark_root"
  val lab_push_root = V.namedvar "gc_push_root"

  val lab_maybe_pop = V.namedvar "gc_maybe_pop"
  val lab_pop = V.namedvar "gc_pop"

  val lab_trace = V.namedvar "gc_trace"
  val lab_trace_array = V.namedvar "gc_trace_array"
  val lab_trace_tagged = V.namedvar "gc_trace_tagged"

  val tab_array_fixed_jump = V.namedvar "gc_array_fixed_jump"

  val lab_maybe_trace_array_field = V.namedvar "gc_maybe_trace_array_field"
  val lab_trace_array_field = V.namedvar "gc_trace_array_field"
  val lab_maybe_mark_array_field = V.namedvar "gc_maybe_mark_array_field"
  val lab_mark_array_field = V.namedvar "gc_mark_array_field"
  val lab_maybe_push_array_field = V.namedvar "gc_maybe_push_array_field"
  val lab_push_array_field = V.namedvar "gc_push_array_field"

  val lab_trace_array_two = V.namedvar "gc_trace_array_two"
  val lab_trace_array_two_other = V.namedvar "gc_trace_array_two_other"
  val lab_maybe_mark_array_two = V.namedvar "gc_maybe_mark_array_two"
  val lab_push_array_two = V.namedvar "gc_push_array_two"

  val lab_maybe_mark_tagged = V.namedvar "gc_maybe_mark_tagged"
  val lab_maybe_trace_one = V.namedvar "gc_maybe_trace_one"

  val lab_sweep = V.namedvar "gc_sweep"
  val lab_free_object = V.namedvar "gc_free_object"
  val lab_sweep_object = V.namedvar "gc_sweep_object"
  val tab_sweep_jump = V.namedvar "gc_sweep_jump"
  val lab_unmark_object = V.namedvar "gc_unmark_object"

  val lab_finish_sweep = V.namedvar "gc_finish_sweep"
  val lab_threshold_too_large = V.namedvar "gc_threshold_too_large"
  val lab_threshold_too_small = V.namedvar "gc_threshold_too_small"
  val lab_cleanup = V.namedvar "gc_cleanup"

  fun gc_check roots_length =
      [
       (* first: decrement the counter *)
       LITERAL_ADDR_SMALL (cc, lab_counter),
       ASUB (dd, zz, cc),
       (* load -1 *)
       NAND (bb, zz, zz),
       ADD (dd, dd, bb),
       (* store the new value into the global counter *)
       UPD (zz, cc, dd),
       
       (* second: check if the counter has reached zero *)
       (* put the gc_collect label in bb *)
       LITERAL_ADDR (bb, lab_collect, cc),

       (* move the roots information into a register on the off chance we are
         actually about to start a collection *)
       LITERAL (cc, wtoi roots_length),

       (* now conditional move, overwriting the target of the following jump
         unless the counter has reached zero *)
       (if !paranoid then MANY [] (* Always jump in paranoid mode! *)
        else CMOV (bb, gc_returnreg, dd)),
       
       (* finally: do the jump! *)
       LOADPROG (zz, bb)
      ]

  structure RH = RegHelper

  fun gc_alloc_common t1 t2 res_reg sh =
      (* res_reg currently contains the computed size for the new object *)
      (ALLOC (res_reg, res_reg))::
      (* set flag bits according to shape *)
      (case sh of
         UNTRACED => 
          (* untraced objects always start with all bits unset, and fresh
             allocations should already be zeroed *)
         nil
       | _ => 
         [
          LITERAL (t2, flags_offset),
          LITERAL (t1, shape_to_flags sh),
          UPD (res_reg, t2, t1)
         ])
      @
      (if statistics then
         [
          LITERAL_ADDR_SMALL (t1, lab_alloc_count),
          ASUB (t1, zz, t1),
          LITERAL (t2, 0w1),
          ADD (t2, t1, t2),
          LITERAL_ADDR_SMALL (t1, lab_alloc_count),
          UPD (zz, t1, t2)
         ]
       else nil)
      @
      [ 
       (* add the new object to the unfree list *)
       LITERAL_ADDR_SMALL (t1, lab_unfree_list),
       ASUB (t2, zz, t1), (* t2 is now previous most-recent object *)
       UPD (res_reg, (* next_unfree_offset *) zz, t2),
       UPD (zz, t1, res_reg) (* unfree now points to new object *)
      ]

  fun gc_alloc t1 t2 res_reg size_reg sh =
      [
       (* compute size; allocate space for object + header *)
       LITERAL (res_reg, wtoi gc_header_size),
       ADD (res_reg, size_reg, res_reg)
      ]
      @ gc_alloc_common t1 t2 res_reg sh

  fun gc_alloc_static t1 t2 res_reg size sh =
      [
       LITERAL (res_reg, wtoi (size + gc_header_size))
      ]
      @ gc_alloc_common t1 t2 res_reg sh

  fun gc_alloc_common' wb size unlocksize sh =
      let val res = RH.getreg wb
      in
        (* res_reg currently contains the computed size for the new object *)
        RH.emit wb (ALLOC (res, size));
        if unlocksize then RH.unlock wb size else ();
        (* set flag bits according to shape *)
        (case sh of
           UNTRACED => 
           (* untraced objects always start with all bits unset, and fresh
              allocations should already be zeroed *)
           ()
         | _ => 
           let val off = RH.getlitro wb flags_offset
               val sh = RH.getlitro wb (shape_to_flags sh)
           in
             RH.emit wb (UPD (res, off, sh));
             RH.unlock wb off;
             RH.unlock wb sh
           end);
        (if statistics then
           let val t1 = RH.getreg wb
               val t2 = RH.getreg wb
           in
             RH.emit wb (LITERAL_ADDR_SMALL (t1, lab_alloc_count));
             RH.emit wb (ASUB (t1, zz, t1));
             RH.emit wb (LITERAL (t2, 0w1));
             RH.emit wb (ADD (t2, t1, t2));
             RH.emit wb (LITERAL_ADDR_SMALL (t1, lab_alloc_count));
             RH.emit wb (UPD (zz, t1, t2));
             RH.unlock wb t1;
             RH.unlock wb t2
           end
         else ());
        (* add the new object to the unfree list *)
        let val off = RH.getaddrro wb lab_unfree_list
            val old = RH.getreg wb
        in
          RH.emit wb (ASUB (old, zz, off)); (* t2 is now previous most-recent object *)
          RH.emit wb (UPD (res, (* next_unfree_offset *) zz, old));
          RH.emit wb (UPD (zz, off, res)); (* unfree now points to new object *)
          RH.unlock wb off;
          RH.unlock wb old
        end;
        res
      end

  fun gc_alloc' wb size sh f =
      let val hdr = RH.getlitro wb (wtoi gc_header_size) 
        (* compute size; allocate space for object + header *)
          val () = RH.emit wb (ADD (size, size, hdr))
          val () = RH.unlock wb hdr
          val res = gc_alloc_common' wb size false sh
          val x = f res
      in
        RH.unlock wb res;
        x
      end

  fun gc_alloc_static' wb size sh f =
      let val size = RH.getlitro wb (wtoi (size + gc_header_size))
          val res = gc_alloc_common' wb size true sh
          val x = f res
      in
        (* size unlocked by gc_alloc_common *)
        RH.unlock wb res;
        x
      end


(*
notes:
  assume that alloc and free are very expensive
    ==> avoid pushing and popping from mark stack
    ==> mark before push, never push a previously marked object
    ==> only push objects that contain pointers

  assume that ASUB and UPD are expensive
    ==> keep everything in registers

  ignore memory effects
    ==> don't worry about traversal order

  ? how expensive are branches?
*)

  val setup_code =
      let val mark_stack = aa
          val current_offset = bb
          val last_offset_plus_one = cc
          val object = dd
          val t1 = ee
          val t2 = ff
          val t3 = hh
      in
        [
         (lab_maybe_mark_root,
          [
           (* if current_offset = last_offset_plus_one then jump to the
            tracing code *)
           (* Use object as a temporary just until the jump. *)
           SUB (object, last_offset_plus_one, current_offset, t3),
           JZ (object, lab_maybe_pop, t1, t2, t3),

           (* INV: current_offset is a valid offset *)
           (* fetch the next root *)
           ASUB (object, zz, current_offset),

           (* then add one to the current_offset *)
           LITERAL (t1, 0w1),
           ADD (current_offset, current_offset, t1),

           (* branch on whether or not the root is null *)
           LITERAL_ADDR (t1, lab_maybe_mark_root, t3),
           LITERAL_ADDR (t2, lab_mark_root, t3),
           CMOV (t1, t2, object),
           LOADPROG (zz, t1)
          ]),

         (lab_mark_root, (* INV: object is non-null *)
          [
           (* PERF this code will push duplicates in the root set onto the
            stack multiple times: this is correct, but perhaps not as
            efficient as it could be.  if duplicate roots are common then we
            should add a check, but if they are uncommon, it's probably better
            just to add them (and avoid the extra branch). *)

           (* mark the object *)
           LITERAL (t1, flags_offset),
           ASUB (t3, object, t1), (* t3 = flags *)
           (* mark_bit = flags_offset *)
           (* LITERAL (t1, mark_bit), *)
           OR (t3, t3, t1, t2),
           (* LITERAL (t1, flags_offset), *)
           UPD (object, t1, t3),

           (* mask original flags with pointer bits just in case the object
            appeared in multiple roots *)
           LITERAL (t1, any_traced_bits),
           AND (t3, t3, t1),

           LITERAL_ADDR_SMALL (t1, lab_maybe_mark_root),
           LITERAL_ADDR_SMALL (t2, lab_push_root),
           CMOV (t1, t2, t3),
           LOADPROG (zz, t1)
          ]),

         (lab_push_root, (* INV : object is non-null and contains pointers *)
          [
           (* allocate a new cell *)
           LITERAL (t2, 0w2),
           ALLOC (t2, t2),

           (* store the root and a pointer to the mark_stack *)
           UPD (t2, zz, object),
           LITERAL (t1, stack_next_offset),
           UPD (t2, t1, mark_stack),
             (* t1 is still live *)

           (* update the mark stack *)
           CMOV (mark_stack, t2, t1), (* not conditional as t1 = 1 *)

           (* continue with the next root, if any *)
           LITERAL_ADDR (t1, lab_maybe_mark_root, t3),
           LOADPROG (zz, t1)
          ])
        ]
      end

  val tracing_code =
      (* set up register conventions within the tracing code.  these may be
       violated within basic blocks, but those violations will always be
       let-bound below. *)
      let val mark_stack = aa
          val parent_object = bb
          val object = cc
          val field_offset = dd
          val t1 = ee
          val t2 = ff
          val t3 = hh
      in
        [
         (lab_maybe_pop, (* INV: mark_stack points to a linked list or 
                         is null *)
          [
           (* pop an object off the mark stack, if any *)
           LITERAL_ADDR (t1, lab_sweep, t3),
           LITERAL_ADDR (t2, lab_pop, t3),
           CMOV (t1, t2, mark_stack),
           LOADPROG (zz, t1)
          ]),

         (lab_pop, (* INV: mark_stack contains at least one object *)
          [
           (* pop an element off of the mark_stack *)
           ASUB (object, mark_stack, zz),
           LITERAL (t2, stack_next_offset),
           ASUB (t1, mark_stack, t2),
           (* free the top cell on the mark stack *)
           FREE (mark_stack),
           CMOV (mark_stack, t1, t2), (* not conditional as t2 = 1 *)

           (* now go ahead and trace the object *)
           LITERAL_ADDR (t1, lab_trace, t3),
           LOADPROG (zz, t1) (* PERF fall-through? *)
          ]),

         (lab_trace, (* INV: object is non-null, marked, and contains pointers *)
          let val t4 = field_offset in
          [
           (* inspect the flags to determine the shape *)
           LITERAL (t1, flags_offset),
           ASUB (t4, object, t1), (* t4 = flags *)

           (* PERF jump table *)
           (* branch on the shape of the object *)
           LITERAL (t1, array_traced_bit),
           AND (t4, t4, t1), (* t4 > 0 iff object is an array *)

           LITERAL_ADDR (t1, lab_trace_tagged, t3),
           LITERAL_ADDR (t2, lab_trace_array, t3),
           CMOV (t1, t2, t4),
           LOADPROG (zz, t1)
          ]
          end),

         (lab_trace_array, (* INV: object is a non-null, marked array *)
          [
           (* not conditional as object is not null *)
           CMOV (parent_object, object, object),
           LITERAL (t1, array_length_offset),
           ASUB (object, parent_object, t1),
               (* object is now the length of the array *)
           ADD (field_offset, object, t1), 
               (* field_offset is now the offset of the last field *)
           (* PERF same as array_length_offset *)
           LITERAL (t2, max_fixed_array_size),
           SUB (t3, t2, object, t1),

           (* jump right in and start at the last field; note that we DON'T
            use maybe_trace since zero-length arrays are handled by the fixed
            case *)
           JLZ_SMALL (t3, lab_trace_array_field, t1, t2),

           LITERAL_ADDR_SMALL (t2, tab_array_fixed_jump),
           ADD (t1, t2, object),
           ASUB (t1, zz, t1),

           (* load the value of the first field to trace *)
           ASUB (object, parent_object, field_offset),
           LOADPROG (zz, t1),
           
           LABEL tab_array_fixed_jump,
           DATALAB lab_maybe_pop, (* i.e. trace_array_zero *)
           DATALAB lab_maybe_trace_one,
           DATALAB lab_trace_array_two
          ]),

         (* PERF move this case for fall-thru opportunities *)
         (lab_maybe_trace_array_field, (* INV: parent_object is non-null *)
          [
           (* check to see if there is another field or if we have reached the
            end *)
           LITERAL (t1, array_length_offset),
           (* Use object as a temporary just until the jump. *)
           SUB (object, field_offset, t1, t2), 
               (* object = 0 iff field_offset = array_length_offset
                         iff there are no more fields to trace *)

           LITERAL_ADDR (t1, lab_maybe_pop, t3),
           LITERAL_ADDR (t2, lab_trace_array_field, t3),
           CMOV (t1, t2, object),
           LOADPROG (zz, t1)
          ]),

         (* PERF special case the last field (as we can now assume there is at
           least one) *)
          (lab_trace_array_field, (* INV: parent_object is non-null, 
                                   field_offset is an offset to a field *)
          [
           (* make the field at the given offset the current object under
            scrutiny *)
           ASUB (object, parent_object, field_offset),

           (* subtract one from offset *)
           (* PERF this traversal order would favor layouts that put spines at
            index 0 *)
           DEC (field_offset, t1),

           (* if the field is null then just repeat, otherwise, maybe mark
            it. *)
           LITERAL_ADDR (t1, lab_maybe_trace_array_field, t3),
           LITERAL_ADDR (t2, lab_maybe_mark_array_field, t3),
           CMOV (t1, t2, object),
           LOADPROG (zz, t1)
          ]),

         (lab_maybe_mark_array_field, (* INV: object is not null *)
          [
           (* PERF jump table *)
           (* get the flags of the object *)
           LITERAL (t1, flags_offset),
           ASUB (t2, object, t1), (* t2 = flags *)
           (* mark_bit = flags_offset *)
           (* LITERAL (t1, mark_bit), *)
           AND (t3, t1, t2), (* t3 = 0 iff obj is not marked *)
           
           (* only continue to mark if the object is not yet marked *)
           LITERAL_ADDR_SMALL (t1, lab_mark_array_field),
           LITERAL_ADDR_SMALL (t2, lab_maybe_trace_array_field),
           CMOV (t1, t2, t3),
           LOADPROG (zz, t1)
          ]),

         (lab_mark_array_field, (* INV: object is non-null and not marked *)
          [
           (* mark the object *)
           LITERAL (t1, flags_offset),
           ASUB (t3, object, t1), (* t3 = original flags *)
           (* mark_bit = flags_offset *)
           (* LITERAL (t1, mark_bit), *)
           (* it's safe to use ADD instead of OR here since we know the object
            was not yet marked. *)
           ADD (t2, t3, t1), (* preserve the original flags! *)
           UPD (object, t1, t2),
         
           (* only push if this field contains pointers, otherwise continue
            with the next field.  original flags > 0 iff contains pointers *)
           LITERAL_ADDR_SMALL (t1, lab_maybe_trace_array_field),
           LITERAL_ADDR_SMALL (t2, lab_maybe_push_array_field),
           CMOV (t1, t2, t3),
           LOADPROG (zz, t1)
          ]),

         (lab_maybe_push_array_field, (* INV: object is non-null, marked, 
                                      and contains pointers *)
          [
           (* if field_offset = array_length_offset then rather than push the
            final field on the mark stack (and then immediately pop it off),
            jump directly to the trace-one-object code. *)
           LITERAL (t1, array_length_offset),
           SUB (t3, field_offset, t1, t2), 
               (* t3 = 0 iff field_offset = array_length_offset *)

           LITERAL_ADDR_SMALL (t1, lab_trace),
           LITERAL_ADDR_SMALL (t2, lab_push_array_field),
           CMOV (t1, t2, t3),
           LOADPROG (zz, t1)
          ]),

         (lab_push_array_field, (* INV: object is non-null, marked, contains
                                pointers, and is not the last field of this
                                parent *)
          [
           (* allocate a new list element *)
           LITERAL (t1, 0w2),
           ALLOC (t1, t1),
           UPD (t1, zz, object),
           LITERAL (t2, stack_next_offset),
           UPD (t1, t2, mark_stack),
           CMOV (mark_stack, t1, t1), (* not conditional as t1 is non-null *)

           LITERAL_ADDR (t1, lab_trace_array_field, t3),
           LOADPROG(zz, t1)                
          ]),

          (lab_trace_array_two, (* INV: parent_object is non-null, 
                                 has exactly two fields and field_offset
                                 is the offset of the second field *)
          [
           (* get the offset of the other field *)
           (* PERF this traversal order would favor layouts that put spines at
            index 0 *)
           (* ENH is there a better/faster way of computing this? *)
           DEC (field_offset, t1),

           (* save the other field as field_offset (since that is the only
           reason we would need parent_object and field_offset *)
           ASUB (field_offset, parent_object, field_offset),

           (* if the field is null then just repeat, otherwise, maybe mark
            it. *)
           LITERAL_ADDR (t1, lab_trace_array_two_other, t3),
           LITERAL_ADDR (t2, lab_maybe_mark_array_two, t3),
           CMOV (t1, t2, object),
           LOADPROG (zz, t1)
          ]),

         (lab_trace_array_two_other, (* INV: field_offset is null
                                               or a pointer
                                        and t1 is non-zero *)
          [
           CMOV (object, field_offset, t1),
           LITERAL_ADDR_SMALL (t1, lab_maybe_trace_one),
           LOADPROG (zz, t1)
          ]),

         (lab_maybe_mark_array_two, (* INV: object is not null,
                                       field_offset is possibly
                                         another object*)
          let val flags = parent_object in
          [
           (* PERF jump table *)
           (* get the flags of the object *)
           LITERAL (t1, flags_offset),
           ASUB (flags, object, t1),
           (* mark_bit = flags_offset *)
           (* LITERAL (t1, mark_bit), *)
           AND (t3, t1, flags), (* t3 = 0 iff obj is not marked *)
           
           (* only continue to mark if the object is not yet marked *)
           JNZ_SMALL (t3, lab_trace_array_two_other, t1, t2),

           (* mark the object *)
           LITERAL (t1, flags_offset),
           (* mark_bit = flags_offset *)
           (* LITERAL (t1, mark_bit), *)
           (* it's safe to use ADD instead of OR here since we know the object
            was not yet marked. *)
           ADD (t2, flags, t1), (* preserve the original flags! *)
           UPD (object, t1, t2),
         
           (* only push if this field contains pointers, otherwise continue
            with the next field.  original flags > 0 iff contains pointers *)
           LITERAL_ADDR_SMALL (t1, lab_trace_array_two_other),
           LITERAL_ADDR_SMALL (t2, lab_push_array_two),
           CMOV (t1, t2, flags),
           LOADPROG (zz, t1)
          ]
          end),

         (lab_push_array_two, (* INV: object is non-null, marked, contains
                                pointers, and field_offset is possibly 
                                another object *)
          [
           (* allocate a new list element *)
           LITERAL (t1, 0w2),
           ALLOC (t1, t1),
           UPD (t1, zz, object),
           LITERAL (t2, stack_next_offset),
           UPD (t1, t2, mark_stack),
           CMOV (mark_stack, t1, t1), (* not conditional as t1 is non-null *)

           LITERAL_ADDR (t1, lab_trace_array_two_other, t3),
           LOADPROG(zz, t1)                
          ]),

         (lab_trace_tagged, (* INV: object is non-null, marked, tagged 
                             pointer *)
          let val flags = field_offset 
              val t4 = parent_object in
          [
           (* load the pointer contained within this object and continue by
            tracing the object that pointer points to *)
           LITERAL (t1, tagged_pointer_offset), 
           ASUB (object, object, t1),

           LABEL lab_maybe_trace_one, (* INV: object is null or pointer *)

           (* PERF jump table? *)
           (* check to see whether or not the object is null *)
           JZ (object, lab_maybe_pop, t1, t2, t3),

           (* INV: object is non-null *)
           (* look up the current flags *)
           LITERAL (t1, flags_offset),
           ASUB (flags, object, t1),
           (* mark_bit = flags_offset *)
           (* LITERAL (t1, mark_bit), *)
           AND (t4, flags, t1), (* t4 = 0 iff obj is not marked *)

           (* set the mark bit unconditionally *)
           OR (t2, flags, t1, t3), (* preserve the original flags *)
             (* t2 is the new flags *)
           (* LITERAL (t1, flags_offset), *)
           (* update the flags *)
           UPD (object, t1, t2),
           
           (* branch depending on whether or not the object is already marked
            and whether the object contains pointers... note the double
            CMOV!! *)
           LITERAL_ADDR (t1, lab_maybe_pop, t3),
           LITERAL_ADDR (t2, lab_trace, t3),
           CMOV (t2, t1, t4),
           CMOV (t1, t2, flags),
           LOADPROG (zz, t1)
          ]
          end)
        ]
      end

  val sweeping_code =
      let val current_object = cc
          val sweep_table_label = aa
          val continue_unmark_label = bb
          val t1 = ee
          val t2 = ff
          val t3 = hh
          val t4 = dd
      in
        [
         (lab_sweep, 
          [
           (* number of clock ticks during trace phase *)
           LITERAL_ADDR_SMALL (t1, lab_timestamp),
           ASUB (t3, zz, t1),
           rdtsc (t2),
           SUB (t3, t2, t3, t4),
           info (0w4161, t3),
           UPD (zz, t1, t2),

           (* load the beginning of the old unfree list *)
           LITERAL_ADDR_SMALL (t1, lab_unfree_list),
           ASUB (current_object, zz, t1),
           (* there is no previous object *)
           UPD (zz, t1, zz),

           (* no live objects yet! *)
           LITERAL_ADDR_SMALL (t2, lab_live_object_count),
           UPD (zz, t2, zz),

           (* save this value in case anyone else depends on it *)
           LITERAL (t2, flags_offset),
           ASUB (t3, zz, t2),
           LITERAL_ADDR_SMALL (t1, lab_spilled_text),
           UPD (zz, t1, t3),
           LITERAL (t4, 0w6), (* must agree with value in table below! *)
           UPD (zz, t2, t4),

           (* keep these labels around, as we use then often *)
           LITERAL_ADDR_SMALL (sweep_table_label, tab_sweep_jump),
           LITERAL_ADDR_SMALL (continue_unmark_label, lab_unmark_object),

           (* the code below requires t1 to contains flags_offset *)
           LITERAL (t1, flags_offset),

           LITERAL_ADDR (t2, lab_sweep_object, t3),
           LOADPROG (zz, t2)
          ]),

         (lab_free_object, (* INV: current_object is non-null and unmarked *)
          (* PERF unfold this loop swapping current_object and t2 *)
          let val offset = t1 in
          [
           ASUB (t2, current_object, (* next_unfree_offset *) zz),
           FREE (current_object),
           (* PERF this could be faster if we made one version of
             maybe_sweep/free for current_object and one for t2 *)
           CMOV (current_object, t2, current_object),
             (* not conditional as current_object is non-null *)

           MANY (if statistics then [
                                     LITERAL_ADDR_SMALL (t4, lab_dead_object_count),
                                     ASUB (t2, zz, t4),
                                     LITERAL (t3, 0w1),
                                     ADD (t2, t2, t3),
                                     UPD (zz, t4, t2)
                                     ]
                 else nil),

           (* continue with the next object *)
           LABEL lab_sweep_object,
           (* NB if current_object is null, we've already set the right word
            of the zero array to 110. *)
           (* PERF there is an extra register now *)

           ASUB (t3, current_object, offset),
           ADD (t4, t3, sweep_table_label),
           ASUB (t4, zz, t4),
           LOADPROG (zz, t4),

           LABEL tab_sweep_jump,
           DATALAB lab_free_object,  (* 000 *)
           DATALAB lab_unmark_object,(* 001 *)
           DATALAB lab_free_object,  (* 010 *)
           DATALAB lab_unmark_object,(* 011 *)
           DATALAB lab_free_object,  (* 100 *)
           DATALAB lab_unmark_object,(* 101 *)
           DATALAB lab_finish_sweep, (* 110 *)

           LABEL lab_unmark_object,
           (* INV: current_object is non-null and marked *)
           (* clear the mark bit *)
           LITERAL (t2, non_mark_bits),
           AND (t3, t3, t2), (* t3 = new flags *)
           UPD (current_object, offset, t3),

           (* remember the next object in the old unfree list *)
           ASUB (t2, current_object, (* next_unfree_offset *) zz),
           
           (* link the object into the new unfree list *)
           LITERAL_ADDR_SMALL (t4, lab_unfree_list),
           ASUB (t3, zz, t4),
           UPD (current_object, (* next_unfree_offset *) zz, t3),
           UPD (zz, t4, current_object),

           (* update current_object *)
           CMOV (current_object, t2, current_object),
              (* not conditional as current_object is non-null *)

           (* increment counter of live objects *)
           (* flags_offset = 1 *)
           LITERAL_ADDR_SMALL (t2, lab_live_object_count),
           ASUB (t3, zz, t2),
           ADD (t3, t3, offset),
           UPD (zz, t2, t3),

           (* continue with next object *)
           LITERAL_ADDR (t4, lab_sweep_object, t3),
           LOADPROG (zz, t4)
          ]
          end)
        ]
      end

  fun gc_code roots_start mainlab =
      [
       (lab_data,
        [
         (* contains a pointer to first object in the heap *)
         LABEL lab_unfree_list,
         DATA 0w0,
         (* contains the number of function calls until next collection *)
         LABEL lab_counter,
         DATA 0w0,
         (* contains the number of function calls between collections *)
         LABEL lab_last_counter,
         DATA 0w0,
         (* contains the number of live objects at the end of the previous 
           collection *)
         LABEL lab_last_heap_size,
         DATA 0w0,
         (* used to implement internal GC calling convention *)
         LABEL lab_return_addr,
         DATA 0w0,
         LABEL lab_first_saved_reg,
         DATA 0w0,
         LABEL lab_second_saved_reg,
         DATA 0w0,
         LABEL lab_last_saved_reg,
         DATA 0w0,
         (* spilled vars used during sweeping *)
         LABEL lab_live_object_count,
         DATA 0w0,
         LABEL lab_spilled_text,
         DATA 0w0,
         (* used to count instructions in various phases *)
         LABEL lab_timestamp,
         DATA 0w0,
         LABEL lab_dead_object_count,
         DATA 0w0,
         LABEL lab_alloc_count,
         DATA 0w0
        ]),

       (gc_init, 
        [
         LITERAL_ADDR_SMALL (bb, lab_unfree_list),
         UPD (zz, bb, zz),

         LITERAL_ADDR_SMALL (bb, lab_counter),
         LITERAL (cc, min_function_call_threshold), 
         UPD (zz, bb, cc),

         LITERAL_ADDR_SMALL (bb, lab_last_counter),
         UPD (zz, bb, cc),

         LITERAL_ADDR_SMALL (bb, lab_last_heap_size),
         UPD (zz, bb, zz),

         LITERAL_ADDR(bb, mainlab, aa),
         LOADPROG (zz, bb)
        ]),

       (lab_collect, (* gc_returnreg contains the return address,
                     cc = number of roots *)
        [
         (* save the return address *)
         LITERAL_ADDR_SMALL (bb, lab_return_addr),
         UPD (zz, bb, gc_returnreg),

         (* save the other registers *)
         LITERAL_ADDR_SMALL (bb, lab_first_saved_reg),
         UPD (zz, bb, ee),
         LITERAL_ADDR_SMALL (bb, lab_second_saved_reg),
         UPD (zz, bb, ff),
         LITERAL_ADDR_SMALL (bb, lab_last_saved_reg),
         UPD (zz, bb, hh),

         (* number of roots *)
         info (0w4096, cc),

         (* number of clock ticks since end of last collection *)
         LITERAL_ADDR_SMALL (bb, lab_timestamp),
         ASUB (ee, zz, bb),
         rdtsc (aa),
         SUB (ee, aa, ee, hh),
         info (0w4160, ee),
         UPD (zz, bb, aa),

         (* mark roots and add them to mark stack *)
         (* initialize the mark stack *)
         LITERAL (aa, 0w0),
         (* set the pointer to the current root *)
         LITERAL (bb, wtoi roots_start),
         (* add the roots_start to the number of roots to yield the offset
          just past the last root *)
         ADD (cc, cc, bb),

         LITERAL_ADDR (ee, lab_maybe_mark_root, hh),
         LOADPROG (zz, ee)
        ]),

       (lab_finish_sweep, (* XXX DOC bb = number of live objects *)
        let val target_ratio = case W.fromString (!target_ratio)
                                of SOME w => w | NONE => raise GC "invalid target ratio"
        in
        [
         LITERAL_ADDR_SMALL (ee, lab_spilled_text),
         ASUB (ff, zz, ee),
         LITERAL (hh, flags_offset),
         UPD (zz, hh, ff),

         LITERAL_ADDR_SMALL (ee, lab_live_object_count),
         ASUB (bb, zz, ee),

         (* if there weren't any live objects around after the previous
          collection, skip computation of a new function call threshold *)
         LITERAL_ADDR_SMALL (ee, lab_last_heap_size),
         ASUB (ff, zz, ee),
         JZ (ff, lab_cleanup, cc, ee, hh)
        ]
         (* alternative: we could compute the new threshold as
              hh = max (last counter * 2L / (L + D + 1), function_call_threshold) 
          but that would require tracking the number of dead objects and we
          currently don't have enough registers to do that. *)
        @ (if statistics then
                  [
                   (* look up the old counter threshold *)
                   LITERAL_ADDR_SMALL (ee, lab_last_counter),
                   ASUB (hh, zz, ee),

                   MUL (hh, bb, hh),
                   LITERAL (aa, 0w2),
                   MUL (hh, aa, hh),
                   
                   LITERAL (aa, 0w1),
                   ADD (aa, aa, bb),

                   LITERAL_ADDR_SMALL (ee, lab_dead_object_count),
                   ASUB (cc, zz, ee),
                   ADD (aa, aa, cc),
                   
                   DIV (hh, hh, aa),
                   info (0w4353, hh)
                  ]
           else nil) @
        [
         (* actual: compute hh = last counter * current heap size / last heap size *)
         LITERAL_ANY (hh, W.div (0wxFFFFFFFF, target_ratio), cc),
         SUB (ff, hh, bb, cc),
         JLZ (ff, lab_threshold_too_large, cc, dd, ee),

         (* ok, no chance of overflow; go ahead and compute the new threshold *)
         LITERAL (hh, target_ratio),
         MUL (hh, bb, hh),

         (* check to see if the new threshold is below min_function_call_threshold *)
         LITERAL (aa, min_function_call_threshold),
         SUB (ff, hh, aa, cc),
         JLZ (ff, lab_threshold_too_small, cc, dd, ee),

         (* set the new threshold *)
         LITERAL_ADDR_SMALL (ee, lab_last_counter),
         UPD (zz, ee, hh),

         LITERAL_ADDR (ee, lab_cleanup, hh),
         LOADPROG (zz, ee)
        ]
        end),

       (lab_threshold_too_large,
        [
         (* if the new threshold is too large to compute in 32 bits, then
          use a really large number instead *)
         LITERAL_ADDR_SMALL (ee, lab_last_counter),
         LITERAL_ANY (aa, max_function_call_threshold, dd),
         UPD (zz, ee, aa),

         LITERAL_ADDR (ee, lab_cleanup, hh),
         LOADPROG (zz, ee)
        ]),

       (lab_threshold_too_small,
        [
         (* if the new threshold is too small then overwrite it with the minimum *)
         LITERAL_ADDR_SMALL (ee, lab_last_counter),
         LITERAL (aa, min_function_call_threshold),
         UPD (zz, ee, aa),

         LITERAL_ADDR (ee, lab_cleanup, hh),
         LOADPROG (zz, ee)
        ]),

       (lab_cleanup,
        [
         (* store the size of the heap and reset the counter *)
         LITERAL_ADDR_SMALL (ee, lab_last_heap_size),
         UPD (zz, ee, bb),
         (* heap size i.e. number of live objects *)
         info (0w4224, bb),

         LITERAL_ADDR_SMALL (ee, lab_dead_object_count),
         ASUB (ff, zz, ee),
         (* number of objects collected in this cycle -- only meaningful if
         statistics is on *)
         info (0w4225, ff),
         UPD (zz, ee, zz),

         LITERAL_ADDR_SMALL (ee, lab_alloc_count),
         ASUB (ff, zz, ee),
         (* number of objects allocated in this cycle -- only meaningful if
         statistics is on *)
         info (0w4226, ff),
         UPD (zz, ee, zz),

         LITERAL_ADDR_SMALL (ee, lab_last_counter),
         ASUB (ff, zz, ee),
         (* number of function calls until next gc *)
         info (0w4352, ff),

         LITERAL_ADDR_SMALL (ee, lab_counter),
         UPD (zz, ee, ff),

         (* number of clock ticks during sweep phase *)
         LITERAL_ADDR_SMALL (bb, lab_timestamp),
         ASUB (ee, zz, bb),
         rdtsc (aa),
         SUB (ee, aa, ee, hh),
         info (0w4162, ee),
         UPD (zz, bb, aa),

         (* restore registers *)
         LITERAL_ADDR_SMALL (bb, lab_first_saved_reg),
         ASUB (ee, zz, bb),
         LITERAL_ADDR_SMALL (bb, lab_second_saved_reg),
         ASUB (ff, zz, bb),
         LITERAL_ADDR_SMALL (bb, lab_last_saved_reg),
         ASUB (hh, zz, bb),

         (* load return address *)
         LITERAL_ADDR_SMALL (bb, lab_return_addr),
         ASUB (aa, zz, bb),

         (* goodbye for now! *)
         LOADPROG (zz, aa)
        ]),

       (gc_end, 
        [
         LOADPROG (zz, gc_returnreg)
        ])
      ]
      @
      setup_code
      @
      tracing_code
      @
      sweeping_code

end
