
(* keep track of symbol -> name mappings,
   so that looking at debug dumps can be made
   more pleasant *)
structure SymbolDB =
struct
(*  
  structure SSMap = 
  SplayMapFn(type ord_key = string * string
             fun compare ((a, b), (aa, bb)) =
               case String.compare (a, aa) of 
                 EQUAL => String.compare (b, bb)
               | c => c)

  val db = ref SSMap.empty : (int SSMap.map) ref
*)
  structure SM = StringMap
  structure IM = IntMap

  val db = ref SM.empty : string IM.map ref SM.map ref 

  fun clear () = db := SM.empty

  fun push cat n s =
    (case SM.find (!db, cat) of
       NONE => db := SM.insert (!db, cat, 
                                ref (IM.insert(IM.empty, n, s)))
     | SOME ib => ib := IM.insert(!ib, n, s))

  fun tofile f =
    let
      val ff = TextIO.openOut f

      val ims = SM.listItemsi (!db)
      val all = map (fn (cat, im) =>
                     (cat, IM.listItemsi (!im))) ims

      fun oneitem (i, s) =
        TextIO.output(ff, "  " ^ Int.toString i ^ " " ^ s ^ "\n")
      fun onecat (c, ism) =
        let in
          TextIO.output (ff, c ^ ":\n");
          app oneitem ism;
          TextIO.output (ff, "\n")
        end
        
    in
      app onecat all;
      TextIO.closeOut ff
    end

end
