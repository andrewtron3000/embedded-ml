
structure TestLZ =
struct
  val corpus = 
    implode [chr 255, chr 255, chr 255, chr 0, chr 0, chr 0, chr 0] ^
    "QQQ (that tests the 'special case', as does this:) ZxZxZxZxZxZxZ W00W00W00W00W Y```Y```Y````Y``Y`````Y`Y``````YY```````Y Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Typi non habent claritatem insitam; est usus legentis in iis qui facit eorum claritatem. Investigationes demonstraverunt lectores legere me lius quod ii legunt saepius. Claritas est etiam processus dynamicus, qui sequitur mutationem consuetudium lectorum. Mirum est notare quam littera gothica, quam nunc putamus parum claram, anteposuerit litterarum formas humanitatis per seacula quarta decima et quinta decima. Eodem modo typi, qui nunc nobis videntur parum clari, fiant sollemnes in futurum." ^ CharVector.tabulate (256, chr)

  val corpus = corpus ^ corpus
  val corpus = corpus ^ corpus
  val corpus = corpus ^ corpus

  val corpus = corpus ^ StringUtil.readfile "tests/pub.um"

  val corpus = corpus ^ StringUtil.readfile "COPYING"
(*  val corpus = corpus ^ corpus *)
  val corpus = corpus ^ (CharVector.map (fn x => (chr ((ord x + 1) mod 256))) corpus)
  val corpus = corpus ^ (CharVector.map (fn x => (chr ((ord x + 1) mod 256))) corpus)
(*
  val corpus = corpus ^ (CharVector.map (fn x => (chr ((ord x + 1) mod 256))) corpus)

  val corpus = corpus ^ StringUtil.readfile "humlock"
*)

  (* val corpus = "QQQQQQQQQQ" *)

  val () = print ("Uncompressed length: " ^ (Int.toString (size corpus)) ^ " bytes\n")
    
  exception Impossible

  val () =
    let

  val ctr = ref 0
  val cmp = L.compress (SimpleStream.fromstring corpus)
  val lb = SimpleStream.tovector 
    (SimpleStream.map (fn (L.CODE (x, b)) => 
                       let in
                         (* print ("CODE " ^ Int.toString x ^ "\n"); *)
                         if (!ctr mod 100000) = 0
                         then print (Int.toString (!ctr) ^ " bytes: bitlen " ^
                                     Int.toString b ^ "\n")
                         else ();
                         ctr := !ctr + 1;
                         (x, b)
                       end
                          | _ => raise Impossible) cmp)
    
  val () = print "Done compressing.\n"

  val bits = (Vector.foldl (fn ((_, a), b) => a + b) 0 lb)
  val () = print ("Compressed length: " ^ (Int.toString (Vector.length lb)) ^ " codes @ " ^
                  Int.toString bits ^ " bits = " ^
                  Int.toString (bits div 8 + (if bits mod 8 = 0 then 0 else 1)) ^ 
                  " bytes = " ^
                  Int.toString (bits div 32 + (if bits mod 32 = 0 then 0 else 1)) ^ 
                  " words\n")

(*    
  val () = Vector.appi (fn (i, (a, b)) =>
                        if i < 260
                          then
                            print ("@" ^ Int.toString i ^ " " ^ 
                                   Int.toString a ^ " : " ^ Int.toString b ^ "\n")
                          else ()) lb
*)


  val ctr = ref 0
  val i = SimpleStream.map (fn (x, b) =>
                            fn i =>
                            (* check that compressor and decompressor agree
                               on how many bits are needed for this code *)
                            if i = b
                            then (ctr := !ctr + 1; L.ICODE x)
                            else 
                              let in
                                print ("@" ^ Int.toString (!ctr) ^ " " ^
                                       "ERROR: bad bits for decoding. " ^
                                       "expect: " ^ Int.toString b ^ 
                                       " got: " ^ Int.toString i ^ "\n");
                                raise Match
                              end) (SimpleStream.fromvector lb)

  val s = let val a = L.decompressex i 
          in CharVector.tabulate (Array.length a, fn x => Array.sub(a, x)) 
          end
    
    in
      if s = corpus
      then print "Success!\n"
      else print ("FAIL: " ^ s ^ "\n")
    end handle L.LZW s => (print ("LZW ERROR: " ^ s ^ "\n");
                             raise Impossible)

end
