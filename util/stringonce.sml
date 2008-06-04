
structure StringOnce :> STRINGONCE =
struct

    structure SM = 
        SplayMapFn(type ord_key = string val compare = String.compare)


    type stringarena =
        { sep : string,
          ctr : int ref,
          table : (int ref) SM.map ref }

    fun arena () = { sep = "", ctr = ref 0, table = ref SM.empty }

    fun arenaex sep = { sep = sep, ctr = ref 0, table = ref SM.empty }

    fun clear ({ ctr, table, ... } : stringarena) = 
        let in
            table := SM.empty;
            ctr := 0
        end

    fun ++ x = (x := !x + 1; !x)

    val uniqid  = ref 0

    (* XXX this is not optimal. 
       the best behavior would be to delay the
       choice of who gets to be 's' (without
       digits) to the first one whose f is
       called. *)

    fun symbol { sep, ctr, table } s =
        let 
            val s = (StringUtil.replace "`" "_b_" s)
            val s = (StringUtil.replace "|" "_o_" s)
            val s = (StringUtil.replace "&" "_a_" s)
            val s = (StringUtil.replace "<" "_l_" s)
            val s = (StringUtil.replace ">" "_g_" s)
            val s = (StringUtil.replace "$" "_d_" s)
            val s = (StringUtil.replace "@" "_t_" s)
            val s = (StringUtil.replace "%" "_e_" s)
            val s = (StringUtil.replace "'" "_q_" s)
            val s = (StringUtil.replace "+" "_p_" s)
            val s = (StringUtil.replace "*" "_s_" s)
            val s = (StringUtil.replace "-" "_m_" s)
            val s = (StringUtil.replace ":" "_c_" s)
            val s = (String.implode o (map Char.toLower) o String.explode) s
            val n = ( ++ uniqid ; 
                      Int.toString(!uniqid) ) 
            val s = if s = "mainentry" then s else "f" ^ n
        in
        (case SM.find (!table, s) of
             NONE =>
               let
                   val ir = ref 0
               in
                 (* first var with this name.
                    it will always be called 's'
                    *)
                 table := SM.insert(!table, s, ir);
                 (++ ctr,
                  fn () => s)
               (*
                  if !ir = 0 
                  then s
                  else s ^ sep ^ "0") *)
               end
           | SOME ir => 
               let
                   val me = ++ ir
               in


                   (++ ctr,
                    fn () =>
                    Int.toString me ^ sep ^ s)
               end)
        end
end
