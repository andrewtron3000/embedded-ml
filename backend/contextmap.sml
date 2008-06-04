(* manage variable mapping *)

structure ContextMap =
struct

open CPS

exception ContextMap of string

datatype context_rc = Found of int 
                    | NotFound of int

(* contextLength returns the length of the context *)

fun contextLength (ctxt) = List.length(ctxt)

(* returnContextList returns the position of the variable v if it
   exists in the context.  If v doesn't exist in the context,
   returnContextList returns a negative number corresponding to the
   first open slot in the context. *)

fun returnContextList ({var=v'}::cs, position, v) =
    if Variable.eq(v',v) then (Found position) else returnContextList(cs, (position + 1), v)
  | returnContextList (nil, position, v) = (NotFound position)

(* findInContext tries to find a variable in the context *)
fun findInContext (ctxt, v) = 
    let
        val posn = returnContextList(ctxt, 0, v)
    in
        case posn of NotFound p => raise ContextMap ("Couldn't find " ^ (Variable.tostring v) ^ " in context")
                   | Found p =>  {position = p, context = ctxt}
    end

(* addToContext adds a variable to the context *)
fun addToContext (ctxt, v) =
    let
        val p = length(ctxt)
        val () = if p >= Limits.STACK_SIZE 
                 then raise ContextMap "exceeded maximum context length"
                 else ()
    in
        {position = p, context = ctxt @ [{var = v}]}
    end

(* *)
fun initializeContext (c, (v::vs)) =
    let
        val new_context = #context(addToContext(c, v))
    in 
        initializeContext(new_context, vs)
    end
  | initializeContext(c, nil) = c
                                
(* *)
fun toString ({var=v'}::cs) = 
    "[" ^ Variable.tostring (v') ^ "]" ^ " " ^ toString (cs)
  | toString (nil) = ""
                    
(* moves a variable to the end of the context *)
fun rebindOneVar (context, var) =
    let
        val context_info = findInContext(context, var)
        val posn = #position(context_info)
        val (first_part, remainder_part) = if posn < length(context) then
                                               (List.take(context, posn),
                                                List.drop(context, posn+1))
                                           else
                                               (context, nil)
        val context_without_var = first_part @ ({var=Variable.newvar ()} :: remainder_part)
    in
        #context(addToContext(context_without_var, var))
    end

fun padContextOnce (context) = context @ [{var=Variable.newvar ()}]

fun padContext (context, n) = 
    if n > 0 then
        padContext(padContextOnce(context), n-1)
    else
        context

end

(* test stuff below *)
(* val context = [ {var = "var1"}, {var = "var2"} ]; *)
(* findVInContext(context, "var5"); *)

