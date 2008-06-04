
signature VARIABLE =
sig
    exception Variable of string

    type var
        
    val newvar : unit -> var
    val namedvar : string -> var
        
    (* fails on special vars *)
    val alphavary : var -> var

    (* Some variables are really external references. They have to
       be printed with a specific name that is agreed upon with
       the runtime. They may also be qualified by a module name.
       
       this distinction probably deserves to be made explicit
       in the types, but isn't currently. *)
    val special : string option -> string -> var

    (* a special var *)
    val getspecial : var -> (string option * string) option

    val eq : var * var -> bool
    val compare : var * var -> order
        
    val basename : var -> string
        
    (* fails on special vars *)
    val tostring : var -> string

    (* works for any var, just for printing *)
    val show : var -> string

    structure Map : ORD_MAP where type Key.ord_key = var
end
