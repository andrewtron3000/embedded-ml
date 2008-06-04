
signature CPSPRINT =
sig

    val etosil : int -> CPS.cexp -> string list
    val etosi : int -> CPS.cexp -> string
    val printe : CPS.cexp -> unit
    val writee : string -> CPS.cexp -> unit
    val ttos : CPS.tag -> string
    val vtos : CPS.value -> string

end