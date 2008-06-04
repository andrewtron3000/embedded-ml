
signature SYMBOLDB =
sig

  val clear : unit -> unit

  (* push category n name *)
  val push : string -> int -> string -> unit

  (* write to a file *)
  val tofile : string -> unit


end
