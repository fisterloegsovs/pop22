module Pegs

type t
val init     : int -> t
val move     : int -> int -> t -> t
val toString : t -> string

module App =
  val reset : int -> unit
  val mv    : int -> int -> unit
