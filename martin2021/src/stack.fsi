module Stack

type 'a stack
val empty : unit -> 'a stack
val push  : 'a stack -> 'a -> 'a stack
val pop   : 'a stack -> ('a * 'a stack) option
