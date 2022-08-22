module Queue // content of queue.fsi

type 'a queue                        // FIFO
val empty : unit -> 'a queue
val insert  : 'a queue -> 'a -> 'a queue
val remove  : 'a queue -> ('a * 'a queue) option
