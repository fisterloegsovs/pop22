module MyTicker

type public ITicker =
  abstract member tick : unit -> int

/// This function can be called by all
let public generateTicker () =
  let mutable count = 0
  {new ITicker with
   member x.tick () = count <- count + 1; count}
