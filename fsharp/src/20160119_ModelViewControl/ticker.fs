module MyTicker

type public ITicker =
  abstract member tick : unit -> int

/// This function can be called by all
let public generateTicker () =
  let mutable count = 0
  {new ITicker with
   member x.tick () = count <- count + 1; count}

/// This function can only be called from this file and in this module
let private f x = x**2.0

/// This function can only be called from the same dll or exe
let internal g x = 1/x
