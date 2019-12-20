type example () =
  static let mutable i = 0
  do printfn "I'm non-static: %d" i
  do i <- i + 1
  static do printfn "I'm static: %d" i
  static do i <- i + 1
let a = example()
let b = example()
  
