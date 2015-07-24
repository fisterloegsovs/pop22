/// This is a comment to fact function
let rec fact = function
  | 0 -> 1
  | n -> n * fact(n-1)

let ls = [0 .. 5]
printfn "%A" ls
let facts = List.map fact ls
printfn "%A" facts

/// A zipped list
let zipped = List.zip ls facts
let printPair (a,b) = printfn "%d->%d" a b
List.map printPair zipped
