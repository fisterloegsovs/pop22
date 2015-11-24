// Examples of list iterations

let lst = [2; 3; 1; 5];

//
printfn "Print list by printfn special syntax"
printfn "%A " lst

//
printfn "Print list by recursive iteration"
let rec printList = function
  | x::xs -> printf "%d " x; printList xs
  | _ -> printfn ""

printList lst

//
printfn "Print list by while looping"
let mutable i = 0
while i < lst.Length do
  printf "%d " lst.[i]
  i <- i + 1
printfn ""

//
printfn "Print list by for-in looping"
for e in lst do
  printf "%d " e
printfn ""

//
printfn "Print list by for-to looping"
for i = 0 to (lst.Length-1) do
  printf "%d " lst.[i]
printfn ""

//
printfn "Print list by List.iter"
let printElement x = printf "%d " x
List.iter printElement lst
printfn ""

