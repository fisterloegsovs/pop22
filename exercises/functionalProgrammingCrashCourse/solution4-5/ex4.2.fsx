let rec sumRec n =
  match n with
    n when n > 1 -> n + sumRec (n-1)
    | _ -> 1

let rec sumWhile n =
   let mutable sum = 0
   let mutable i = n
   while i > 0 do
     sum <- sum + i
     i <- i - 1
   sum

let n = 5
printfn "sumRec %d = %d" n (sumRec n)

printfn "  n sumRec sumWhile"
for n = 1 to 10 do
  printfn "% 3d    % 3d      % 3d" n (sumRec n) (sumWhile n)
