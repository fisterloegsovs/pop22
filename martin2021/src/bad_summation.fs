let lst = List.init 50000 (fun x -> x)         // BAD
let mutable i = 0                             // BAD
let mutable sum = 0                          // BAD
while (i < List.length lst) do              // BAD
  sum <- sum + lst.[i]                       // BAD
  i <- i + 1                                  // BAD
printf "%d\n" sum                              // BAD
