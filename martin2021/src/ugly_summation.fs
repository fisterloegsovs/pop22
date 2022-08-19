let lst = List.init 50000 (fun x -> x)     // UGLY
let mutable i = 0                         // UGLY
let mutable sum = 0                      // UGLY
let len = List.length lst               // UGLY
let mutable lst2 = lst                 // UGLY
while (i < len) do                      // UGLY
  sum <- sum + List.head lst2            // UGLY
  lst2 <- List.tail lst2                  // UGLY
  i <- i + 1                               // UGLY
printf "%d\n" sum                           // UGLY
