let rec fib n = if n = 1 || n = 2 then 1
                else fib(n-1) + fib (n-2)
let x = fib 10

do printf "%d\n" x

// De første 10 fibonacci tal:

let xs = List.init 10 (fun x -> x + 1)

do printf "%A\n" (List.map fib xs)

let rec fib2 p1 p2 n =
  if n = 1 || n = 2 then p2
  else fib2 p2 (p1+p2) (n-1)

let y = fib2 1 1 10

do printf "%d\n" y

do printf "%A\n" (List.map (fib2 1 1) xs)
