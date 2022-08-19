














// Emacs - brug fsharpi --readline-
// Exit fsharpi with "#quit;;"

// First example
let lst = [1;2;3;4];;
let lst2 = 5 :: List.tail (List.tail lst);;








// BAD CODE:
let lst = List.init 50000 (fun x -> x);;

let mutable i = 0
let mutable sum = 0
while (i < lst.Length) do
  sum <- sum + lst.[i]
  i <- i + 1
printf "%d\n" sum;;

// Instead:
List.fold (fun x y -> x+y) 0 lst;;
List.fold (+) 0 lst;;


// Listesummation
let sum = List.fold (+) 0 [3;6;2;5]

// Mindste element i en liste
let min x y = if x < y then x else y
let maxInt = System.Int32.MaxValue  // = 2147483647
let min_elem = List.fold min maxInt [3;6;2;5]

// List reverse
let f s x = x :: s
let rev xs = List.fold f [] xs

let ex = rev [1;2;3]

// Dot product
let vec_mul (xs:float list) ys = List.map2 (*) xs ys
let dot xs ys = List.fold (+) 0.0 (vec_mul xs ys)
let vec_len xs = sqrt (dot xs xs)
let ex = vec_len [3.0; 4.0]

// Find

let find p xs =
  List.fold (fun s x -> if s = None && p x then Some x else s)
            None xs

let ex = find (fun x -> x > 4) [3;2;5;6;45]


// For-in

let lst = List.init 50000 (fun x -> x)
let mutable sum = 0
for x in lst do sum <- sum + x
do printf "%d\n" sum


// Array summation

let arr = Array.init 50000 (fun x -> x);;

let mutable sum = 0
for x in arr do sum <- sum + x
do printf "%d\n" sum

// Mutability
let arr = [|1;2;3;4|];;

for i in [0..arr.Length-1] do arr.[i] <- arr.[i]*arr.[i]
do printf "%A\n" arr


// 2D

let a = Array2D.init 5 5 (fun r c -> (r+1) * (c+1));;

let prA (a : int[,]) =
  for r in [0..Array2D.length1 a - 1] do    // 1  2  3  4  5
    for c in [0..Array2D.length2 a - 1] do  // 2  4  6  8 10
      printf "%2d " (a.[r,c])               // 3  6  9 12 15
    printf "\n"                             // 4  8 12 16 20
do prA a                                    // 5 10 15 20 25
