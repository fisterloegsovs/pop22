(* http://www.wikihow.com/Calculate-Pi
 * http://math.stackexchange.com/questions/14815/why-does-this-converge-to-pi-4
 * https://en.wikipedia.org/wiki/Approximations_of_π
 *)
let piArea n = 
   let mutable count = 0;
   let m = int (sqrt (float n))
   let m2 = (float m) ** 2.0
   for i in 1 .. m do
     for j in 1 .. m do
       if (float i) ** 2.0+(float j) ** 2.0 <= m2 then
         count <- count + 1;
   4.0 * (float count) / ((float m) * (float m))

let rec piGregoryLeibniz = function
   | 0 -> 4.0
   | n ->
     let term = 4.0 / (2.0 * (float n) + 1.0)
     let sgn = pown -1.0 (n % 2)
     sgn * term + piGregoryLeibniz (n-1)
       
let rec piNilakantha = function
   | 0 -> 3.0
   | n ->
     let m = 2.0 * (float n)
     let term = 4.0 / (m * (m + 1.0) * (m + 2.0))
     let sgn = -pown -1.0 (n % 2)
     sgn * term + piNilakantha (n-1)

let rec piBaileyBorweinPlouffe = function
   | -1 -> 0.0
   | k ->
     let m = 8.0 * (float k)
     (4.0 / (m + 1.0) - 2.0 / (m + 4.0) - 1.0 / (m + 5.0) - 1.0 / (m + 6.0)) / (pown 16.0 k) + piBaileyBorweinPlouffe (k-1)

let pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286;
printfn "pi = %.18f..." pi
printfn "piArea"
for i in 0..10000 do
    printf "%d: %.18f\r" i (piArea i)
printfn "";

printfn "piGregoryLeibniz"
for i in 0..10000 do
    printf "%d: %.18f\r" i (piGregoryLeibniz i)
printfn "";

printfn "piNilakantha"
for i in 0..10000 do
    printf "%d: %.18f\r" i (piNilakantha i)
printfn "";

printfn "piBaileyBorweinPlouffe"
for i in 0..10000 do
    printf "%d: %.18f\r" i (piBaileyBorweinPlouffe i)
printfn "";
