(* http://www.wikihow.com/Calculate-Pi
 * http://math.stackexchange.com/questions/14815/why-does-this-converge-to-pi-4
 * https://en.wikipedia.org/wiki/Approximations_of_π
 *)

open System.Threading

let piArea n = 
   let mutable count = 0;
   let n2 = (float n) ** 2.0
   for i in 1 .. n do
     for j in 1 .. n do
       if (float i) ** 2.0+(float j) ** 2.0 <= n2 then
         count <- count + 1;
   4.0 * (float count) / n2

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

(* Won't work since this is a value and only executed once - at time of declaration
let pressEnter =
  printf "Press <enter>";
  System.Console.ReadLine();
*)
   
let pressEnter () =
   printf "Press <enter>";
   System.Console.ReadLine();

let pi = 3.141592653589793238462643383279502884197169399375105820974944592307816406286;
printfn "pi = %.18f..." pi

pressEnter () |> ignore;
printfn "piArea"
for i in 1..1000 do
    printf "%d: %.18f\r" (i*i) (piArea i)
printfn "";

pressEnter () |> ignore;
printfn "piGregoryLeibniz"
for i in 0..10000 do
    printf "%d: %.18f\r" i (piGregoryLeibniz i)
    Thread.Sleep 1
printfn "";

pressEnter () |> ignore;
printfn "piNilakantha"
for i in 0..10000 do
    printf "%d: %.18f\r" i (piNilakantha i)
    Thread.Sleep 10
printfn "";

pressEnter () |> ignore;
printfn "piBaileyBorweinPlouffe"
for i in 0..10 do
    printf "%d: %.18f\r" i (piBaileyBorweinPlouffe i)
    Thread.Sleep 1000
printfn "";
