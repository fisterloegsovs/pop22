
// Maximum segment sum using lists

let rec mbss =    // maximum beginning segment sum
  function [] -> 0
         | x::xs -> max x (mbss xs + x)

let rec mss (a:int list) = // list version of maximum segment sum
  match a with
    | [] -> 0
    | x::xs -> max (mbss a) (mss xs)

let ex = [-2; 1; -3; 4; -1; 2; 1; -5; 4]

do printfn "%A" (mss ex)
