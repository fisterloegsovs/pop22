module Collection
/// A collection of functions
///
/// How to compile:
/// <code>
/// fsharpc --doc:Collection.xml -a Collection.fsi Collection.fs
/// </code>
///
/// Author: Jon Sporring.
/// Date: 2015/11/11

let rec fact = function
  | 1 -> 1 // Wrong stopping condition: should be 0->1
  | n when n > 1 -> n*(fact (n-1))
  | _ -> failwith "fact is undefined for negative values"

let rec polynomial a x =
  match a with
  | first :: rest -> first + x*(polynomial rest x)
  | _ -> 0.0

let pairToString = function
  | (a,b) -> sprintf "(%f, %f)" a b

let listToString lst = "[" + (List.foldBack (fun first rest -> (string first) + ", " + rest) lst "") + "]";;
  
