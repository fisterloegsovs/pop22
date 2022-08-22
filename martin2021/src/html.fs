module Html

// Trees
type 'a tree = Leaf of 'a | Tree of 'a tree * 'a tree

// Catenable strings
let (++) x y = Tree (x,y)    // infix operator definition
let S s = Leaf s             // simple leaf construktor
let rec csloop i : string tree =
  if i < 1 then S"" else csloop (i-1) ++ S(i.ToString())

let rec flatten (acc:'a list) (t:'a tree) : 'a list =
  match t with
  | Leaf s -> s :: acc
  | Tree (x,y) -> flatten (flatten acc y) x

let toString (x:string tree) : string =
  String.concat "" (flatten [] x)

type html = string tree
let tag t e = S("<"+t+">") ++ e ++ S("</"+t+">")

// Use of library:  toString(tag "h2" (S"Nice world"));;

// let items (xs:string list) : html =
//   tag "ul"
//     (List.fold (fun a x -> a ++ tag "li" (S(string x)))
//                (S "") xs)

// let rec fib n =
//   if n <= 2 then 1
//   else fib (n-1) + fib (n-2)

// let doc =
//   let fibs = List.map (fun i -> string(fib i)) [1..10]
//   in tag "html" (tag "body" (tag "h2" (S"Fibs") ++
//                              items fibs));;

// printing

// toString doc;;
