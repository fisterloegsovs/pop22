
// Problematic string concatenation
let rec loop i =
  if i < 1 then "" else loop (i-1) + i.ToString()

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


// Tests

let len = String.length(toString (csloop 50000))

do printfn "Size of string with Numbers from 1 to 50000: %d" len
