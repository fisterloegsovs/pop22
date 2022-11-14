// Catenable lists
module CatList

type 'a catlist = Empty | Single of 'a | Append of 'a catlist * 'a catlist

// Constructors
let nil = Empty
let single = Single
let append l1 l2 = Append (l1, l2)
let cons x xs = append (single x) xs
let snoc xs x = append xs (single x)

let rec length' xs =
    match xs with
      Empty -> 0
    | Single _ -> 1
    | Append (ys, zs) -> length' ys + length' zs

// Recursion on cat-lists
let recurse g e s  =
    let rec f xs =
        match xs with 
          Empty -> e
        | Single x -> s x
        | Append (ys, zs) -> g ys zs f 
    f  

// Structural recursion on lists: folding right
let fold (cf, e) = recurse (fun ys zs f -> cf (f ys) (f zs)) e

let map s = fold (append, nil) (single << s)
let filter p = fold (append, nil) (fun x -> if p x then single x else nil)
let reduce (cf, e) = fold (cf, e) id

// Conversion to/from cons-lists via difference lists
let fromCatList xs = DiffList.fromDiffList (fold (DiffList.append, DiffList.nil) DiffList.single xs)
let toCatList xs = List.foldBack cons xs nil 

let flip f x y = f y x
let rev xs = fold (flip append, nil) single xs

let length xs = fold ((+), 0) (fun _ -> 1) xs
let all p = fold ((fun b1 b2 -> b1 && b2), true) p // no short-cutting
let exists p = fold ((fun b1 b2 -> b1 || b2), false) p  // no short-cutting

let item' xs = recurse (fun ys zs f n -> if n < length ys then f ys n else f zs (n - length ys)) 
                       (fun _ -> None) (fun x n -> if n = 0 then Some x else None) 
                       xs
let item i = flip item' i