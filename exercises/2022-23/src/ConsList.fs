// Cons-lists: built-in lists in F#
module ConsList 

type 'a conslist = 'a list 

// Constructors
let nil = []
let cons x y = x :: y
let single x = cons x nil

// Recursion on lists
let recurse g e =
    let rec f xs =
        match xs with 
          [] -> e
        | x :: xs' -> g x xs' f 
    f  

// Structural recursion on lists: folding right
let fold' cf = recurse (fun x xs' f -> cf x (f xs'))
// Without recurse:
let fold cf e =
    let rec f xs =
        match xs with
            [] -> e
          | x :: xs' -> cf x (f xs')
    f
let foldr = fold // common synonym for listFold

// folding left, tail recursively
let foldl cf =
    let rec f e xs =
        match xs with
            [] -> e
          | x :: xs' -> f (cf x e) xs' 
    f 

let mapl f = foldl (fun x y -> cons (f x) y) nil
let map f = fold (fun x y -> cons (f x) y) nil
let filterl p = foldl (fun x y -> if p x then cons x y else y) nil
let filter p = fold (fun x y -> if p x then cons x y else y) nil

let rev xs = foldl cons nil xs
let length xs = foldl (fun x v -> 1 + v) 0 xs
let all p = foldl (fun x b -> p x && b) true
let exists p = foldl (fun x b -> p x || b) false

let append' ys = fold (fun x zs -> cons x zs) ys
let append xs ys = append' ys xs 
let snoc xs y = append xs (single y)

// precondition: i >= 0
let item' xs = recurse (fun x xs f n -> if n = 0 then Some x else f xs (n-1)) (fun _ -> None) xs
let item i xs = item' xs i