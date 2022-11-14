// Self-balanced ropes via length-augmented data structure
module Rope 

type 'a rope = 'a ropeBody * int 
and  'a ropeBody = Empty | Single of 'a | Append of 'a rope * 'a rope

// Smart constructors
let length (_, n) = n // O(1) time!
let nil = (Empty, 0)
let single x = (Single x, 1)
let append xs ys = (Append (xs, ys), length xs + length ys)
let cons x = append (single x)
let snoc xs x = append xs (single x)

// Recursion on ropes
let recurse g e s  =
    let rec f (xs, l) =
        match xs with 
          Empty -> e
        | Single x -> s x
        | Append (ys, zs) -> g ys zs f 
    f  

// Structural recursion on lists: folding right
let fold cf = recurse (fun ys zs f -> cf (f ys) (f zs))

let map s = fold append nil (single << s)
let filter p = fold append nil (fun x -> if p x then single x else nil)
let reduce cf e = fold cf e id

let height t = fold (fun h1 h2 -> 1 + max h1 h2) 0 (fun _ -> 1) t

// Conversion to/from cons-lists via difference lists
let fromRope xs = DiffList.fromDiffList (fold DiffList.append DiffList.nil DiffList.single xs)
// Unbalanced rope in O(n) time
// let toRope' xs = List.foldBack cons xs nil
// Balanced rope in O(n) time

// precondition: 0 < n <= length xs
// postcondition: fromRope t @ ys = xs if (t, ys) = toRope' xs
let rec toRope' xs n = 
        if n = 1 then (single (List.head xs), List.tail xs)
        else (* n > 1 *) let l = n / 2 
                         let r = n - l
                         let (tl, ys) = toRope' xs l
                         let (tr, zs) = toRope' ys r
                         (append tl tr, zs)
let toRope xs = let n = List.length xs
                if n = 0 then nil
                else (* n > 0 *) fst (toRope' xs n)
          
let flip f x y = f y x
let rev xs = fold (flip append) nil single xs

let all p = fold (fun b1 b2 -> b1 && b2) true p // no short-cutting
let exists p = fold (fun b1 b2 -> b1 || b2) false p  // no short-cutting

// item': Executes in time O(height(xs)) because length ys executes in time O(1)
let item' xs = recurse (fun ys zs f n -> if n < length ys then f ys n else f zs (n - length ys)) 
                       (fun _ -> None) (fun x n -> if n = 0 then Some x else None) 
                       xs
let item i = flip item' i 

let insert' xs = recurse (fun ys zs f i v -> if i < length ys then append (f ys i v) zs else append ys (f zs (i - length ys) v))
                      (fun i v -> single v)
                      (fun x i v -> if i = 0 then append (single v) (single x) else append (single x) (single v)) xs
// let insert i v xs = insert' xs i v

let delete' xs = recurse (fun ys zs f i -> if i < length ys then append (f ys i) zs else append ys (f zs (i - length ys)))
                      (fun i -> nil)
                      (fun x i -> nil) xs
let delete i = flip delete' i

let rotateL xs = recurse (fun ys zs _ -> 
                              recurse (fun zs1 zs2 _ -> if length ys < length zs1 || length ys < length zs2
                                                        then append (append ys zs1) zs2
                                                        else append ys zs)
                                      ys
                                      (snoc ys) zs)
                         nil
                         single xs

// precondition: 0 <= i <= length t
// with a bit of balancing
let rec insert i x (t, _) = 
    match t with
       Empty -> single x 
     | Single x' -> if i = 0 then append (single x) (single x') else append (single x') (single x)
     | Append (t1, t2) -> 
          let n1 = length t1
          let n2 = length t2
          match (i < n1), (n1 < n2) with
              (true, true) -> append (insert i x t1) t2 // insert into smaller subtree
            | (true, false) -> insertLeft i x t1 t2 // rotate right
            | (false, true) -> insertRight (i - n1) x t1 t2 // rotate left
            | (false, false) -> append t1 (insert (i - n1) x t2) // insert into smaller subtree
and insertLeft i x (t1, _) t2 =
    match t1 with
       Empty -> append (single x) t2
     | Single x' -> if i = 0 then append (append (single x) (single x')) t2 else append (append (single x') (single x)) t2
     | Append (t11, t12) -> if i < length t11 
                            then append (insert i x t11) (append t12 t2)
                            else insertBetween (i - length t11) x t11 t12 t2
and insertRight i x t1 (t2, _) =
    match t2 with
       Empty -> append t1 (single x)
     | Single x' -> if i = 0 then append t1 (append (single x) (single x')) else append t1 (append (single x') (single x))
     | Append (t21, t22) -> if i < length t21
                            then insertBetween i x t1 t21 t22
                            else append (append t1 t21) (insert (i - length t21) x t22)
and insertBetween i x tl t tr =
    match fst (insert i x t) with
       Empty -> append tl tr // impossible!
     | Single x' -> append tl (append (single x') tr)
     | Append (t1, t2) -> append (append tl t1) (append t2 tr)

(* let rec toRopeBal xs =
    match xs with
        [] -> nil
      | [x] -> single x
      | _ -> let (lefts, rights) = ListRec.splitAt (ListRec.length xs / 2) xs
             append (toRopeBal lefts) (toRopeBal rights) *)