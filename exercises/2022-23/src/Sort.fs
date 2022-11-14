module Sort 
// generic comparison-based functional sorting with cons-lists
open ConsList

let isort lte = 
    // inserts y into lte-sorted list
    let insert y = primrec (fun x xs f -> if lte y x then cons y xs else cons x (f xs)) (single y)
    fold insert nil     

let rec ssort lte =
      // precondition: xs = [x1, ..., xm] is nonempty, that is m > 0
      // postcondition: (xmin, [y1,...,yn]) = splitMin [x1,...,xm] where
      //        lte xmin xi = true for all 0 < i <= k and 
      //        [xmin, y1, ..., yn] is permutation of [x1,...,xm]
      let rec splitMin xs = 
          match xs with // incomplete pattern ok due to precondition
               [x] -> (x, []) 
             | x :: xs' -> let (xmin, ys) = splitMin xs'
                           if lte x xmin then (x, xmin :: ys) else (xmin, x :: ys)
      fun xs -> match xs with
                [] -> []
              | x :: _ -> let (xmin, ys) = splitMin xs
                          xmin :: ssort lte ys

let rec qsort lte =
    // ([l1, ..., lm], [h1, ..., hn]) = splitBy p xs such that
    // lte li p = true for all 0 < i <= m, lte hj p = false for all 0 < j <= n, and
    // [l1, ..., lm, h1, ..., hn] is a permutation of xs
    let splitBy p = fold (fun x (lower, higher) -> 
                             if lte x p then (x :: lower, higher) else (lower, x :: higher)) 
                            ([], [])
    fun xs -> 
        match xs with
          [] -> []
        | x :: xs' -> let (lower, higher) = splitBy x xs'
                      qsort lte lower @ [x] @ qsort lte higher

let rec splitAt k xs = 
        match xs with
            _ when k = 0 -> ([], xs)
          | [] -> ([], []) 
          | x :: xs' -> let (lefts, rights) = splitAt (k-1) xs'
                        (x :: lefts, rights)

let rec msort lte = 
    // precondition: xs and ys are sorted (according to lte)
    // postcondition: zs = merge xs ys, zs is sorted, and zs is permutation of xs @ ys
    let rec merge xs ys = 
          match (xs, ys) with
             ([], ys) -> ys // parentheses for tuples added for clarity
           | (xs, []) -> xs 
           | (x :: xs', y :: ys') -> 
               if lte x y then x :: merge xs' ys else y :: merge xs ys'
    fun xs -> match xs with
                [] -> []
              | [x] -> [x]
              | _ ->  let (lefts, rights) = splitAt (length xs / 2) xs
                      merge (msort lte lefts) (msort lte rights)
