let bsearch (arr:int[]) x =
  let rec bs min max =
       if min > max then None
       else let mid = (max+min) / 2
            in if x < arr.[mid] then bs min (mid-1)
               else if x > arr.[mid] then bs (mid+1) max
               else Some mid
  in bs 0 (Array.length arr - 1)

let arr = [|23;34;41;56;76;123;323|]

do printf "%A\n" (bsearch arr 76)
