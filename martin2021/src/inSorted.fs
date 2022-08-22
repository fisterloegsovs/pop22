let inSorted (arr:int[]) (x:int) : bool =
  let rec bs min max =
       if min > max then false
       else let mid = (max+min) / 2
            in if x < arr.[mid] then bs min (mid-1)
               else if x > arr.[mid] then bs (mid+1) max
               else true
  in bs 0 (Array.length arr - 1)

let arr = [|23;34;41;56;76;123;323;500|]   // sorteret array
do printf "Test 1: %A\n" (inSorted arr 76 = true)
do printf "Test 2: %A\n" (inSorted arr 37 = false)


// false branch: min <= max => mid >= min && mid <= max => mid-1 < max && mid+1 > min =>
// intervallet [min;max] er stærkt mindre for hvert rekursivt kald.
