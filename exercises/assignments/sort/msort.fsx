///<summary>Merge two lists. The elements must be comparable, i.e., if x and y are elements, then y < x must be defined.</summary>
///<param name = "xs">A sorted list with the smallest value at the Head.</param>
///<param name = "ys">A sorted list with the smallest value at the Head.</param>
///<returns>A with all the elements in xs and ys and in sorted order with the smallest value at the Head.</returns>
let rec merge xs ys =
  if List.isEmpty xs then ys
  else if List.isEmpty ys then xs
  else let x = List.head xs
       let y = List.head ys
       let xs = List.tail xs
       let ys = List.tail ys
       in if x < y then x :: merge xs (y::ys)
          else y :: merge (x::xs) ys

///<summary>Sort a lists. The elements must be comparable, i.e., if x and y are elements, then y < x must be defined.</summary>
///<param name = "xs">A unsorted list.</param>
///<returns>A sorted list with all elements from xs and with the smallest value at the Head.</returns>
let rec msort xs =
  let sz = List.length xs
  if sz < 2 then xs
  else let n = sz / 2
       let ys = xs.[0..n-1]
       let zs = xs.[n..sz-1]
       in merge (msort ys) (msort zs)

let t1 = [34;7;34;23;2;73;2;36;86;12;11;4;54;35]

do printf "%A\n" t1

do printf "%A\n" (msort t1)
